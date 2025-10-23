############################################################
# Fishpond DO ~ Temp + pH (with AR(1))
# Model: "fit_med_ar1"
# - Single response: oxygen
# - Predictors: temp_z, pH_z, temp_z:pH_z, diel (hour_sin/cos), seasonal smooth s(doy_s, k=6)
# - Random effects: (1 + temp_z || site)   # uncorrelated intercept & slope by site
# - Autocorrelation: AR(1) within site using numeric time (hours)
# Data expectations (long format CSV):
#   columns: date_time_hst, site, site_specific, variable ? {oxygen,pH,temperature}, value
#
# Notes:
# - We aggregate to a regular 10-minute grid per site.
# - Temperature is averaged across subsites to a site-level value.
# - Oxygen & pH are taken from the "General" subsite (your coverage pattern).
# - We then keep only COMPLETE CASES for pH and temperature to keep the model light.
############################################################

# ------------------------- 1) Packages -------------------------
library(brms)
library(dplyr)

# Prefer cmdstanr backend if available (faster compile/run). Works fine without it.
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  options(brms.backend = "cmdstanr")
  # use all available CPU cores for chains by default
  if (is.null(getOption("mc.cores"))) options(mc.cores = parallel::detectCores())
}

# ------------------------- 2) Small helper -------------------------
# Bucket timestamps to a fixed cadence (default: 10 minutes).
bucket_time <- function(x, mins = 10) {
  x <- as.POSIXct(x, tz = "Pacific/Honolulu")
  base <- as.numeric(x)
  step <- mins * 60
  as.POSIXct(floor(base / step) * step, origin = "1970-01-01", tz = "Pacific/Honolulu")
}

# ------------------------- 3) Load & minimal wrangling -------------------------
csv_path <- "master_data_pivot.csv"  # <-- update the path if needed
raw <- read.csv(csv_path, stringsAsFactors = FALSE)

# Ensure time is POSIXct in HST (adjust tz if needed)
if (!inherits(raw$date_time_hst, "POSIXct")) {
  raw$date_time_hst <- as.POSIXct(raw$date_time_hst, tz = "Pacific/Honolulu")
}

# Keep only variables we need; create a 10-min bucket for alignment
dat <- raw %>%
  filter(variable %in% c("oxygen","pH","temperature")) %>%
  mutate(t10 = bucket_time(date_time_hst, 10L))

# ------------------------- 4) Build site-time panels -------------------------
# 4a) Site-level temperature (mean across subsites at each site × t10)
temp_site10 <- dat %>%
  filter(variable == "temperature") %>%
  group_by(site, t10) %>%
  summarise(temperature = mean(value, na.rm = TRUE), .groups = "drop")

# 4b) Oxygen & pH from "General" subsite (per site × t10)
oxy_gen10 <- dat %>%
  filter(variable == "oxygen", site_specific == "General") %>%
  group_by(site, t10) %>%
  summarise(oxygen = mean(value, na.rm = TRUE), .groups = "drop")

ph_gen10 <- dat %>%
  filter(variable == "pH", site_specific == "General") %>%
  group_by(site, t10) %>%
  summarise(pH = mean(value, na.rm = TRUE), .groups = "drop")

# 4c) Join into a wide table
#     - require temperature at that site-time (inner join)
#     - require oxygen at that site-time (inner join via oxy_gen10 start)
#     - pH must be present for this simplified model (inner join below keeps complete cases)
wide_all <- oxy_gen10 %>%
  inner_join(ph_gen10,   by = c("site","t10")) %>%
  inner_join(temp_site10, by = c("site","t10")) %>%
  transmute(
    site,
    time_stamp = t10,     # regularized 10-min time
    oxygen,
    pH,
    temperature
  )

stopifnot(nrow(wide_all) > 0)

# ------------------------- 5) Feature engineering -------------------------
# Diel features and seasonal index
wide_all <- wide_all %>%
  mutate(
    hour     = as.integer(format(time_stamp, "%H")),
    doy      = as.integer(format(time_stamp, "%j")),
    hour_sin = sin(2 * pi * hour / 24),
    hour_cos = cos(2 * pi * hour / 24)
  )

# Standardize predictors (z-scores). Keep as plain numerics.
wide_all <- wide_all %>%
  mutate(
    temp_z = as.numeric(scale(temperature)),
    pH_z   = as.numeric(scale(pH)),
    doy_s  = as.numeric(scale(doy))
  )

# Numeric time for AR(1): hours since epoch (any monotone numeric works)
wide_all$time_num <- as.numeric(wide_all$time_stamp) / 3600

# ------------------------- 6) Final modeling frame -------------------------
# For this lightweight model we keep complete cases (oxygen, temp_z, pH_z present).
dat_med <- wide_all %>%
  filter(!is.na(temp_z), !is.na(pH_z), !is.na(oxygen))

stopifnot(nrow(dat_med) > 0)

# ------------------------- 7) Priors (simple, weakly-informative) -------------------------
priors_med <- c(
  prior(student_t(3, 0, 2), class = "Intercept"),          # baseline oxygen
  prior(normal(0, 1),        class = "b"),                 # fixed effects (on z-scales)
  prior(student_t(3, 0, 1),  class = "sd", group = "site"),# RE SDs for site
  prior(exponential(1),      class = "sigma")              # residual scale
)

# ------------------------- 8) Fit the AR(1) model -------------------------
# Model formula:
# oxygen ~ 1 + temp_z + pH_z + temp_z:pH_z + diel (hour terms) + seasonal smooth
#           + (1 + temp_z || site)    # uncorrelated RE intercept and temp slope by site
# AR(1) is specified with time_num within site.
fit_med_ar1 <- brm(
  oxygen ~ 1 +
    temp_z + pH_z + temp_z:pH_z +
    hour_sin + hour_cos +
    s(doy_s, k = 6) +
    (1 + temp_z || site),
  data    = dat_med,
  family  = student(),
  prior   = priors_med,
  autocor = cor_ar(~ time_num | site, p = 1),
  chains  = 2, iter = 1500, warmup = 750,                 # quick-but-reasonable defaults
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  # Optional caching if using cmdstanr (speeds re-runs):
  # save_pars = save_pars(all = TRUE), file = "fit_med_ar1_cache", file_refit = "never"
)

# ------------------------- 9) Basic outputs -------------------------
print(fit_med_ar1)
bayes_R2(fit_med_ar1)
pp_check(fit_med_ar1, ndraws = 100)

# (Optional) one-liner to extract AR(1) estimate and the interaction:
# fixef(fit_med_ar1)["temp_z:pH_z", ]   # interaction
# ar1 <- as.data.frame(VarCorr(fit_med_ar1)$ar)  # AR(1) summary (version-dependent)

############################################################
# What this script does NOT include (by design):
# - Multivariate modeling or missing-data imputation (mi): we keep complete pH cases only.
# - Rolling/tolerant joins: we require exact site×10-min matches (simple & reproducible).
# - Heavy random-effect correlation: we use '||' to drop RE correlations (fewer params).
#
# If you want to use more rows:
# - Increase overlap by using a tolerance (±30?60 min) for temperature joins, or
# - Interpolate site-level temperature/pH before modeling, or
# - Switch to the multivariate + mi() approach (heavier, but preserves more data).
############################################################
