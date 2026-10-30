## ============================================================
##  BAYESIAN MODEL CHALLENGE  (final-class individual challenge)
##
##  10 questions. Students read a situation and the sentence they want to
##  be able to say AFTER modeling, choose the model, then run it and
##  interpret the output. Question types: choose-the-model, live model runs,
##  numeric answers, multiple choice, match-the-statement, true/false.
##
##  Packages : shiny, bslib, tidyverse, rstanarm, e1071
##             (bayesrules is used for the data if installed; otherwise the
##              .rda files are read from a ./data folder next to this file)
##  Run      : shiny::runApp()
##  Scoring  : one attempt per item. Students download a CSV report
##             (name, answers, score) on the Results tab to turn in.
##  Notes    : Q7 uses a SIMULATED playlist column (documented in-app).
##             Q10 uses output from the class BART analysis of synthetic data.
## ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(rstanarm)
library(e1071)

theme_set(theme_minimal(base_size = 13))

## ------------------------------------------------------------
## 1. Data
## ------------------------------------------------------------
get_data <- function(name) {
  if (requireNamespace("bayesrules", quietly = TRUE)) {
    return(getExportedValue("bayesrules", name))
  }
  f <- file.path("data", paste0(name, ".rda"))
  if (file.exists(f)) { e <- new.env(); load(f, e); return(get(name, e)) }
  stop("Install the bayesrules package (install.packages('bayesrules')) or add data/", name, ".rda")
}

# fake news (Bayes' theorem)
fake_news <- get_data("fake_news")
P_FAKE <- round(mean(fake_news$type == "fake"), 3)
P_EXCL_FAKE <- round(mean(fake_news$title_has_excl[fake_news$type == "fake"]), 3)
P_EXCL_REAL <- round(mean(fake_news$title_has_excl[fake_news$type == "real"]), 3)

# Bechdel 1991 (beta-binomial)
b91 <- get_data("bechdel") %>% filter(year == 1991)
Y91 <- sum(b91$binary == "PASS"); N91 <- nrow(b91)

# bikes (linear)
bikes <- get_data("bikes")

# Perth weather (logistic)
weather <- get_data("weather_perth") %>%
  transmute(rain = as.integer(raintomorrow == "Yes"), humidity9am)

# penguins (naive Bayes)
penguins <- get_data("penguins_bayes") %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>% na.omit()

# cherry blossom race (hierarchical)
running <- get_data("cherry_blossom_sample") %>% select(runner, age, net) %>% na.omit()

# spotify + SIMULATED playlist counts (Simpson's paradox)
spotify <- get_data("spotify")
set.seed(84735)
genre_base <- c(edm = 20, rock = 16, rap = 14, pop = 10, latin = 9, `r&b` = 8)
spotify <- spotify %>%
  group_by(artist) %>%
  mutate(artist_mean_pop = mean(popularity),
         artist_genre = names(which.max(table(genre)))) %>%
  ungroup() %>%
  mutate(n_playlists = pmax(round(unname(genre_base[artist_genre]) +
                                    0.15 * (popularity - artist_mean_pop) +
                                    rnorm(n(), 0, 3.5)), 0)) %>%
  select(-artist_mean_pop)

## ------------------------------------------------------------
## 2. Models (fit on demand, cached so each is fit once per R process)
## ------------------------------------------------------------
.fits <- new.env()
get_fit <- function(name) {
  if (!is.null(.fits[[name]])) return(.fits[[name]])
  ctl <- list(chains = 2, iter = 2000, seed = 84735, refresh = 0)
  fit <- switch(name,
                bikes = stan_glm(rides ~ temp_feel, data = bikes, family = gaussian,
                                 prior_intercept = normal(5000, 1000), prior = normal(100, 40),
                                 prior_aux = exponential(0.0008),
                                 chains = 2, iter = 2000, seed = 84735, refresh = 0),
                weather = stan_glm(rain ~ humidity9am, data = weather, family = binomial,
                                   prior_intercept = normal(-1.4, 0.7), prior = normal(0.07, 0.035),
                                   chains = 2, iter = 2000, seed = 84735, refresh = 0),
                run_pool = stan_glm(net ~ age, data = running, family = gaussian,
                                    prior_intercept = normal(100, 10), prior = normal(2.5, 1),
                                    prior_aux = exponential(1, autoscale = TRUE),
                                    chains = 2, iter = 2000, seed = 84735, refresh = 0),
                run_hier = stan_glmer(net ~ age + (1 | runner), data = running, family = gaussian,
                                      prior_intercept = normal(100, 10), prior = normal(2.5, 1),
                                      prior_aux = exponential(1, autoscale = TRUE),
                                      prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
                                      chains = 2, iter = 2000, seed = 84735, refresh = 0),
                play_pool = stan_glm(popularity ~ n_playlists, data = spotify, family = gaussian,
                                     prior_intercept = normal(50, 2.5, autoscale = TRUE),
                                     prior = normal(0, 2.5, autoscale = TRUE),
                                     prior_aux = exponential(1, autoscale = TRUE),
                                     chains = 2, iter = 2000, seed = 84735, refresh = 0),
                play_hier = stan_glmer(popularity ~ n_playlists + (1 | artist), data = spotify, family = gaussian,
                                       prior_intercept = normal(50, 2.5, autoscale = TRUE),
                                       prior = normal(0, 2.5, autoscale = TRUE),
                                       prior_aux = exponential(1, autoscale = TRUE),
                                       prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
                                       chains = 2, iter = 2000, seed = 84735, refresh = 0),
                stop("unknown model ", name))
  .fits[[name]] <- fit
  fit
}

run_nb <- function(preds) {
  nb <- naiveBayes(species ~ ., data = penguins[, c("species", preds), drop = FALSE])
  pred <- predict(nb, penguins)
  list(acc = mean(pred == penguins$species),
       tab = table(Predicted = pred, Actual = penguins$species))
}

## ------------------------------------------------------------
## 3. Small helpers
## ------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
shuffle <- function(x) x[sample.int(length(x))]
ci95 <- function(x) c(mean = mean(x), lo = unname(quantile(x, .025)), hi = unname(quantile(x, .975)))
sum_row <- function(label, x) tibble(Quantity = label, Mean = fmt(mean(x), 3),
                                     `2.5%` = fmt(quantile(x, .025), 3), `97.5%` = fmt(quantile(x, .975), 3))

MODELS <- c(bayes = "Bayes' theorem",
            betabin = "Beta-binomial model",
            linear = "Bayesian linear regression",
            logistic = "Bayesian logistic regression",
            nb = "Naive Bayes classifier",
            hier = "Hierarchical (multilevel) model",
            bart = "BART (Bayesian additive regression trees)")

TF <- c(true = "True", false = "False")

## ------------------------------------------------------------
## 4. Item bank (every graded thing lives here)
## ------------------------------------------------------------
mc  <- function(id, q, stage, label, prompt, options, answer, explain, ordered = FALSE)
  list(id = id, q = q, stage = stage, type = "mc", label = label, prompt = prompt,
       options = options, answer = answer, explain = explain, ordered = ordered)
num <- function(id, q, stage, label, prompt, answer, tol, explain)
  list(id = id, q = q, stage = stage, type = "num", label = label, prompt = prompt,
       answer = answer, tol = tol, explain = explain)
model_item <- function(id, q, answer, explain)
  mc(id, q, "model", "Choose the model",
     "Which model would you use to be able to say the outcome sentence above?",
     MODELS, answer, explain)

items <- list(
  ## ---- Q1: logistic (weather) ----
  model_item("q1m", 1, "logistic",
             "The outcome is yes/no (rain tomorrow), the predictor is continuous, and the sentence is about odds: Bayesian logistic regression."),
  num("q1a", 1, "work", "Odds multiplier for 15 points",
      "Run the model. The slope is on the log-odds scale. Compute exp(15 × slope) and enter the multiplier on the odds for a 15-point rise in 9am humidity.",
      function() median(exp(15 * as.data.frame(get_fit("weather"))$humidity9am)), 0.15,
      "The odds of rain tomorrow are multiplied by exp(15β), about 2.1 (95% interval roughly 1.8 to 2.4) for every 15-point rise in humidity."),
  mc("q1b", 1, "work", "Odds vs probability",
     "Which statement about the result is correct?",
     c(a = "A 15-point rise in humidity roughly doubles the odds of rain tomorrow, which is not the same as doubling the probability.",
       b = "A 15-point rise in humidity roughly doubles the probability of rain tomorrow, whatever the starting humidity was.",
       c = "A 15-point rise in humidity adds about 2 percentage points to the chance of rain tomorrow on any given day.",
       d = "Humidity causes rain tomorrow, and its effect is about twice as strong as that of any other weather variable."),
     "a", "A multiplier on the odds is not a multiplier on the probability. The change in probability depends on where you start."),

  ## ---- Q2: Bayes' theorem (fake news) ----
  model_item("q2m", 2, "bayes",
             "You know the base rate and how often fake and real titles use '!', and you want to flip those into P(fake | '!'): Bayes' theorem."),
  num("q2a", 2, "work", "Posterior with a 10% base rate",
      "In the workbench set the prior P(fake) to 0.10 and leave the other two sliders at their starting values. What is P(fake | '!')? (Enter a proportion.)",
      function() 0.10 * P_EXCL_FAKE / (0.10 * P_EXCL_FAKE + 0.90 * P_EXCL_REAL), 0.03,
      "Bayes' theorem: 0.10×0.267 / (0.10×0.267 + 0.90×0.022) is about 0.57. A lower base rate of fake news lowers the posterior even though the evidence is the same."),
  mc("q2b", 2, "work", "Meaning of P(! | fake)",
     "What does P(! | fake) = 0.267 mean?",
     c(a = "About 27% of fake articles have an exclamation point in the title.",
       b = "About 27% of articles with an exclamation point are fake.",
       c = "About 27% of all articles are fake.",
       d = "An article has a 27% chance of being fake."),
     "a", "P(! | fake) conditions on the article being fake. The reverse, P(fake | !), is what Bayes' theorem produces."),

  ## ---- Q3: hierarchical (running) ----
  model_item("q3m", 3, "hier",
             "Repeated races by the same runner are not independent, and each runner needs their own baseline: a hierarchical (multilevel) model."),
  mc("q3a", 3, "work", "Why the slopes disagree",
     "The pooled and hierarchical models give different age slopes. Which explanation is best?",
     c(a = "The pooled model mixes differences between runners with changes within a runner. The hierarchical model compares each runner with themselves.",
       b = "The hierarchical model has more parameters, and models with more parameters always produce larger slopes than simpler ones.",
       c = "The pooled model gives a smaller slope because it ignores the prior, while the hierarchical model uses the prior information.",
       d = "The two models were fit to different subsets of the races, so their slopes cannot be compared directly."),
     "a", "Pooling treats all races as independent. With a random intercept for each runner, the age slope is estimated from changes within runners."),
  mc("q3b", 3, "work", "Between vs within runners",
     "Compare the between-runner SD with the residual (race-to-race) SD in the table. What do they say?",
     c(a = "Runners differ from each other in baseline speed much more than one runner varies from year to year.",
       b = "Race-to-race noise for a single runner is much larger than the typical difference in speed between two runners.",
       c = "The two standard deviations are about equal, which means runners do not really differ in baseline speed.",
       d = "Neither standard deviation says anything about runners, because both describe the whole race rather than individuals."),
     "a", "The between-runner SD (about 13 minutes) is much larger than the residual SD (about 5 minutes). Most of the variation is between runners."),

  ## ---- Q4: beta-binomial (Bechdel) ----
  model_item("q4m", 4, "betabin",
             "You have a count of successes out of a number of trials and a prior belief about the proportion: a beta-binomial model."),
  num("q4a", 4, "work", "Posterior mean with Beta(5, 11)",
      "Set the prior to Beta(5, 11). What is the posterior mean pass rate? (Enter a proportion.)",
      function() (5 + Y91) / (16 + N91), 0.01,
      "The posterior is Beta(5 + y, 11 + n - y). Its mean is (5 + y) / (16 + n), about 0.38. It sits between the prior mean (0.31) and the data proportion (0.46)."),
  mc("q4b", 4, "work", "A flat prior",
     "Now set the prior to Beta(1, 1). The posterior mean ends up very close to the observed proportion. Why?",
     c(a = "A Beta(1, 1) prior is flat and carries only about 2 pseudo-observations, so the data dominate.",
       b = "A Beta(1, 1) prior means we are certain that every film passes, so the posterior is forced to match the data.",
       c = "A Beta(1, 1) prior is the same as a Beta(5, 11) prior, so the posterior mean should not change at all.",
       d = "When the prior is flat, the likelihood is ignored and the posterior is determined by the prior alone."),
     "a", "The prior acts like a + b observations. Beta(1, 1) adds 2, versus 13 real films, so the posterior is close to the data."),

  ## ---- Q5: linear (bikes) ----
  model_item("q5m", 5, "linear",
             "A continuous outcome (daily rides) and a sentence about how much it changes per unit of a predictor: Bayesian linear regression."),
  mc("q5a", 5, "work", "Interpret the slope",
     "Run the model. Which interpretation of the temp_feel slope is correct?",
     c(a = "On average, each additional degree of felt temperature is associated with about 82 more rides per day.",
       b = "Felt temperature explains about 82% of the day-to-day variation in the number of rides.",
       c = "The number of rides increases by about 82% for each additional degree of felt temperature.",
       d = "An increase of 82 rides on a given day raises the felt temperature by about one degree."),
     "a", "The slope is in units of the outcome per unit of the predictor: about 82 rides per day per degree."),
  mc("q5b", 5, "work", "Interpret sigma",
     "The model reports sigma of about 1,283. What does it mean?",
     c(a = "Even on days with the same temperature, daily rides typically differ from the line by about 1,300 rides.",
       b = "The slope is uncertain by about 1,283 rides, so the effect of temperature cannot be pinned down precisely.",
       c = "The model's prediction is off by exactly 1,283 rides on every single day in the data set.",
       d = "The predicted number of rides on a day with a felt temperature of zero degrees is 1,283."),
     "a", "Sigma is the residual standard deviation: the typical spread of rides around the regression line."),

  ## ---- Q6: naive Bayes (penguins) ----
  model_item("q6m", 6, "nb",
             "A categorical outcome (species) with several measured features, and you want class probabilities for a new case: a naive Bayes classifier."),
  num("q6a", 6, "work", "Accuracy with two predictors",
      "Use only bill length and flipper length. What overall accuracy do you get? (Enter a proportion.)",
      function() run_nb(c("bill_length_mm", "flipper_length_mm"))$acc, 0.02,
      "With bill length and flipper length the classifier gets about 95% of penguins right (in-sample)."),
  mc("q6b", 6, "work", "The 'naive' assumption",
     "What is the 'naive' assumption in naive Bayes?",
     c(a = "Within each species, the predictors are treated as independent of each other.",
       b = "All of the species are assumed to be equally common before any measurements are taken into account.",
       c = "The predictors are assumed to be unrelated to species, so they carry no information about the class.",
       d = "Only one numeric predictor can be used at a time, because combining predictors would double count information."),
     "a", "Naive Bayes multiplies the per-predictor likelihoods as if they were independent given the class. It often works well even when they are not, as with correlated penguin measurements."),

  ## ---- Q7: hierarchical / Simpson (playlists) ----
  model_item("q7m", 7, "hier",
             "Songs are grouped by artist and we want the effect within an artist, which the pooled model can't separate: a hierarchical model."),
  mc("q7a", 7, "work", "Respond to a colleague",
     "A colleague sees the pooled slope and says 'playlists don't matter for popularity.' Best response?",
     c(a = "The pooled model mixes patterns between genres and artists with the pattern within an artist. Comparing songs by the same artist, more playlists go with higher popularity.",
       b = "They are right, because the pooled model uses all of the data and so gives the most reliable overall answer.",
       c = "The hierarchical slope cannot be trusted, because a model with more parameters will always find some positive effect.",
       d = "Playlists matter, because the pooled model has a very small p-value for the playlist coefficient."),
     "a", "Group structure can hide a relationship. This is a Simpson's paradox: the between-group pattern cancels the within-group pattern in the pooled fit."),
  mc("q7b", 7, "work", "Causal claim",
     "True or false: because the hierarchical slope is positive, getting a song onto more playlists will raise its popularity.",
     TF, "false",
     "False. The playlist column is simulated from popularity, and in real data causation runs both ways (popular songs get added to playlists). The model shows an association.",
     ordered = TRUE),

  ## ---- Q8: match statements to models ----
  mc("q8a", 8, "main", "Statement 1",
     "A school nurse screens 25 students and finds 4 with a vision problem. She wants a probability statement about the district-wide rate.",
     MODELS, "betabin", "One proportion, a count of successes out of n, with a prior: beta-binomial.", ordered = TRUE),
  mc("q8b", 8, "main", "Statement 2",
     "A test is 95% sensitive and 90% specific. The disease affects 2% of people. A patient tests positive; how likely is it that they have the disease?",
     MODELS, "bayes", "Flipping P(positive | disease) into P(disease | positive) with a base rate: Bayes' theorem.", ordered = TRUE),
  mc("q8c", 8, "main", "Statement 3",
     "Predict a house's sale price from its square footage, with an interval for the price change per additional 100 square feet.",
     MODELS, "linear", "A continuous outcome and a slope: Bayesian linear regression.", ordered = TRUE),
  mc("q8d", 8, "main", "Statement 4",
     "Whether a shopper clicks an ad (yes/no) as a function of ad length and time of day, reported as odds ratios.",
     MODELS, "logistic", "A binary outcome with predictors and odds ratios: Bayesian logistic regression.", ordered = TRUE),
  mc("q8e", 8, "main", "Statement 5",
     "Classify an email as spam or not spam using the presence or absence of hundreds of words.",
     MODELS, "nb", "A class label with many simple features and an independence assumption: naive Bayes.", ordered = TRUE),
  mc("q8f", 8, "main", "Statement 6",
     "Test scores for students in 60 schools of very different sizes. Estimate each school's average without over-trusting tiny schools.",
     MODELS, "hier", "Many groups of uneven size and partial pooling: a hierarchical model.", ordered = TRUE),
  mc("q8g", 8, "main", "Statement 7",
     "The best housing service probably depends on combinations of household characteristics we can't list in advance. We want predictions for each household under each service.",
     MODELS, "bart", "Flexible, automatic discovery of interactions with counterfactual predictions per case: BART.", ordered = TRUE),

  ## ---- Q9: true / false ----
  mc("q9a", 9, "main", "p-value",
     "A p-value of 0.45 shows there is no relationship between the two variables.", TF, "false",
     "A large p-value means the data are consistent with no relationship. It is not evidence of none, and group structure can hide real relationships.", ordered = TRUE),
  mc("q9b", 9, "main", "Credible interval",
     "A 95% credible interval means there is a 95% posterior probability that the parameter lies in the interval, given the model and prior.", TF, "true",
     "That is the direct probability statement Bayesian intervals allow.", ordered = TRUE),
  mc("q9c", 9, "main", "Partial pooling",
     "Partial pooling pulls estimates for groups with little data toward the overall average.", TF, "true",
     "Shrinkage is strongest for small groups and noisy estimates.", ordered = TRUE),
  mc("q9d", 9, "main", "Naive Bayes",
     "Naive Bayes assumes the predictors are independent of each other once the class is known.", TF, "true",
     "It multiplies per-predictor likelihoods within each class.", ordered = TRUE),
  mc("q9e", 9, "main", "BART",
     "To use BART you must specify the interactions between predictors in advance.", TF, "false",
     "The tree structure finds interactions automatically. The priors regularize them, which is also why BART shrinks subgroup effects toward the average.", ordered = TRUE),
  mc("q9f", 9, "main", "Few groups",
     "A random intercept for species is a good choice when there are only 3 species.", TF, "false",
     "With very few groups there is little information to estimate how much groups vary. Treat the group as an ordinary predictor instead.", ordered = TRUE),

  ## ---- Q10: BART output ----
  mc("q10a", 10, "main", "Prevention vs TH",
     "Among Prevention-eligible households, Prevention minus TH has a mean of -3.9 percentage points, a 95% interval of -9.1 to +0.5, and a 96% posterior probability of being below zero. Which conclusion is best?",
     c(a = "Prevention probably lowers reentry relative to TH, but the interval just includes zero, so the evidence is suggestive rather than conclusive.",
       b = "Prevention definitely lowers reentry relative to TH, and the best single estimate of that reduction is exactly 3.9 points.",
       c = "There is no difference between the services, because any interval that includes zero proves the effect is zero.",
       d = "TH is the better service, because the interval includes some positive values where Prevention would do worse."),
     "a", "A 96% probability of a negative difference leans toward Prevention, but an interval that touches zero means we shouldn't be certain."),
  mc("q10b", 10, "main", "TH vs RRH",
     "Among ineligible households, TH minus RRH has a mean of +1.0 percentage point, a 95% interval of -5.1 to +7.1, and only a 38% probability of being below zero. Which conclusion is best?",
     c(a = "The data can't separate TH from RRH on average. The interval is wide and centered near zero, so cost or subgroup effects should drive the choice.",
       b = "RRH is clearly the better service on average, because the posterior mean of the difference is positive.",
       c = "TH is clearly the better service on average, because the posterior mean of the difference is so close to zero.",
       d = "The model has failed to fit, because an effect that is not statistically significant means the estimates are invalid."),
     "a", "A mean near zero with a wide interval says the average effects are about the same. The paper found differences only in subgroups (CATEs).")
)
names(items) <- vapply(items, `[[`, "", "id")
items_for <- function(q, stage = NULL) Filter(function(it) it$q == q && (is.null(stage) || it$stage %in% stage), items)

grade_item <- function(it, val) {
  if (it$type == "num") {
    ans <- if (is.function(it$answer)) it$answer() else it$answer
    list(correct = isTRUE(abs(val - ans) <= it$tol), given = as.character(val), key = fmt(ans, 3))
  } else {
    list(correct = identical(val, it$answer), given = unname(it$options[val]),
         key = unname(it$options[it$answer]))
  }
}
is_blank <- function(it, v) if (it$type == "num") is.null(v) || is.na(v) else is.null(v) || length(v) == 0 || !nzchar(v)

## ------------------------------------------------------------
## 5. UI pieces
## ------------------------------------------------------------
css <- "
 body { font-family: -apple-system, 'Segoe UI', Roboto, sans-serif; }
 .scenario { background:#F1F5F9; border-left:5px solid #2A6F97; padding:12px 16px; border-radius:6px; margin-bottom:14px; }
 .outcome  { background:#FFF7ED; border-left:5px solid #E07A5F; padding:10px 16px; border-radius:6px; margin-bottom:14px; }
 .item { margin: 10px 0 16px 0; padding: 10px 14px; border:1px solid #E2E8F0; border-radius:8px; }
 .fb { margin-top:6px; padding:8px 12px; border-radius:6px; font-size:14px; }
 .fb.ok  { background:#E8F5E9; border:1px solid #A5D6A7; }
 .fb.bad { background:#FDECEA; border:1px solid #F5B7B1; }
 .hint { color:#64748B; font-style:italic; padding:10px 0; }
 .workbench { background:#F8FAFC; border:1px solid #E2E8F0; padding:12px 16px; border-radius:8px; margin-bottom:12px; }
 .stat { font-size:20px; font-weight:700; }
"

item_ui <- function(it) {
  ctl <- if (it$type == "num") {
    numericInput(it$id, NULL, value = NA, step = 0.001, width = "180px")
  } else {
    opts <- if (isTRUE(it$ordered)) it$options else shuffle(it$options)
    radioButtons(it$id, NULL, choices = setNames(names(opts), unname(opts)), selected = character(0), width = "100%")
  }
  div(class = "item", p(strong(it$prompt)), ctl, uiOutput(paste0("fb_", it$id)))
}

work_block <- function(q, controls) {
  tagList(
    div(class = "workbench", controls),
    h4("Now answer"),
    lapply(items_for(q, "work"), item_ui),
    actionButton(paste0("submit_q", q), "Submit these answers", class = "btn-primary"),
    uiOutput(paste0("status_q", q))
  )
}

run_btn <- function(id, label = "Run the model (a few seconds)") actionButton(id, label, class = "btn-success")

# the workbench for each model question
workbench <- list(
  `1` = function() work_block(1, tagList(
    run_btn("run_q1"), tableOutput("q1_table"), plotOutput("q1_plot", height = 300))),
  `2` = function() work_block(2, tagList(
    sliderInput("q2_prior", "Prior: P(fake)", 0.01, 0.99, P_FAKE, step = 0.001, ticks = FALSE),
    sliderInput("q2_pf", "P(title has '!' | fake)", 0.001, 0.99, P_EXCL_FAKE, step = 0.001, ticks = FALSE),
    sliderInput("q2_pr", "P(title has '!' | real)", 0.001, 0.99, P_EXCL_REAL, step = 0.001, ticks = FALSE),
    uiOutput("q2_result"), plotOutput("q2_plot", height = 220))),
  `3` = function() work_block(3, tagList(
    p("Compares a pooled model (one line for everyone) with a model that gives each runner their own baseline."),
    run_btn("run_q3", "Run both models (about 10 seconds)"),
    tableOutput("q3_table"), plotOutput("q3_plot", height = 320))),
  `4` = function() work_block(4, tagList(
    p(sprintf("Data: %d of %d films from 1991 pass the Bechdel test.", Y91, N91)),
    fluidRow(column(6, sliderInput("q4_a", "Prior Beta(a, b):  a =", 0.5, 30, 5, step = 0.5, ticks = FALSE)),
             column(6, sliderInput("q4_b", "Prior Beta(a, b):  b =", 0.5, 30, 11, step = 0.5, ticks = FALSE))),
    uiOutput("q4_result"), plotOutput("q4_plot", height = 260))),
  `5` = function() work_block(5, tagList(
    run_btn("run_q5"), tableOutput("q5_table"), plotOutput("q5_plot", height = 300))),
  `6` = function() work_block(6, tagList(
    checkboxGroupInput("q6_preds", "Choose the measurements to use",
                       c("Bill length" = "bill_length_mm", "Bill depth" = "bill_depth_mm",
                         "Flipper length" = "flipper_length_mm", "Body mass" = "body_mass_g")),
    run_btn("run_q6", "Fit the classifier"), uiOutput("q6_result"), tableOutput("q6_table"),
    p(em("Accuracy is measured on the same penguins used to fit, so it is optimistic."))) ),
  `7` = function() work_block(7, tagList(
    p(strong("Note:"), "the playlist counts are simulated for this exercise. A talent agent represents all 44 artists in the data and wants to know whether pushing a client's songs onto curated playlists goes with higher popularity."),
    run_btn("run_q7", "Run both models (about 10 seconds)"),
    tableOutput("q7_table"), plotOutput("q7_plot", height = 340)))
)

scenario <- list(
  `1` = list("Perth hiking club",
             "Each evening you must decide whether to cancel tomorrow's outing. You have 1,000 days of Perth weather records, including 9am humidity and whether it rained the next day.",
             "Each 15-point rise in morning humidity multiplies the odds of rain tomorrow by about ___ (credible interval ___ to ___)."),
  `2` = list("News-literacy app",
             "In a labeled sample, some share of articles is fake, and fake titles use exclamation points far more often than real titles. A user pastes an article whose title has an exclamation point.",
             "Given the exclamation point, there is a ___% chance this article is fake."),
  `3` = list("Cherry Blossom race",
             "A sample of 36 runners each raced the 10-mile Cherry Blossom several years in a row (net time in minutes, age).",
             "Holding the runner constant, each additional year of age adds about ___ minutes, and runners differ from one another by about ___ minutes."),
  `4` = list("Film audit",
             "A film-studies instructor audits Hollywood films from 1991 for the Bechdel test. Earlier audits suggest roughly 1 in 3 films pass.",
             "There is a 95% chance the true pass rate for 1991 films is between ___ and ___."),
  `5` = list("Bike-share staffing",
             "A bike-share company has 500 days of data on daily rides and the 'feels-like' temperature, and needs to plan staffing.",
             "Each additional degree of felt temperature is associated with about ___ more rides per day (interval ___ to ___)."),
  `6` = list("Field research",
             "A researcher finds a penguin and measures its bill and flipper. The species is unknown (Adelie, Chinstrap or Gentoo).",
             "Given these measurements, the probabilities for each species are ___, ___ and ___."),
  `7` = list("Talent agent",
             "An agent represents all 44 artists in the dataset and advocates for each of them. Songs differ in how many curated playlists they appear on.",
             "For a given artist, each additional curated playlist is associated with about ___ more popularity points.")
)

model_question_tab <- function(q) {
  sc <- scenario[[as.character(q)]]
  mi <- items[[paste0("q", q, "m")]]
  tabPanel(paste0("Q", q), value = paste0("q", q),
           h3(paste0("Question ", q, ": ", sc[[1]])),
           div(class = "scenario", HTML(paste0("<b>Situation.</b> ", sc[[2]]))),
           div(class = "outcome", HTML(paste0("<b>Sentence you want to be able to say after modeling.</b> <i>", sc[[3]], "</i>"))),
           item_ui(mi),
           actionButton(paste0("lock_q", q), "Lock in my model choice", class = "btn-primary"),
           br(), br(),
           uiOutput(paste0("wb_q", q)))
}

matchset_tab <- function(q, title, intro) {
  tabPanel(paste0("Q", q), value = paste0("q", q),
           h3(paste0("Question ", q, ": ", title)),
           div(class = "scenario", HTML(intro)),
           if (q == 10) tagList(
             h5("Output from the class BART analysis (synthetic data, percentage points)"),
             tableOutput("q10_table")),
           lapply(items_for(q), item_ui),
           actionButton(paste0("submit_q", q), "Submit answers", class = "btn-primary"),
           uiOutput(paste0("status_q", q)))
}

ui <- function(request) {
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    tags$head(tags$style(HTML(css))),
    titlePanel("Bayesian Model Challenge"),
    fluidRow(column(4, textInput("student", NULL, placeholder = "Your name")),
             column(8, uiOutput("progress"))),
    tabsetPanel(id = "tabs", type = "pills",
                tabPanel("Start", value = "start",
                         h3("How this works"),
                         p("This is an individual challenge. Each question describes a situation and the sentence you want to be able to say after modeling.
           For Questions 1-7 you first choose the model. That unlocks a workbench where you run the model and answer questions about the output.
           Questions 8-10 mix matching, true/false and interpretation."),
                         tags$ul(
                           tags$li("You get one attempt per group of answers. Feedback appears after you submit."),
                           tags$li("Enter your name above, then download your report from the Results tab to turn in."),
                           tags$li("Models run live. They take a few seconds, so give them a moment after you click.")),
                         p(strong("Tip:"), "read the sentence you want to say first. The wording of the sentence usually points to the model.")),
                model_question_tab(1), model_question_tab(2), model_question_tab(3), model_question_tab(4),
                model_question_tab(5), model_question_tab(6), model_question_tab(7),
                matchset_tab(8, "Match the situation to the model",
                             "Choose the model that best fits each situation. Some of these are about data you haven't seen in this class."),
                matchset_tab(9, "True or false",
                             "Decide whether each statement is true or false."),
                matchset_tab(10, "Read the BART output",
                             "Households in the (synthetic) homeless-services data were compared across services with BART. Use the numbers below, which come from our class analysis."),
                tabPanel("Results", value = "results",
                         h3("Your results"), uiOutput("score_box"), br(),
                         tableOutput("results_table"), br(),
                         downloadButton("download", "Download my report (CSV)", class = "btn-primary"))
    )
  )
}

## ------------------------------------------------------------
## 6. Server
## ------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(res = list())
  max_pts <- length(items)

  total <- reactive(sum(vapply(rv$res, function(r) r$correct, logical(1))))
  n_done <- reactive(length(rv$res))

  output$progress <- renderUI({
    div(style = "text-align:right; padding-top:6px;",
        strong(sprintf("Score: %d / %d", total(), max_pts)),
        sprintf("  |  %d of %d items answered", n_done(), max_pts))
  })

  # ---- feedback outputs for every item ----
  lapply(items, function(it) {
    local({
      it <- it
      output[[paste0("fb_", it$id)]] <- renderUI({
        r <- rv$res[[it$id]]
        if (is.null(r)) return(NULL)
        div(class = paste("fb", if (r$correct) "ok" else "bad"),
            strong(if (r$correct) "Correct. " else paste0("Not quite. Answer: ", r$key, ". ")),
            it$explain)
      })
    })
  })

  # ---- grading ----
  grade_set <- function(its, status_id) {
    its <- Filter(function(it) is.null(rv$res[[it$id]]), its)
    if (length(its) == 0) { showNotification("Already submitted.", type = "message"); return() }
    if (any(vapply(its, function(it) is_blank(it, input[[it$id]]), logical(1)))) {
      showNotification("Answer every part first.", type = "warning"); return()
    }
    for (it in its) {
      r <- tryCatch(grade_item(it, input[[it$id]]),
                    error = function(e) list(correct = FALSE, given = as.character(input[[it$id]]), key = "(run the model first)"))
      rv$res[[it$id]] <- r
    }
  }

  for (q in 1:7) local({
    q <- q
    observeEvent(input[[paste0("lock_q", q)]], grade_set(items_for(q, "model")))
    observeEvent(input[[paste0("submit_q", q)]], grade_set(items_for(q, "work")))
    output[[paste0("wb_q", q)]] <- renderUI({
      if (is.null(rv$res[[paste0("q", q, "m")]]))
        div(class = "hint", "Lock in your model choice above to open the workbench.")
      else workbench[[as.character(q)]]()
    })
  })
  for (q in 8:10) local({
    q <- q
    observeEvent(input[[paste0("submit_q", q)]], grade_set(items_for(q)))
  })

  # ---- Q1: weather logistic ----
  q1_fit <- eventReactive(input$run_q1, withProgress(message = "Fitting model...", value = 0.5, get_fit("weather")))
  output$q1_table <- renderTable({
    d <- as.data.frame(q1_fit())
    bind_rows(sum_row("Intercept (log-odds)", d[["(Intercept)"]]), sum_row("humidity9am slope (log-odds per point)", d$humidity9am))
  })
  output$q1_plot <- renderPlot({
    d <- as.data.frame(q1_fit()); x <- seq(10, 100, length.out = 60); ix <- sample(nrow(d), 80)
    lines <- map_dfr(ix, ~ tibble(draw = .x, x = x, p = plogis(d[["(Intercept)"]][.x] + d$humidity9am[.x] * x)))
    ggplot(lines, aes(x, p, group = draw)) + geom_line(alpha = 0.15, color = "#2A6F97") +
      labs(x = "9am humidity", y = "P(rain tomorrow)", title = "Posterior curves for the chance of rain")
  })

  # ---- Q2: Bayes' theorem ----
  q2_post <- reactive({
    num <- input$q2_prior * input$q2_pf
    num / (num + (1 - input$q2_prior) * input$q2_pr)
  })
  output$q2_result <- renderUI(div(p("P(fake | title has '!') ="), span(class = "stat", fmt(q2_post(), 3))))
  output$q2_plot <- renderPlot({
    tibble(type = c("Fake with '!'", "Real with '!'"),
           n = 1000 * c(input$q2_prior * input$q2_pf, (1 - input$q2_prior) * input$q2_pr)) %>%
      ggplot(aes(x = 1, y = n, fill = type)) + geom_col(width = 0.5) + coord_flip() +
      geom_text(aes(label = round(n, 1)), position = position_stack(vjust = 0.5), color = "white", size = 5) +
      scale_fill_manual(values = c("Fake with '!'" = "#E07A5F", "Real with '!'" = "#2A6F97"), name = NULL) +
      labs(x = NULL, y = "Articles out of 1,000 that have '!' in the title",
           title = "Of the articles with '!', the fake share is the posterior") +
      theme(axis.text.y = element_blank(), legend.position = "bottom")
  })

  # ---- Q3: running ----
  q3_fits <- eventReactive(input$run_q3, withProgress(message = "Fitting models...", value = 0.5,
                                                      list(pool = get_fit("run_pool"), hier = get_fit("run_hier"))))
  output$q3_table <- renderTable({
    f <- q3_fits(); dh <- as.data.frame(f$hier); dp <- as.data.frame(f$pool)
    bind_rows(sum_row("Age slope, pooled (min/year)", dp$age),
              sum_row("Age slope, hierarchical (min/year)", dh$age),
              sum_row("Between-runner SD (min)", sqrt(dh[["Sigma[runner:(Intercept),(Intercept)]"]])),
              sum_row("Residual SD (min)", dh$sigma))
  })
  output$q3_plot <- renderPlot({
    f <- q3_fits(); cf <- coef(f$hier)$runner
    lines <- tibble(runner = rownames(cf), a = cf[["(Intercept)"]], b = cf[["age"]])
    ggplot(running, aes(age, net)) + geom_point(alpha = 0.35) +
      geom_abline(data = lines, aes(intercept = a, slope = b), color = "grey55", linewidth = 0.3) +
      geom_abline(intercept = coef(f$pool)[1], slope = coef(f$pool)[2], color = "blue", linewidth = 1.2) +
      coord_cartesian(ylim = range(running$net)) +
      labs(x = "Age", y = "Net time (min)", title = "Pooled line (blue) vs each runner's own line (grey)")
  })

  # ---- Q4: beta-binomial ----
  output$q4_result <- renderUI({
    a <- input$q4_a; b <- input$q4_b; ap <- a + Y91; bp <- b + N91 - Y91
    ci <- qbeta(c(.025, .975), ap, bp)
    tags$ul(tags$li(sprintf("Prior mean: %s", fmt(a / (a + b), 3))),
            tags$li(sprintf("Data proportion: %s (%d of %d)", fmt(Y91 / N91, 3), Y91, N91)),
            tags$li(strong(sprintf("Posterior Beta(%.1f, %.1f), mean %s, 95%% interval %s to %s",
                                   ap, bp, fmt(ap / (ap + bp), 3), fmt(ci[1], 3), fmt(ci[2], 3)))))
  })
  output$q4_plot <- renderPlot({
    a <- input$q4_a; b <- input$q4_b; p <- seq(0.001, 0.999, length.out = 400)
    bind_rows(tibble(p, d = dbeta(p, a, b), curve = "Prior"),
              tibble(p, d = dbeta(p, Y91 + 1, N91 - Y91 + 1), curve = "Likelihood (scaled)"),
              tibble(p, d = dbeta(p, a + Y91, b + N91 - Y91), curve = "Posterior")) %>%
      mutate(curve = factor(curve, levels = c("Prior", "Likelihood (scaled)", "Posterior"))) %>%
      ggplot(aes(p, d, color = curve, linetype = curve)) + geom_line(linewidth = 1.1) +
      scale_color_manual(values = c(Prior = "#9AA5B1", `Likelihood (scaled)` = "#E07A5F", Posterior = "#2A6F97"), name = NULL) +
      scale_linetype_manual(values = c(Prior = "dashed", `Likelihood (scaled)` = "dotted", Posterior = "solid"), name = NULL) +
      labs(x = "True pass rate", y = NULL) + theme(axis.text.y = element_blank(), legend.position = "bottom")
  })

  # ---- Q5: bikes ----
  q5_fit <- eventReactive(input$run_q5, withProgress(message = "Fitting model...", value = 0.5, get_fit("bikes")))
  output$q5_table <- renderTable({
    d <- as.data.frame(q5_fit())
    bind_rows(sum_row("Intercept", d[["(Intercept)"]]), sum_row("temp_feel slope (rides per degree)", d$temp_feel),
              sum_row("sigma", d$sigma))
  })
  output$q5_plot <- renderPlot({
    d <- as.data.frame(q5_fit()); ix <- sample(nrow(d), 80)
    ggplot(bikes, aes(temp_feel, rides)) + geom_point(alpha = 0.3) +
      geom_abline(intercept = d[["(Intercept)"]][ix], slope = d$temp_feel[ix], alpha = 0.15, color = "#2A6F97") +
      coord_cartesian(ylim = range(bikes$rides)) +
      labs(x = "Feels-like temperature", y = "Daily rides", title = "Data with 80 posterior regression lines")
  })

  # ---- Q6: naive Bayes ----
  q6_res <- eventReactive(input$run_q6, {
    validate(need(length(input$q6_preds) > 0, "Choose at least one measurement."))
    run_nb(input$q6_preds)
  })
  output$q6_result <- renderUI(div(p("Overall accuracy:"), span(class = "stat", fmt(q6_res()$acc, 3))))
  output$q6_table <- renderTable(as.data.frame.matrix(q6_res()$tab), rownames = TRUE)

  # ---- Q7: playlists ----
  q7_fits <- eventReactive(input$run_q7, withProgress(message = "Fitting models...", value = 0.5,
                                                      list(pool = get_fit("play_pool"), hier = get_fit("play_hier"))))
  output$q7_table <- renderTable({
    f <- q7_fits()
    bind_rows(sum_row("Playlist slope, pooled (popularity pts per playlist)", as.data.frame(f$pool)$n_playlists),
              sum_row("Playlist slope, hierarchical", as.data.frame(f$hier)$n_playlists))
  })
  output$q7_plot <- renderPlot({
    f <- q7_fits(); cf <- coef(f$hier)$artist
    lines <- tibble(a = cf[["(Intercept)"]], b = cf[["n_playlists"]])
    ggplot(spotify, aes(n_playlists, popularity, color = artist_genre)) + geom_point(alpha = 0.5) +
      geom_abline(intercept = lines$a, slope = lines$b, color = "grey55", linewidth = 0.3) +
      geom_abline(intercept = coef(f$pool)[1], slope = coef(f$pool)[2], color = "blue", linewidth = 1.2) +
      coord_cartesian(ylim = range(spotify$popularity)) +
      labs(x = "Curated playlists (simulated)", y = "Popularity", color = "Artist genre",
           title = "Pooled line (blue) vs each artist's own line (grey)")
  })

  # ---- Q10: BART table (from class analysis) ----
  output$q10_table <- renderTable(tibble(
    Comparison = c("Prevention - TH (eligible households)", "TH - RRH (ineligible households)"),
    `Mean` = c(-3.88, 0.98), `95% low` = c(-9.09, -5.08), `95% high` = c(0.52, 7.06),
    `P(difference < 0)` = c(0.96, 0.38)), digits = 2)

  # ---- Results ----
  report <- reactive({
    tibble(student = if (nzchar(input$student %||% "")) input$student else "(no name)",
           item = names(rv$res),
           question = map_chr(names(rv$res), ~ as.character(items[[.x]]$q)),
           part = map_chr(names(rv$res), ~ items[[.x]]$label),
           answer = map_chr(rv$res, ~ .x$given %||% ""),
           correct = map_lgl(rv$res, ~ .x$correct),
           points = as.integer(correct),
           time = format(Sys.time(), "%Y-%m-%d %H:%M"))
  })
  output$score_box <- renderUI({
    pct <- total() / max_pts
    badge <- if (n_done() < max_pts) "Keep going" else if (pct >= .9) "Posterior Legend" else if (pct >= .75) "Credible Interval Pro" else if (pct >= .5) "Prior Believer" else "Back to the Prior"
    div(class = "scenario", span(class = "stat", sprintf("%d / %d points (%.0f%%)", total(), max_pts, 100 * pct)),
        br(), sprintf("%d of %d items answered. Badge: %s", n_done(), max_pts, badge))
  })
  output$results_table <- renderTable({
    r <- report(); if (nrow(r) == 0) return(NULL)
    r %>% transmute(Question = question, Part = part, `Your answer` = answer, Result = if_else(correct, "Correct", "Incorrect"))
  })
  output$download <- downloadHandler(
    filename = function() paste0("bayes_challenge_", gsub("[^A-Za-z0-9]+", "_", input$student %||% "student"), ".csv"),
    content = function(file) {
      r <- report()
      r <- bind_rows(r, tibble(student = r$student[1] %||% "(no name)", item = "TOTAL", question = "", part = "",
                               answer = "", correct = NA, points = sum(r$points), time = format(Sys.time(), "%Y-%m-%d %H:%M")))
      write_csv(r, file)
    })
}

shinyApp(ui, server)
