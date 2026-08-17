library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(broom.mixed)

data("hotel_bookings")

hotel_model <- stan_glm(
  is_canceled ~ lead_time + previous_cancellations + is_repeated_guest + average_daily_rate,
  data = hotel_bookings,
  family = binomial,
  prior_intercept = normal(-0.7, 1.5),
  prior = normal(0, 0.5, autoscale = TRUE),
  chains = 4, iter = 5000*2, seed = 84735
)

hotel_model

exp(posterior_interval(hotel_model, prob = 0.80))

classification_summary(model = hotel_model, data = hotel_bookings, cutoff = 0.4)
