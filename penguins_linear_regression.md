# Challenge: Modeling Penguin Flipper Length with Bayesian Regression

You're going to build a Bayesian Normal regression model predicting flipper
length (`flipper_length_mm`) from bill length (`bill_length_mm`), using the
`penguins_bayes` dataset. Work through each section in order, and answer
every numbered question directly in your qmd, right under the code that
produces it.

## 1. Load the data

```r
library(tidyverse)
library(bayesrules)
library(rstanarm)

data <- bayesrules::penguins_bayes
```

## 2. Look before you model

Two quick plots, one sentence each describing what you see:

- A histogram of `flipper_length_mm`. Where is it centered? Roughly
  bell-shaped, or doing something unusual?
- A scatterplot of `flipper_length_mm` (y) against `bill_length_mm` (x).
  Linear? Positive or negative? Strong or weak?

## 3. Translate these three beliefs into priors

Read each statement carefully — you need to convert plain language into
specific numbers.

> **Typical flipper length.** Across all three species in this dataset, a
> reasonable guess for average flipper length is around 200mm — with a
> plausible range of roughly 180mm to 220mm.

> **The bill-to-flipper relationship.** Bill length and flipper length are
> both rough proxies for body size, so for every 1mm increase in bill
> length, flipper length typically increases by around 3.5mm — with a
> plausible range of roughly 1.5mm to 5.5mm.

> **Leftover variability.** Even penguins with identical bill lengths won't
> have identical flippers. A reasonable guess for the typical leftover
> spread is around 12.5mm.

**How to convert each one:**

- For a Normal prior, "around X, plausibly between A and B" means: X is
  your mean, and the width from A to B is your **95% range**, which equals
  **mean ± 1.96 × SD**. Solve for SD.
- For sigma's Exponential prior, there's no range to convert — just a
  single target mean. Recall **E(sigma) = 1/rate**, so `rate = 1/E(sigma)`.

Write your five numbers as plain variables before moving on:

```r
m0 <- ___   # centered intercept mean
s0 <- ___   # centered intercept SD
m1 <- ___   # slope mean
s1 <- ___   # slope SD
l  <- ___   # sigma's exponential rate
```

## 4. Fit the model

```r
model <- stan_glm(
  flipper_length_mm ~ bill_length_mm, data = data,
  family = gaussian,
  prior_intercept = normal(m0, s0),
  prior = normal(m1, s1),
  prior_aux = exponential(l),
  chains = 4, iter = 2000, seed = 84735
)
```

**Before reading your results, a critical note:** the `(Intercept)` row in
your model's printout is *not* the same number as `m0`. `m0`/`s0` describe
**beta0c**, the typical flipper length **at the average bill length** in the
dataset. `stan_glm()` always reports the **raw** intercept instead — typical
flipper length at a bill length of *zero*, which is not a meaningful
scenario and will look nothing like `m0`. To get the number you actually
tuned a prior for, you have to compute it yourself from the posterior draws:

```r
posterior <- as.data.frame(model)
beta0c_draws <- posterior$`(Intercept)` + posterior$bill_length_mm * mean(data$bill_length_mm)
```

`beta0c_draws` is now a vector of posterior draws for the *centered*
intercept — use this, not the raw `(Intercept)` column, for anything below
that asks about "typical flipper length at the average bill length."

## 5. Report your results

For each item below, the exact function to use is named — plug in the right
column name and run it.

**1. The posterior median relationship.**
```r
median(posterior$`(Intercept)`)
median(posterior$bill_length_mm)
```
Write the fitted equation: flipper_length_mm = ___ + ___ × bill_length_mm.

**2. A 90% credible interval for the slope.**
```r
quantile(posterior$bill_length_mm, c(0.05, 0.95))
```
One sentence: what does this range say about how flipper length changes
with bill length?

**3. A 90% credible interval for the centered intercept.**
```r
quantile(beta0c_draws, c(0.05, 0.95))
```
Use `beta0c_draws` here, not the raw `(Intercept)` column — see the note in
Step 4.

**4. The posterior probability the slope is positive.**
```r
mean(posterior$bill_length_mm > 0)
```
One sentence: what does this number tell you that a p-value wouldn't?

**5. MCMC diagnostics.**
```r
summary(model)[, c("Rhat", "n_eff")]
```
Confirm Rhat is close to 1 for every parameter before trusting anything
above.

**6. Compare to a plain linear model.**
```r
lm(flipper_length_mm ~ bill_length_mm, data = data)
```
