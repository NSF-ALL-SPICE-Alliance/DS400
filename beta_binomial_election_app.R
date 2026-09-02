## ============================================================
## Beta-Binomial Explorer -- Two Tabs + Exercise Reference
## Tab 1: The Beta prior model alone (Michelle's election, prior only)
## Tab 2: Prior + Likelihood + Posterior, the full picture
## Tab 3: The same 8 exercises below, for reference while you work
##
## Run with: shiny::runApp("app.R")
##
## Design notes:
## - Equations are rendered with CSS fractions, not MathJax. This
##   avoids CDN load timing issues that can make LaTeX fail to
##   typeset in a live-lecture setting (no internet dependency,
##   renders instantly and identically every time).
## - Both tabs shade the 95% credible interval under the curve and
##   mark its bounds with dotted vertical lines, in addition to the
##   solid mean / dashed mode lines already present on tab 1.
## - Narrative callouts throughout recap the Michelle's-election
##   scenario from the text, so the app can stand alone as a
##   review/study tool, not just an in-lecture demo.
## ============================================================


## ================= EXERCISES: ANSWER HERE =================
## Work through these using the app (run it below), then write your
## answers directly in this script as comments. Save the file when
## you're done -- your answers travel with the script, not the app.
##
## ------------------------------------------------------------
## Q1 (Tab 1) -- Suppose a different analyst believes Michelle's support
## is centered around 60%, and is fairly confident -- most of their
## belief falls between 55% and 65%. Find alpha, beta that produce this
## shape. Report alpha, beta, the resulting mean, and the 95% credible
## interval.
##
## Your answer:
##
##
## ------------------------------------------------------------
## Q2 (Tab 1) -- Starting from Michelle's actual prior (reset to
## Beta(45, 55)), scale both alpha and beta up by 4x (Beta(180, 220))
## while keeping the 45:55 ratio fixed. What happens to the mean? What
## happens to the width of the 95% credible interval? In one sentence,
## explain why the mean doesn't move but the interval does.
##
## Your answer:
##
##
## ------------------------------------------------------------
## Q3 (Tab 1) -- Set alpha = beta = 1 (the flat prior). What are the
## mean, mode, and SD? Now try alpha = beta = 0.5. Both cases report
## "no single mode" in the app -- but for different reasons. Explain,
## in your own words, why Beta(1,1) has no single mode (hint: is one
## point more plausible than any other?) versus why Beta(0.5, 0.5) has
## no single mode (hint: where does the density pile up?).
##
## Your answer:
##
##
## ------------------------------------------------------------
## Q4 (Tab 2) -- Set alpha = beta = 1, then n = 50, y = 30. Compare the
## Posterior curve to the Scaled Likelihood curve. What do you notice?
## Explain why this happens in terms of what a flat prior represents.
##
## Your answer:
##
##
## ------------------------------------------------------------
## Q5 (Tab 2) -- Reset to the text's numbers (alpha=45, beta=55, n=50,
## y=30). Read off the weight sentence under the posterior summary. Now
## change n to 500, keeping y at 60% of n (y=300). How do the weights
## shift? What does this tell you about what it takes for new data to
## overrule a moderately confident prior?
##
## Your answer:
##
##
## ------------------------------------------------------------
## Q6 (Tab 2) -- Using the text's numbers (alpha=45, beta=55, n=50,
## y=30), check "Show frequentist 95% confidence interval." Compare the
## two intervals shown. In one or two sentences, explain what the
## Bayesian credible interval says that the frequentist confidence
## interval cannot -- and why.
##
## Your answer:
##
##
## ------------------------------------------------------------
## Q7 (Tab 2) -- Set alpha = 90, beta = 10 (strong prior belief support
## is ~90%). Now set n = 200, y = 40 (20% support in the new poll -- a
## huge disagreement with the prior). Where does the posterior end up --
## closer to the prior or closer to the data? Does this match the
## weight sentence? Why did the data win here when it didn't in Q5?
##
## Your answer:
##
##
## ------------------------------------------------------------
## Q8 (Reflection -- no fixed answer) -- The app's posterior treats
## every respondent in the poll equally, regardless of when the
## historical belief underlying the prior was formed versus when the
## new poll was taken. Using Q5 and Q7, explain in 2-3 sentences why
## this could be a problem for a real campaign tracking support over
## several months -- and what you'd want the model to do differently
## if you suspected support was genuinely trending upward, not just
## noisily varying around a fixed number.
##
## Your answer:
##
##
## ============================================================


library(shiny)
library(tidyverse)

# ---- Helpers -------------------------------------------------

beta_curve <- function(alpha, beta, label, n_points = 500) {
  x <- seq(0, 1, length.out = n_points)
  tibble(pi = x, density = dbeta(x, alpha, beta), curve = label)
}

scaled_likelihood <- function(y, n, n_points = 500) {
  x <- seq(0, 1, length.out = n_points)
  lik <- dbinom(y, size = n, prob = x)
  area <- sum(diff(x) * (head(lik, -1) + tail(lik, -1)) / 2)
  tibble(pi = x, density = lik / area, curve = "Scaled Likelihood")
}

beta_mean <- function(a, b) a / (a + b)

beta_mode <- function(a, b) {
  if (a > 1 && b > 1) (a - 1) / (a + b - 2)
  else if (a <= 1 && b > 1) 0
  else if (b <= 1 && a > 1) 1
  else NA_real_   # both <= 1: bimodal at 0 and 1, no single mode
}

beta_sd <- function(a, b) sqrt((a * b) / ((a + b)^2 * (a + b + 1)))

# Builds one exercise card: a numbered question and which tab to use.
# Answers are NOT collected here -- students write them as comments at
# the top of this script, so their work travels with the file.
exercise_card <- function(n, tab, prompt) {
  wellPanel(
    class = "exercise-card",
    h4(sprintf("Question %d", n), tags$span(class = "exercise-tab-badge", tab)),
    p(prompt)
  )
}

# CSS-rendered fraction: no MathJax, no JS timing issues, always renders.
html_frac <- function(top, bottom) {
  sprintf(
    '<span class="bb-frac"><span class="bb-num">%s</span><span class="bb-denom">%s</span></span>',
    top, bottom
  )
}

app_css <- tags$head(tags$style(HTML("
  .bb-frac {
    display: inline-block;
    vertical-align: middle;
    margin: 0 6px;
    text-align: center;
    font-size: 1.05em;
    line-height: 1.1;
  }
  .bb-num {
    display: block;
    border-bottom: 2px solid #333;
    padding: 0 6px 2px 6px;
  }
  .bb-denom {
    display: block;
    padding: 2px 6px 0 6px;
  }
  .eq-card {
    background: #f7f7f9;
    border: 1px solid #e1e1e8;
    border-radius: 6px;
    padding: 14px 18px;
    margin-top: 8px;
  }
  .eq-line {
    font-size: 1.15em;
    margin: 10px 0;
  }
  .eq-result {
    font-weight: 700;
    color: #2a5d34;
  }
  .story-panel {
    background: #eef4fb;
    border-left: 4px solid #4C72B0;
    border-radius: 4px;
    padding: 10px 16px;
    margin-bottom: 14px;
  }
  .interp-panel {
    background: #f2f8f0;
    border-left: 4px solid #55A868;
    border-radius: 4px;
    padding: 10px 16px;
    margin-top: 10px;
  }
  .exercise-card {
    border-left: 4px solid #4C72B0;
  }
  .exercise-tab-badge {
    display: inline-block;
    margin-left: 10px;
    font-size: 0.6em;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.03em;
    color: #4C72B0;
    background: #eef4fb;
    border-radius: 10px;
    padding: 3px 10px;
    vertical-align: middle;
  }
")))

# ---- UI --------------------------------------------------------

ui <- navbarPage(
  title = "Beta-Binomial Explorer",
  header = app_css,

  ## ---------------- TAB 1: Prior model only ----------------
  tabPanel(
    "1. The Prior Model",
    sidebarLayout(
      sidebarPanel(
        div(class = "story-panel",
            strong("The scenario:"),
            " You're campaign manager for \u201cMichelle,\u201d who's running for ",
            "president. Over the campaign, you've tracked 30 polls of Minnesota ",
            "voters. Her support has hovered around 45%, dipping to about 35% on ",
            "rough days and climbing to about 55% on good days. Before any new ",
            "polling, that history ", em("is"), " your prior belief about ",
            "\u03c0, her true current support.",
            tags$br(), tags$br(),
            em("Note: we're never told how many people were surveyed in each ",
               "of those 30 polls -- just the reported percentage each time. ",
               "So \u03b1 and \u03b2 here aren't a literal tally of real ",
               "respondents. They're ", strong("tuned"), " by trial and error ",
               "so the Beta curve's shape matches what you observed: centered ",
               "near 45%, with most of the spread between 35% and 55%. That's ",
               "different from a case like Milgram's study later this lecture, ",
               "where \u03b1 and \u03b2 really are small literal pseudo-counts ",
               "the researcher had in mind.")
        ),
        h4("Tune the prior: \u03c0 ~ Beta(\u03b1, \u03b2)"),
        sliderInput("p_alpha", "\u03b1 (prior successes)", min = 0.5, max = 250, value = 45, step = 0.5),
        sliderInput("p_beta",  "\u03b2 (prior failures)",  min = 0.5, max = 250, value = 55, step = 0.5),
        actionButton("reset_prior", "Reset to Michelle's Beta(45, 55) prior"),
        hr(),
        helpText("Pseudo-observation framing: \u03b1 is 'as if' you'd already ",
                 "seen this many prior successes, \u03b2 this many prior ",
                 "failures. Drag the sliders and watch the mean (solid line), ",
                 "mode (dashed line), and 95% credible band (shaded) all move."),
        helpText("Try \u03b1 = \u03b2 = 1 for the flat/uniform prior -- no ",
                 "opinion at all. Then try keeping the 45:55 ratio fixed but ",
                 "scaling both up (e.g. 90 and 110) -- the mean won't move, ",
                 "but the band will narrow: same belief, held more confidently.")
      ),
      mainPanel(
        plotOutput("priorPlot", height = "380px"),
        uiOutput("priorEquations"),
        uiOutput("priorInterpretation")
      )
    )
  ),

  ## ---------------- TAB 2: Full picture ----------------
  tabPanel(
    "2. Prior + Likelihood + Posterior",
    sidebarLayout(
      sidebarPanel(
        div(class = "story-panel",
            strong("New data arrives:"), " You commission a new poll of ",
            em("n"), " Minnesotans and record ", em("y"), ", the number who ",
            "support Michelle. The default below matches the text: a poll of ",
            "50 voters in which 30 (60%) supported her -- noticeably higher ",
            "than her 45% prior average."
        ),
        h4("Prior: \u03c0 ~ Beta(\u03b1, \u03b2)"),
        sliderInput("alpha", "\u03b1 (prior successes)", min = 0.5, max = 250, value = 45, step = 0.5),
        sliderInput("beta",  "\u03b2 (prior failures)",  min = 0.5, max = 250, value = 55, step = 0.5),

        hr(),

        h4("Data: Y successes in n trials"),
        sliderInput("n", "n (trials)", min = 1, max = 600, value = 50, step = 1),
        uiOutput("y_slider"),
        actionButton("reset_data", "Reset to the text's poll: y = 30, n = 50"),

        hr(),
        checkboxInput("show_freq_ci", "Show frequentist 95% confidence interval (data alone, ignoring the prior)", value = FALSE),

        hr(),
        helpText("Set \u03b1 = \u03b2 = 1 (flat prior) and watch the posterior ",
                 "land exactly on top of the scaled likelihood -- with no real ",
                 "prior opinion, the data alone determines the posterior."),
        helpText("Then try a big \u03b1 + \u03b2 (say 200 total) with a small ",
                 "n (say 5) -- watch the posterior barely move off the prior.")
      ),

      mainPanel(
        plotOutput("betaPlot", height = "400px"),
        br(),
        wellPanel(
          h4("Posterior summary"),
          textOutput("credible_sentence"),
          uiOutput("freq_ci_sentence"),
          uiOutput("weight_sentence"),
          verbatimTextOutput("summaryTable")
        ),
        fluidRow(
          column(4, h5("Prior"),     textOutput("prior_mean"), textOutput("prior_sd")),
          column(4, h5("Data"),      textOutput("data_summary")),
          column(4, h5("Posterior"), textOutput("post_mean"),  textOutput("post_sd"))
        )
      )
    )
  ),

  ## ---------------- TAB 3: Exercises ----------------
  tabPanel(
    "3. Exercises",
    fluidPage(
      div(class = "story-panel",
          strong("How to use this tab:"), " Each question tells you which tab to ",
          "use and what to set the sliders to. Work the sliders, read off the ",
          "numbers the app gives you, then write your answer as a comment at the ",
          "top of ", code("app.R"), " (the script you downloaded to run this app). ",
          "That way your answers are saved with the script itself, not lost when ",
          "you close the app."
      ),
      exercise_card(
        n = 1, tab = "Tab 1",
        prompt = HTML(paste0(
          "Suppose a different analyst believes Michelle's support is centered ",
          "around 60%, and is fairly confident -- most of their belief falls ",
          "between 55% and 65%. Using Tab 1, find values of \u03b1 and \u03b2 that ",
          "produce this shape. Report your \u03b1, \u03b2, the resulting mean, and ",
          "the 95% credible interval. (There's no single right answer -- any ",
          "reasonable pair that matches the description is fine.)"
        ))
      ),
      exercise_card(
        n = 2, tab = "Tab 1",
        prompt = HTML(paste0(
          "Starting from Michelle's actual prior (reset to Beta(45, 55)), scale ",
          "both \u03b1 and \u03b2 up by 4\u00d7 (i.e. Beta(180, 220)) while keeping ",
          "the 45:55 ratio fixed. What happens to the mean? What happens to the ",
          "width of the 95% credible interval? In one sentence, explain ",
          "<em>why</em> the mean doesn't move but the interval does."
        ))
      ),
      exercise_card(
        n = 3, tab = "Tab 1",
        prompt = HTML(paste0(
          "Set \u03b1 = \u03b2 = 1 (the flat prior). What are the mean, mode, and ",
          "SD? Now try \u03b1 = \u03b2 = 0.5. Both cases report \"no single mode\" ",
          "in the app -- but for different reasons. Look at the shape of each ",
          "curve and explain, in your own words, why Beta(1,1) has no single ",
          "mode (hint: what does the curve look like -- is one point more ",
          "plausible than any other?) versus why Beta(0.5, 0.5) has no single ",
          "mode (hint: where does the density pile up?)."
        ))
      ),
      exercise_card(
        n = 4, tab = "Tab 2",
        prompt = HTML(paste0(
          "On Tab 2, set \u03b1 = \u03b2 = 1, then set n = 50, y = 30. Compare the ",
          "Posterior curve to the Scaled Likelihood curve. What do you notice? ",
          "Explain <em>why</em> this happens in terms of what a flat prior ",
          "represents."
        ))
      ),
      exercise_card(
        n = 5, tab = "Tab 2",
        prompt = HTML(paste0(
          "Reset to the text's numbers (\u03b1=45, \u03b2=55, n=50, y=30). Read ",
          "off the weight sentence under the posterior summary. Now change n to ",
          "500 while keeping y at 60% of n (i.e. y=300). How do the weights ",
          "shift? What does this tell you about what it takes for new data to ",
          "overrule a moderately confident prior?"
        ))
      ),
      exercise_card(
        n = 6, tab = "Tab 2",
        prompt = HTML(paste0(
          "Using the text's numbers (\u03b1=45, \u03b2=55, n=50, y=30), check the ",
          "\"Show frequentist 95% confidence interval\" box. Compare the two ",
          "intervals shown. In one or two sentences, explain what the Bayesian ",
          "credible interval says that the frequentist confidence interval ",
          "cannot -- and why."
        ))
      ),
      exercise_card(
        n = 7, tab = "Tab 2",
        prompt = HTML(paste0(
          "Set a very confident prior: \u03b1 = 90, \u03b2 = 10 (strong prior belief ",
          "that support is ~90%). Now set n = 200, y = 40 (20% support in the new ",
          "poll -- a huge disagreement with the prior). Where does the posterior ",
          "end up -- closer to the prior or closer to the data? Does this match ",
          "what the weight sentence predicts? Why did the data win here when it ",
          "didn't in Question 5?"
        ))
      ),
      exercise_card(
        n = 8, tab = "Reflection -- no fixed answer",
        prompt = HTML(paste0(
          "The app's posterior treats every respondent in the poll equally, ",
          "regardless of when the historical belief underlying the prior was ",
          "formed versus when the new poll was taken. Using what you found in ",
          "Questions 5 and 7, explain in 2-3 sentences why this could be a ",
          "problem for a real campaign tracking support over several months -- ",
          "and what you'd want the model to do differently if you suspected ",
          "Michelle's support was genuinely trending upward, not just noisily ",
          "varying around a fixed number."
        ))
      )
    )
  )
)

# ---- Server ------------------------------------------------------

server <- function(input, output, session) {

  ## ===== TAB 1: Prior only =====

  observeEvent(input$reset_prior, {
    updateSliderInput(session, "p_alpha", value = 45)
    updateSliderInput(session, "p_beta",  value = 55)
  })

  output$priorPlot <- renderPlot({
    a <- input$p_alpha; b <- input$p_beta
    df <- beta_curve(a, b, "Prior")
    m  <- beta_mean(a, b)
    md <- beta_mode(a, b)
    ci <- qbeta(c(0.025, 0.975), a, b)

    ribbon_df <- df %>% filter(pi >= ci[1], pi <= ci[2])

    p <- ggplot(df, aes(x = pi, y = density)) +
      geom_area(fill = "#4C72B0", alpha = 0.12) +
      geom_area(data = ribbon_df, fill = "#4C72B0", alpha = 0.35) +
      geom_line(color = "#4C72B0", linewidth = 1.2) +
      geom_vline(xintercept = ci, color = "#4C72B0", linetype = "dotted", linewidth = 0.8) +
      geom_vline(xintercept = m, color = "#2a2a2a", linetype = "solid", linewidth = 0.9) +
      labs(x = expression(pi), y = "Density",
           title = sprintf("Beta(%.1f, %.1f) Prior for \u03c0", a, b),
           subtitle = "Solid = mean \u00b7 Dashed = mode \u00b7 Dotted = 95% credible bounds \u00b7 Shaded = 95% credible region") +
      theme_minimal(base_size = 15)

    if (!is.na(md)) {
      p <- p + geom_vline(xintercept = md, color = "#2a2a2a", linetype = "dashed", linewidth = 0.9)
    }
    p
  })

  output$priorEquations <- renderUI({
    a <- input$p_alpha; b <- input$p_beta
    m  <- beta_mean(a, b)
    sd <- beta_sd(a, b)
    md <- beta_mode(a, b)

    mean_line <- HTML(sprintf(
      '<div class="eq-line">E(\u03c0) = %s = %s = <span class="eq-result">%.3f</span></div>',
      html_frac("\u03b1", "\u03b1 + \u03b2"),
      html_frac(sprintf("%.1f", a), sprintf("%.1f + %.1f", a, b)),
      m
    ))

    mode_line <- if (!is.na(md)) {
      HTML(sprintf(
        '<div class="eq-line">Mode(\u03c0) = %s = %s = <span class="eq-result">%.3f</span></div>',
        html_frac("\u03b1 - 1", "\u03b1 + \u03b2 - 2"),
        html_frac(sprintf("%.1f", a - 1), sprintf("%.1f", a + b - 2)),
        md
      ))
    } else {
      HTML('<div class="eq-line">Mode(\u03c0): no single mode when both \u03b1 \u2264 1 and \u03b2 \u2264 1 (density is bimodal, piling up at 0 and 1)</div>')
    }

    sd_line <- HTML(sprintf(
      '<div class="eq-line">SD(\u03c0) = &radic;( %s ) = <span class="eq-result">%.3f</span></div>',
      html_frac(sprintf("\u03b1\u03b2 = %.1f", a * b),
                sprintf("(\u03b1+\u03b2)\u00b2(\u03b1+\u03b2+1) = %.1f", (a + b)^2 * (a + b + 1))),
      sd
    ))

    div(class = "eq-card", mean_line, mode_line, sd_line)
  })

  output$priorInterpretation <- renderUI({
    a <- input$p_alpha; b <- input$p_beta
    ci <- qbeta(c(0.025, 0.975), a, b)
    n0 <- a + b

    text <- sprintf(
      paste0(
        "<strong>In words:</strong> Before any new poll, you believe there's a ",
        "95%% chance Michelle's true support \u03c0 is between %.0f%% and %.0f%%, ",
        "with %.0f%% as your single best guess. Once you've tuned \u03b1 and \u03b2 ",
        "to match that belief, their sum (\u03b1 + \u03b2 \u2248 %.0f) behaves ",
        "<em>as if</em> it were a sample size -- call it an 'effective' prior sample ",
        "size, not a real headcount from the 30 polls. The bigger that number, the ",
        "harder a real new poll will have to work to change your mind, exactly the ",
        "way a bigger real sample would."
      ),
      100 * ci[1], 100 * ci[2], 100 * beta_mean(a, b), n0
    )

    div(class = "interp-panel", HTML(text))
  })

  ## ===== TAB 2: Full prior + likelihood + posterior =====

  observeEvent(input$reset_data, {
    updateSliderInput(session, "n", value = 50)
    updateSliderInput(session, "y", value = 30)
  })

  output$y_slider <- renderUI({
    sliderInput("y", "y (observed successes)", min = 0, max = input$n,
                value = min(30, input$n), step = 1)
  })

  post_alpha <- reactive({ req(input$y); input$alpha + input$y })
  post_beta  <- reactive({ req(input$y); input$beta + input$n - input$y })

  output$betaPlot <- renderPlot({
    req(input$y)

    prior_df <- beta_curve(input$alpha, input$beta, "Prior")
    post_df  <- beta_curve(post_alpha(), post_beta(), "Posterior")
    lik_df   <- scaled_likelihood(input$y, input$n)

    post_ci <- qbeta(c(0.025, 0.975), post_alpha(), post_beta())
    post_ribbon <- post_df %>% filter(pi >= post_ci[1], pi <= post_ci[2])

    all_df <- bind_rows(prior_df, lik_df, post_df) %>%
      mutate(curve = factor(curve, levels = c("Prior", "Scaled Likelihood", "Posterior")))

    p <- ggplot(all_df, aes(x = pi, y = density, color = curve, fill = curve)) +
      geom_area(alpha = 0.18, position = "identity") +
      geom_area(data = post_ribbon, aes(x = pi, y = density),
                inherit.aes = FALSE, fill = "#55A868", alpha = 0.35) +
      geom_vline(xintercept = post_ci, color = "#2a5d34", linetype = "dotted", linewidth = 0.8) +
      geom_line(linewidth = 1.1) +
      scale_color_manual(values = c("Prior" = "#4C72B0",
                                    "Scaled Likelihood" = "#DD8452",
                                    "Posterior" = "#55A868")) +
      scale_fill_manual(values = c("Prior" = "#4C72B0",
                                   "Scaled Likelihood" = "#DD8452",
                                   "Posterior" = "#55A868")) +
      labs(x = expression(pi), y = "Density", color = NULL, fill = NULL,
           title = "Prior, (Scaled) Likelihood, and Posterior",
           subtitle = "Shaded green + dotted lines = posterior's 95% credible region") +
      theme_minimal(base_size = 15) +
      theme(legend.position = "top")

    if (isTRUE(input$show_freq_ci)) {
      freq_ci <- binom.test(input$y, input$n)$conf.int
      p <- p +
        geom_vline(xintercept = freq_ci, color = "#8B3A3A", linetype = "twodash", linewidth = 0.9) +
        labs(subtitle = "Shaded green + dotted = posterior's 95% credible region \u00b7 Dark red twodash = frequentist 95% CI (data alone)")
    }

    p
  })

  output$credible_sentence <- renderText({
    req(input$y)
    ci <- qbeta(c(0.025, 0.975), post_alpha(), post_beta())
    sprintf(
      "Bayesian: there's a 95%% probability that Michelle's true support \u03c0 lies between %.1f%% and %.1f%%.",
      100 * ci[1], 100 * ci[2]
    )
  })

  output$freq_ci_sentence <- renderUI({
    req(input$y)
    if (!isTRUE(input$show_freq_ci)) return(NULL)

    freq_ci <- binom.test(input$y, input$n)$conf.int

    text <- sprintf(
      paste0(
        "<p style='margin-top:4px; color:#8B3A3A;'>",
        "<strong>Frequentist (data alone, ignoring the prior):</strong> ",
        "the 95%% confidence interval is %.1f%% to %.1f%%.</p>",
        "<p style='color:#8B3A3A;'>",
        "This is a <em>different kind</em> of statement than the Bayesian one above. ",
        "In plain English: if you ran this exact poll over and over, thousands of ",
        "times, and built an interval the same way each time, about 95%% of those ",
        "intervals would end up containing Michelle's true support somewhere inside ",
        "them. But for <em>this one</em> poll you actually ran, you can't say ",
        "\"there's a 95%% chance \u03c0 is in this interval\" -- \u03c0 either is in ",
        "it or it isn't. The 95%% describes how reliable the <em>method</em> is over ",
        "many repeats, not how confident you should be about this one result.",
        "</p>",
        "<p style='color:#8B3A3A; font-style: italic;'>",
        "An analogy: a net that catches fish 95%% of the time. After one cast, an ",
        "empty net doesn't mean \"there's a 95%% chance a fish was nearby\" -- the ",
        "95%% describes the net's track record, not this particular cast.",
        "</p>",
        "<p style='color:#8B3A3A;'>",
        "Compare this to the Bayesian credible interval above, which <em>can</em> say ",
        "\"there's a 95%% probability \u03c0 is in this range\" directly -- because the ",
        "Bayesian approach treats \u03c0 itself as having a probability distribution, ",
        "while the frequentist approach treats \u03c0 as a fixed, unknown constant.",
        "</p>"
      ),
      100 * freq_ci[1], 100 * freq_ci[2]
    )

    HTML(text)
  })

  output$weight_sentence <- renderUI({
    req(input$y)
    n0 <- input$alpha + input$beta   # prior "pseudo-sample size"
    n  <- input$n
    w_prior <- n0 / (n0 + n)
    w_data  <- n / (n0 + n)

    p(
      style = "margin-top: 6px; font-style: italic;",
      sprintf(
        paste0(
          "Think of the posterior mean as a blend: your prior (worth ~%.0f ",
          "pseudo-voters) contributes %.0f%% of the weight, and your new poll ",
          "(n = %.0f voters) contributes the other %.0f%%."
        ),
        n0, 100 * w_prior, n, 100 * w_data
      )
    )
  })

  output$prior_mean <- renderText(sprintf("Mean: %.3f", beta_mean(input$alpha, input$beta)))
  output$prior_sd   <- renderText(sprintf("SD: %.3f",   beta_sd(input$alpha, input$beta)))

  output$data_summary <- renderText({
    req(input$y)
    sprintf("%d successes / %d trials (%.0f%%)", input$y, input$n, 100 * input$y / input$n)
  })

  output$post_mean <- renderText({
    req(input$y)
    sprintf("Mean: %.3f", beta_mean(post_alpha(), post_beta()))
  })
  output$post_sd <- renderText({
    req(input$y)
    sprintf("SD: %.3f", beta_sd(post_alpha(), post_beta()))
  })

  output$summaryTable <- renderPrint({
    req(input$y)
    cat(sprintf("Prior:     Beta(%.1f, %.1f)\n", input$alpha, input$beta))
    cat(sprintf("Data:      y = %d, n = %d\n", input$y, input$n))
    cat(sprintf("Posterior: Beta(%.1f, %.1f)\n", post_alpha(), post_beta()))
  })
}

shinyApp(ui, server)
