# shiny/app.R
# Instrument Validity Explorer
#
# Two-panel app visualizing the IV identification strategy:
#   Left:  First stage  — rainfall → distance from protest
#   Right: Reduced form — rainfall → turnout
#
# Controls: county filter, LOESS bandwidth, toggle spatial outliers
# Live stats: first-stage F-stat, reduced-form coefficient, implied IV ratio

library(shiny)
library(tidyverse)
library(ivreg)

# ---------------------------------------------------------------------------
# Load data
# Data is exported by the `shiny_data` target in _targets.R.
# Run `targets::tar_make()` from the project root before launching the app.
# ---------------------------------------------------------------------------

data_path <- "data/plot_data.rds"  # relative to shiny/

if (!file.exists(data_path)) {
  stop(
    "shiny/data/plot_data.rds not found.\n",
    "Run targets::tar_make() from the project root to generate it."
  )
}

GA_tbl <- readRDS(data_path) |>
  filter(is.finite(log_dist), is.finite(log_rel), is.finite(General_2020_11_03))

county_choices <- c("All Georgia" = "ALL", sort(unique(GA_tbl$county_name)))

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("BLM Turnout: Instrument Validity Explorer"),
  p("Explore the first stage and reduced form that underpin the rainfall IV design."),

  fluidRow(
    column(3,
      wellPanel(
        selectInput("county", "Highlight county:",
                    choices  = county_choices,
                    selected = "ALL"),
        sliderInput("span", "LOESS smoother bandwidth:",
                    min = 0.1, max = 1.0, value = 0.5, step = 0.05),
        checkboxInput("outliers", "Show spatial outliers", value = TRUE),
        hr(),
        h5("Live IV Statistics"),
        tableOutput("iv_stats")
      )
    ),
    column(4,
      h4("First Stage"),
      p(em("Rainfall → Distance from protest")),
      p("A negative slope confirms instrument relevance: more rain, farther from protests."),
      plotOutput("plot_first_stage", height = "420px")
    ),
    column(4,
      h4("Reduced Form"),
      p(em("Rainfall → 2020 Turnout")),
      p("A negative slope is the intention-to-treat effect of rainfall on turnout."),
      plotOutput("plot_reduced_form", height = "420px")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # Reactive: filter and flag data
  plot_data <- reactive({
    df <- GA_tbl

    # Flag selected county
    df <- df |>
      mutate(
        highlight = if (input$county == "ALL") FALSE
                    else county_name == input$county
      )

    # Flag high-leverage outliers (hat values from first-stage OLS)
    fs_hat <- hatvalues(lm(log_dist ~ log_rel + pop_dens + nh_black, data = df))
    df$outlier <- fs_hat > (2 * mean(fs_hat))

    if (!input$outliers) df <- filter(df, !outlier)

    df
  })

  # Reactive: fit models on filtered data
  models <- reactive({
    df <- plot_data()

    fs <- lm(log_dist ~ log_rel + pop_dens + nh_black, data = df)
    rf <- lm(General_2020_11_03 ~ log_rel + General_2018_11_06, data = df)

    list(first_stage = fs, reduced_form = rf, data = df)
  })

  # ---------------------------------------------------------------------------
  # First stage plot
  # ---------------------------------------------------------------------------
  output$plot_first_stage <- renderPlot({
    m  <- models()
    df <- m$data

    ggplot(df, aes(x = log_rel, y = log_dist)) +
      geom_point(
        aes(color = highlight, alpha = if_else(highlight, 1, 0.3)),
        size  = 0.8,
        show.legend = FALSE
      ) +
      geom_smooth(method = "loess", span = input$span,
                  color = "#2c7bb6", fill = "#abd9e9", linewidth = 1) +
      geom_smooth(method = "lm", se = FALSE,
                  color = "#d7191c", linewidth = 0.8, linetype = "dashed") +
      scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "#e67e22")) +
      scale_alpha_identity() +
      labs(
        x = "log(Relative rainfall + 1)",
        y = "log(Distance from protest + 1)",
        caption = sprintf(
          "OLS slope: %.3f | F-stat: %.1f",
          coef(m$first_stage)["log_rel"],
          summary(m$first_stage)$fstatistic[1]
        )
      ) +
      theme_minimal(base_size = 13)
  })

  # ---------------------------------------------------------------------------
  # Reduced form plot
  # ---------------------------------------------------------------------------
  output$plot_reduced_form <- renderPlot({
    m  <- models()
    df <- m$data

    ggplot(df, aes(x = log_rel, y = General_2020_11_03)) +
      geom_point(
        aes(color = highlight, alpha = if_else(highlight, 1, 0.3)),
        size  = 0.8,
        show.legend = FALSE
      ) +
      geom_smooth(method = "loess", span = input$span,
                  color = "#2c7bb6", fill = "#abd9e9", linewidth = 1) +
      geom_smooth(method = "lm", se = FALSE,
                  color = "#d7191c", linewidth = 0.8, linetype = "dashed") +
      scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "#e67e22")) +
      scale_alpha_identity() +
      labs(
        x = "log(Relative rainfall + 1)",
        y = "2020 general election turnout share",
        caption = sprintf(
          "OLS slope: %.4f",
          coef(m$reduced_form)["log_rel"]
        )
      ) +
      theme_minimal(base_size = 13)
  })

  # ---------------------------------------------------------------------------
  # Live IV statistics
  # ---------------------------------------------------------------------------
  output$iv_stats <- renderTable({
    m    <- models()
    fs   <- m$first_stage
    rf   <- m$reduced_form

    fs_coef <- coef(fs)["log_rel"]
    rf_coef <- coef(rf)["log_rel"]
    iv_ratio <- rf_coef / fs_coef
    f_stat   <- summary(fs)$fstatistic[1]

    tibble(
      Statistic = c(
        "First-stage F-stat",
        "First-stage coef (rainfall)",
        "Reduced-form coef (rainfall)",
        "Implied IV ratio"
      ),
      Value = round(c(f_stat, fs_coef, rf_coef, iv_ratio), 4)
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")
}

# ---------------------------------------------------------------------------
shinyApp(ui, server)
