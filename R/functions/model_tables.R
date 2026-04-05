# R/functions/model_tables.R
# modelsummary wrappers for publication-quality regression tables.

# Default covariate labels — extend as needed.
coef_labels <- c(
  "General_2018_11_06" = "Turnout 2018",
  "General_2016_11_08" = "Turnout 2016",
  "General_2014_11_04" = "Turnout 2014",
  "General_2012_11_06" = "Turnout 2012",
  "General_2010_11_02" = "Turnout 2010",
  "dist"               = "Distance from protest (mi)",
  "log(dist + 1)"      = "log(Distance + 1)",
  "log(rel + 1)"       = "log(Relative rainfall + 1)",
  "pop_dens"           = "Population density",
  "nh_black"           = "% Non-Hispanic Black",
  "(Intercept)"        = "Intercept"
)

# Standard model table: pass a named list of models.
model_table <- function(models, output = "html", ...) {
  modelsummary(
    models,
    coef_map   = coef_labels,
    stars      = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_map    = c("nobs", "r.squared", "adj.r.squared", "rmse"),
    fmt        = 3,
    output     = output,
    ...
  )
}

# First-stage table with F-statistic added as a custom row.
first_stage_table <- function(fs_model, output = "html") {
  fstat <- summary(fs_model)$fstatistic
  f_val <- round(fstat[1], 2)

  modelsummary(
    list("First Stage" = fs_model),
    coef_map  = coef_labels,
    stars     = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_map   = c("nobs", "r.squared"),
    add_rows  = data.frame(term = "F-statistic", `First Stage` = f_val),
    fmt       = 3,
    output    = output
  )
}
