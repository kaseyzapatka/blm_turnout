# _targets.R
# BLM Turnout pipeline — managed by the {targets} package.
#
# Run the full pipeline:   targets::tar_make()
# Visualize dependencies:  targets::tar_visnetwork()
# Read a cached target:    targets::tar_read(target_name)
# Check status:            targets::tar_outdated()

library(targets)

tar_option_set(
  packages = c(
    "tidyverse", "sf", "tigris",
    "spdep", "spatialreg", "modelsummary",
    "here"
  )
)

# Source all reusable functions from R/functions/
tar_source("R/functions/")

# ---------------------------------------------------------------------------
# State selection — change this one value to switch states
# Use the full state name as it appears in the NAME column of the Census data
# ---------------------------------------------------------------------------

analysis_state      <- "Georgia"
analysis_state_fips <- "GA"   # two-letter FIPS abbreviation for tigris

# ---------------------------------------------------------------------------
# Shared formula components (defined once, referenced in multiple targets)
# ---------------------------------------------------------------------------

past_turnout_vars <- c(
  "General_2018_11_06", "General_2016_11_08",
  "General_2014_11_04", "General_2012_11_06", "General_2010_11_02"
)

turnout_rhs <- paste(past_turnout_vars, collapse = " + ")

# ---------------------------------------------------------------------------
# Pipeline
# ---------------------------------------------------------------------------

list(

  # -------------------------------------------------------------------------
  # 1. Ingest
  # -------------------------------------------------------------------------

  tar_target(
    GA_data_raw,
    read_csv(here("data/raw/bg_data.csv"), show_col_types = FALSE)
  ),

  tar_target(
    pred_data_raw,
    readRDS(here("data/raw/bg1.rds"))
  ),

  # Shapefile: cached to avoid re-downloading on every run.
  # tigris downloads directly from Census TIGER — no API key needed.
  tar_target(
    GA_shapefile,
    block_groups(state = analysis_state_fips, year = 2019, cb = TRUE, class = "sf") |>
      rename(blkgrpid = GEOID)
  ),

  # -------------------------------------------------------------------------
  # 2. Clean
  # -------------------------------------------------------------------------

  tar_target(
    GA_data_clean,
    clean_geoid(GA_data_raw)
  ),

  tar_target(
    pred_data_clean,
    clean_geoid(pred_data_raw)
  ),

  # Georgia block groups merged with shapefile, Inf/NA handled
  tar_target(
    GA_sf,
    {
      GA_data_clean |>
        filter(state_name == analysis_state) |>
        inner_join(GA_shapefile, by = "blkgrpid") |>
        st_as_sf() |>
        mutate(across(starts_with("General"), ~ na_if(.x, Inf))) |>
        mutate(across(starts_with("General"), ~ replace_na(.x, 0))) |>
        mutate(across(c(dist, rel, pop_dens, nh_black), ~ na_if(.x, Inf))) |>
        mutate(across(c(dist, rel, pop_dens, nh_black), ~ replace_na(.x, 0)))
    }
  ),

  tar_target(
    pred_clean,
    pred_data_clean |>
      mutate(across(c(dist, rel, pop_dens, nh_black), ~ na_if(.x, Inf))) |>
      mutate(across(c(dist, rel, pop_dens, nh_black), ~ replace_na(.x, 0))) |>
      select(blkgrpid, dist, rel, pop_dens, nh_black)
  ),

  # Main modeling dataset: GA block groups + IV covariates
  # GA_sf already carries dist, rel, pop_dens, nh_black from bg_data.csv
  tar_target(
    GA_iv,
    GA_sf |>
      mutate(
        log_dist = log(dist + 1),
        log_rel  = log(rel  + 1)
      )
  ),

  # -------------------------------------------------------------------------
  # 3. Spatial weights
  # -------------------------------------------------------------------------

  tar_target(
    ga_weights,
    {
      ga_nb <- poly2nb(GA_iv, queen = TRUE)
      nb2listw(ga_nb, style = "W", zero.policy = TRUE)
    }
  ),

  tar_target(
    moran_results,
    {
      run_moran <- function(x, w, label) {
        res <- moran.test(x, w, zero.policy = TRUE)
        tibble(
          variable = label,
          moran_i  = res$estimate["Moran I statistic"],
          p_value  = res$p.value
        )
      }
      bind_rows(
        run_moran(GA_iv$General_2020_11_03, ga_weights, "2020 Turnout"),
        run_moran(GA_iv$dist,               ga_weights, "Distance from protest"),
        run_moran(GA_iv$rel,                ga_weights, "Relative rainfall")
      )
    }
  ),

  # -------------------------------------------------------------------------
  # 4. Models
  # -------------------------------------------------------------------------

  # OLS Model 1: 2020 turnout ~ 2018 turnout only
  tar_target(
    ols_m1,
    lm(General_2020_11_03 ~ General_2018_11_06, data = GA_iv)
  ),

  # OLS Model 2: all past turnout years
  tar_target(
    ols_m2,
    lm(as.formula(paste("General_2020_11_03 ~", turnout_rhs)), data = GA_iv)
  ),

  # OLS Model 3: all controls + distance (baseline for LM tests)
  tar_target(
    ols_m3,
    lm(as.formula(paste("General_2020_11_03 ~", turnout_rhs, "+ dist")), data = GA_iv)
  ),

  # First stage: distance ~ rainfall + demographics
  tar_target(
    first_stage,
    lm(log_dist ~ log_rel + pop_dens + nh_black, data = GA_iv)
  ),

  # Reduced form: turnout ~ rainfall + controls (intention-to-treat)
  tar_target(
    reduced_form,
    lm(as.formula(paste("General_2020_11_03 ~ log_rel +", turnout_rhs)), data = GA_iv)
  ),

  # 2SLS: dist instrumented by rainfall + demographics
  tar_target(
    iv_2sls,
    ivreg(
      as.formula(paste(
        "General_2020_11_03 ~ dist +", turnout_rhs,
        "| log_rel + pop_dens + nh_black +", turnout_rhs
      )),
      data = GA_iv
    ),
    packages = c("tidyverse", "sf", "ivreg")
  ),

  # Lagrange multiplier tests: determines spatial model specification
  tar_target(
    lm_tests,
    lm.RStests(model = ols_m3, listw = ga_weights, test = "all", zero.policy = TRUE)
  ),

  # Spatial error model (controls for spatially structured unobservables)
  tar_target(
    sp_error,
    errorsarlm(
      as.formula(paste("General_2020_11_03 ~", turnout_rhs, "+ dist")),
      data        = GA_iv,
      listw       = ga_weights,
      zero.policy = TRUE
    )
  ),

  # Spatial IV: first-stage fitted values as treatment in spatial error model
  # Implements Betz, Cook & Hollenbach (2019) spatial 2SLS approximation
  tar_target(
    sp_iv,
    {
      GA_iv_fitted <- GA_iv |> mutate(dist_hat = predict(first_stage, newdata = GA_iv))
      errorsarlm(
        as.formula(paste("General_2020_11_03 ~ dist_hat +", turnout_rhs)),
        data        = GA_iv_fitted,
        listw       = ga_weights,
        zero.policy = TRUE
      )
    }
  ),

  # -------------------------------------------------------------------------
  # 5. Figures
  # -------------------------------------------------------------------------

  tar_target(
    map_turnout,
    mapping(
      data     = GA_iv,
      variable = General_2020_11_03,
      ltitle   = "Turnout share",
      ptitle   = "Georgia — 2020 General Election Turnout",
      stitle   = "Share of registered voters who turned out, by block group"
    )
  ),

  tar_target(
    map_distance,
    mapping(
      data     = GA_iv,
      variable = dist,
      ltitle   = "Miles",
      ptitle   = "Georgia — Distance from Nearest BLM Protest",
      stitle   = "Distance (miles) from block group centroid to nearest 2020 BLM protest"
    )
  ),

  tar_target(
    map_rainfall,
    mapping(
      data     = GA_iv,
      variable = rel,
      ltitle   = "Inches",
      ptitle   = "Georgia — Relative Rainfall During Protest Period",
      stitle   = "Relative rainfall (inches) during study period"
    )
  ),

  tar_target(
    map_resid_ols,
    {
      GA_iv |>
        mutate(resid = residuals(ols_m3)) |>
        mapping(
          variable = resid,
          ltitle   = "Residual",
          ptitle   = "OLS Residuals",
          stitle   = "Model: turnout ~ past turnout + distance"
        )
    }
  ),

  tar_target(
    map_resid_iv,
    {
      GA_iv |>
        mutate(resid = residuals(iv_2sls)) |>
        mapping(
          variable = resid,
          ltitle   = "Residual",
          ptitle   = "2SLS Residuals",
          stitle   = "IV model: turnout ~ dist (instrumented by rainfall)"
        )
    }
  ),

  # -------------------------------------------------------------------------
  # 6. Export for Shiny (geometry-free, portable)
  # -------------------------------------------------------------------------

  tar_target(
    shiny_data,
    {
      dir.create(here("shiny/data"), showWarnings = FALSE, recursive = TRUE)
      out <- GA_iv |>
        st_drop_geometry() |>
        select(blkgrpid, county_name, dist, rel, pop_dens, nh_black,
               log_dist, log_rel, General_2020_11_03, General_2018_11_06)
      saveRDS(out, here("shiny/data/plot_data.rds"))
      here("shiny/data/plot_data.rds")  # return path so targets tracks the file
    },
    format = "file"
  )

)
# Quarto rendering is a separate step — run after tar_make() completes:
#   quarto render          (from terminal)
#   quarto::quarto_render() (from R)
