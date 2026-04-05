# exploratory.R
# Quick look at the raw data: what states are available and how do they compare
# on the key variables for the IV analysis?
# Run interactively: source("R/exploratory.R") or open and run in RStudio

library(tidyverse)
library(here)

# ── Load raw data ──────────────────────────────────────────────────────────────

# Block-group-level voter file data (29 states with L2 coverage)
bg <- read_csv(here("data/raw/bg_data.csv"), show_col_types = FALSE)

# National protest/rainfall data (all US block groups)
prot <- readRDS(here("data/raw/bg1.rds"))

# FIPS → state name lookup
fips_lookup <- tibble(
  state = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,
            24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
            42,44,45,46,47,48,49,50,51,53,54,55,56),
  state_name = c("Alabama","Alaska","Arizona","Arkansas","California",
                 "Colorado","Connecticut","Delaware","DC","Florida",
                 "Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa",
                 "Kansas","Kentucky","Louisiana","Maine","Maryland",
                 "Massachusetts","Michigan","Minnesota","Mississippi",
                 "Missouri","Montana","Nebraska","Nevada","New Hampshire",
                 "New Jersey","New Mexico","New York","North Carolina",
                 "North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
                 "Rhode Island","South Carolina","South Dakota","Tennessee",
                 "Texas","Utah","Vermont","Virginia","Washington",
                 "West Virginia","Wisconsin","Wyoming")
)

# ── Merge ──────────────────────────────────────────────────────────────────────

merged <- bg |>
  mutate(state = as.integer(state)) |>
  left_join(fips_lookup, by = "state") |>
  # clean Inf values (same as pipeline)
  mutate(across(starts_with("General"), ~ na_if(.x, Inf))) |>
  mutate(across(c(dist, rel, pop_dens, nh_black), ~ na_if(.x, Inf))) |>
  # log-transform the instrument and treatment to match the pipeline
  mutate(
    log_dist = log(dist + 1),
    log_rel  = log(rel  + 1)
  )

# ── State-level summary ────────────────────────────────────────────────────────

state_summary <- merged |>
  group_by(state, state_name) |>
  summarise(
    n_block_groups    = n(),
    # Instrument: rainfall deviation (higher = more rain on protest days)
    mean_rainfall_dev = mean(rel, na.rm = TRUE),
    sd_rainfall_dev   = sd(rel, na.rm = TRUE),
    # Treatment: distance to nearest protest (miles)
    mean_dist_mi      = mean(dist, na.rm = TRUE),
    pct_within_5mi    = mean(dist < 5, na.rm = TRUE) * 100,
    # Outcome: 2020 turnout
    mean_turnout_2020 = mean(General_2020_11_03, na.rm = TRUE),
    mean_turnout_2018 = mean(General_2018_11_06, na.rm = TRUE),
    # IV relevance: first-stage correlation (instrument ~ treatment)
    iv_corr           = cor(log_rel, log_dist, use = "complete.obs"),
    .groups = "drop"
  ) |>
  arrange(desc(n_block_groups))

# ── Print summary ──────────────────────────────────────────────────────────────

cat("\n=== STATES WITH VOTER FILE COVERAGE ===\n\n")
state_summary |>
  select(state_name, n_block_groups, mean_dist_mi, pct_within_5mi,
         mean_turnout_2020, iv_corr) |>
  mutate(across(where(is.double), ~ round(.x, 3))) |>
  print(n = Inf)

cat("\n=== INSTRUMENT STRENGTH BY STATE ===\n")
cat("(iv_corr = correlation between log(rainfall) and log(distance))\n")
cat("More negative = stronger instrument (rain → less protest → farther from protest)\n\n")

state_summary |>
  select(state_name, n_block_groups, iv_corr, sd_rainfall_dev) |>
  arrange(iv_corr) |>
  mutate(across(where(is.double), ~ round(.x, 3))) |>
  print(n = Inf)

cat("\n=== MISSING DATA CHECK ===\n\n")
merged |>
  group_by(state_name) |>
  summarise(
    pct_missing_turnout = mean(is.na(General_2020_11_03)) * 100,
    pct_missing_dist    = mean(is.na(dist)) * 100,
    pct_missing_rel     = mean(is.na(rel)) * 100,
    .groups = "drop"
  ) |>
  filter(pct_missing_turnout > 0 | pct_missing_dist > 0) |>
  print(n = Inf)

# ── Visualise: turnout distribution by state ───────────────────────────────────

p1 <- merged |>
  filter(!is.na(General_2020_11_03)) |>
  mutate(state_name = fct_reorder(state_name, General_2020_11_03, median)) |>
  ggplot(aes(x = General_2020_11_03, y = state_name)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.size = 0.3) +
  labs(
    title = "2020 General Election Turnout by State",
    x = "Share of registered voters turning out",
    y = NULL
  ) +
  theme_minimal()

print(p1)

# ── Visualise: instrument strength by state ────────────────────────────────────

p2 <- state_summary |>
  mutate(state_name = fct_reorder(state_name, iv_corr)) |>
  ggplot(aes(x = iv_corr, y = state_name, fill = iv_corr < -0.05)) +
  geom_col() +
  scale_fill_manual(values = c("grey70", "steelblue"), guide = "none") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "First-Stage Instrument Strength by State",
    subtitle = "Correlation between log(rainfall deviation) and log(distance to protest)",
    x = "Correlation (more negative = stronger instrument)",
    y = NULL
  ) +
  theme_minimal()

print(p2)

cat("\n=== TO SWITCH STATES IN THE PIPELINE ===\n")
cat("In _targets.R, find the GA_sf target and change:\n")
cat('  filter(state_name == "Georgia")\n')
cat('to e.g.:\n')
cat('  filter(state_name == "Minnesota")\n\n')
