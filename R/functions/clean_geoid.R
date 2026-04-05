# R/functions/clean_geoid.R
# Parse Census GEOID and NAME into component geography variables.
# Replaces the duplicated separate() + mutate() chain from the original notebook.

clean_geoid <- function(df) {
  df |>
    separate(
      NAME,
      into  = c("blkgrp_name", "tract_name", "county_name", "state_name"),
      sep   = ",",
      extra = "merge"
    ) |>
    mutate(across(ends_with("_name"), str_trim)) |>
    rename(blkgrpid = GEOID) |>
    mutate(
      state   = str_sub(blkgrpid, 1, 2),
      county  = str_sub(blkgrpid, 3, 5),
      tract   = str_sub(blkgrpid, 6, 11),
      blkgrp  = str_sub(blkgrpid, 12, 12),
      tractid = str_c(state, county, tract)
    )
}
