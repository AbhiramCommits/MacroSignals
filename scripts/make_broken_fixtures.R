#!/usr/bin/env Rscript
# Create inst/fixtures/broken/ by corrupting the clean fixture set.
#
# The corruption is deliberately unambiguous, so every validation check has a
# visible failure:
#   * US_PMI history is truncated in mid-2024 (stale_series)
#   * three monthly observations are removed in 2021 (gap_in_history)
#   * the final 24 US_PMI values are pinned to 50.0 (constant_series)
#   * one US_PMI print is set to 999 (zscore_outlier)
#   * a vintage dated 2027 is appended (future_vintage)
#   * a phantom US_UNRATE metadata row has no observations (missing_series)
#
# Usage: Rscript scripts/make_broken_fixtures.R [--help]

args <- commandArgs(trailingOnly = TRUE)
if (file.exists("R/utils.R")) source("R/utils.R")
maybe_help(
  args,
  "Usage: Rscript scripts/make_broken_fixtures.R",
  "Writes inst/fixtures/broken/*.csv from inst/fixtures/*.csv for gate demos."
)

root <- project_root()
setwd(root)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

src <- read_fixture_dir("inst/fixtures")
out_dir <- "inst/fixtures/broken"
ensure_dir(out_dir)

obs <- src$observations
meta <- src$series_meta

# --- stale + constant: keep vintages through a 2024-06 reference month and
# pin the last 24 observations to a single value.
pmi <- obs[obs$series_id == "US_PMI", ]
pmi <- pmi[pmi$obs_date <= as.Date("2024-06-30"), ]
last_24 <- utils::tail(sort(unique(pmi$obs_date)), 24)
pmi$value[pmi$obs_date %in% last_24] <- 50.0

# --- gap: remove three consecutive monthly reference periods in 2021.
pmi <- pmi[!pmi$obs_date %in% as.Date(c("2021-05-31", "2021-06-30",
                                         "2021-07-30")), ]

# --- outlier: one print in 2020 is corrupted.
pmi$value[pmi$obs_date == as.Date("2020-05-29")] <- 999

# --- future vintage: a row that no point-in-time query may ever see.
future_row <- data.frame(
  series_id = "US_PMI",
  obs_date = as.Date("2024-06-28"),
  vintage_date = as.Date("2027-01-15"),
  value = 50.0,
  revision_no = 9L,
  stringsAsFactors = FALSE
)
pmi <- rbind(pmi, future_row)

obs <- rbind(obs[obs$series_id != "US_PMI", ], pmi)

# --- missing series: metadata for a series that was never loaded.
phantom <- data.frame(
  series_id = "US_UNRATE",
  name = "US unemployment rate (phantom metadata row)",
  asset_class = "Macro",
  country = "US",
  frequency = "monthly",
  unit = "percent",
  source = "broken-fixture",
  publish_lag_days = 5L,
  stringsAsFactors = FALSE
)
meta <- rbind(meta, phantom)

obs <- obs[order(obs$series_id, obs$obs_date, obs$vintage_date), ]
row.names(obs) <- NULL

readr::write_csv(meta, file.path(out_dir, "series_meta.csv"))
readr::write_csv(src$instrument_meta, file.path(out_dir, "instrument_meta.csv"))
readr::write_csv(obs, file.path(out_dir, "observations.csv"))
readr::write_csv(src$prices, file.path(out_dir, "prices.csv"))

cat("Wrote broken fixture set to", out_dir, "\n")
cat("  US_PMI visible rows:", sum(obs$series_id == "US_PMI" & obs$vintage_date <= as.Date("2026-11-13")), "\n")
cat("  last visible US_PMI obs:", format(max(obs$obs_date[obs$series_id == "US_PMI" &
                                                             obs$vintage_date <= as.Date("2026-11-13")]), "%Y-%m-%d"), "\n")
cat("  phantom series:", phantom$series_id, "\n")
