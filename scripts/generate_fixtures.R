#!/usr/bin/env Rscript
# Regenerate the deterministic fixture universe.
# Usage: Rscript scripts/generate_fixtures.R [--seed N] [--help]

args <- commandArgs(trailingOnly = TRUE)
if (file.exists("R/utils.R")) source("R/utils.R")
maybe_help(
  args,
  "Usage: Rscript scripts/generate_fixtures.R [--seed N] [--out DIR]",
  paste("Regenerates inst/fixtures/*.csv from the seeded simulation.",
        "The committed fixtures use seed 20261114 and dates 2018-11-01..2026-11-13.")
)

opts <- parse_args(args)
root <- project_root()
setwd(root)

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

seed <- if (!is.null(opts$seed)) as.integer(opts$seed) else 20261114
out <- if (!is.null(opts$out)) opts$out else "inst/fixtures"

universe <- generate_universe(seed = seed)
write_fixtures(universe, out)

cat("Wrote fixtures to", out, "\n")
cat("  business days :", universe$coverage$business_days, "\n")
cat("  instruments   :", universe$coverage$instruments, "\n")
cat("  macro series  :", universe$coverage$macro_series, "\n")
cat("  observations  :", universe$coverage$observation_rows, "\n")
cat("  prices        :", universe$coverage$price_rows, "\n")
