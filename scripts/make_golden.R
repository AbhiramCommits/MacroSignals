#!/usr/bin/env Rscript
# Regenerate the golden backtest metrics used by test-backtest.R.
# Usage: Rscript scripts/make_golden.R [--help]

args <- commandArgs(trailingOnly = TRUE)
if (file.exists("R/utils.R")) source("R/utils.R")
maybe_help(
  args,
  "Usage: Rscript scripts/make_golden.R",
  paste("Recomputes tests/testthat/golden/metrics.csv from the deterministic",
        "hand-built case in tests/testthat/helper-golden.R.", sep = "\n  ")
)

root <- project_root()
setwd(root)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
source(file.path("tests", "testthat", "helper-golden.R"))

case <- golden_case()
bt <- run_backtest(case$signal, case$prices, case$params,
                   signal_name = "golden")
m <- metrics(bt, "golden")
ensure_dir(file.path("tests", "testthat", "golden"))
readr::write_csv(m, file.path("tests", "testthat", "golden", "metrics.csv"))

cat("Wrote golden metrics for", nrow(m), "rows:\n")
print(m[, c("basis", "ann_return", "ann_vol", "sharpe", "max_dd",
            "ann_turnover")], digits = 6, row.names = FALSE)
