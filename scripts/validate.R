#!/usr/bin/env Rscript
# Run the point-in-time data-quality gate.
# Usage:
#   Rscript scripts/validate.R [--db PATH] [--as-of DATE] [--fixtures DIR]
#                              [--out results/validation_summary.csv] [--help]
#
# Exits 0 when no FAIL is present, 1 otherwise.

args <- commandArgs(trailingOnly = TRUE)
if (file.exists("R/utils.R")) source("R/utils.R")
maybe_help(
  args,
  "Usage: Rscript scripts/validate.R [--db PATH] [--as-of DATE] [--fixtures DIR] [--out CSV]",
  paste("Runs six named checks against the vintage store.",
        "--fixtures DIR rebuilds a scratch database from that fixture set",
        "(used to demonstrate the gate on inst/fixtures/broken).",
        "Exit status is 0 on pass and 1 when any FAIL fires.", sep = "\n  ")
)

opts <- parse_args(args)
root <- project_root()
setwd(root)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

params <- load_params("config/params.yml")
as_of_date <- if (!is.null(opts[["as-of"]])) as.Date(opts[["as-of"]]) else
  as.Date(params$as_of)
db_path <- if (!is.null(opts$db)) opts$db else "data/macrosignals.sqlite"

if (!is.null(opts$fixtures)) {
  db_path <- if (!is.null(opts$db)) opts$db else "data/validation_scratch.sqlite"
  cat("Rebuilding scratch database from", opts$fixtures, "...\n")
  build_fixture_db(db_path, opts$fixtures)
}

if (!file.exists(db_path)) {
  stop("Database not found: ", db_path,
       ". Run `Rscript scripts/ingest.R` first.", call. = FALSE)
}

con <- db_connect(db_path)
on.exit(DBI::dbDisconnect(con), add = TRUE)

hash <- params_hash("config/params.yml")
run_id <- if (!is.null(opts[["run-id"]])) opts[["run-id"]] else
  make_run_id(as_of_date, hash)

result <- validate_universe(con, as_of_date, params, run_id = run_id)

cat("\nMacroSignals validation gate\n")
cat("  as of      :", format(as_of_date, "%Y-%m-%d"), "\n")
cat("  run id     :", result$run_id, "\n")
cat("  database   :", db_path, "\n\n")

print(result$summary, row.names = FALSE)

run_checks <- nrow(result$summary)
fired <- sum(result$summary$n_flagged > 0)
cat("\nChecks run:", run_checks,
    "| checks fired:", fired,
    "| FAIL rows:", result$n_fail,
    "| WARN rows:", result$n_warn,
    "| INFO rows:", result$n_info, "\n")

if (!is.null(opts$out)) {
  ensure_dir(dirname(opts$out))
  readr::write_csv(result$summary, opts$out)
  cat("Wrote summary to", opts$out, "\n")
}

if (!result$ok) {
  cat("\nPUBLICATION BLOCKED by series:",
      paste(result$blocked_series, collapse = ", "), "\n")
  fails <- result$details[result$details$severity == "fail", ]
  for (i in seq_len(nrow(fails))) {
    cat("  FAIL", fails$series_id[[i]], "|", fails$check_name[[i]], "|",
        fails$message[[i]], "\n")
  }
  quit(status = 1)
}

cat("\nPublication gate passed: no FAIL checks.\n")
