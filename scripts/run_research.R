#!/usr/bin/env Rscript
# Run the full research chain: validate, build signals, backtest, persist.
# Usage:
#   Rscript scripts/run_research.R [--as-of DATE] [--db PATH]
#                                  [--results results] [--params config/params.yml]
#                                  [--help]

args <- commandArgs(trailingOnly = TRUE)
if (file.exists("R/utils.R")) source("R/utils.R")
maybe_help(
  args,
  "Usage: Rscript scripts/run_research.R [--as-of DATE] [--db PATH] [--results DIR]",
  paste("Validates the vintage store, builds carry/momentum/macro/composite signals,",
        "runs cost-aware backtests and writes results/metrics.csv, equity_curve.csv,",
        "ranked_latest.csv, cost_sensitivity.csv and run_summary.json.",
        "Exits non-zero if the publication gate finds a FAIL.", sep = "\n  ")
)

opts <- parse_args(args)
root <- project_root()
setwd(root)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

params_path <- if (!is.null(opts$params)) opts$params else "config/params.yml"
params <- load_params(params_path)
as_of_date <- if (!is.null(opts[["as-of"]])) as.Date(opts[["as-of"]]) else
  as.Date(params$as_of)
db_path <- if (!is.null(opts$db)) opts$db else "data/macrosignals.sqlite"
results_dir <- if (!is.null(opts$results)) opts$results else "results"
ensure_dir(results_dir)

if (!file.exists(db_path)) {
  stop("Database not found: ", db_path,
       ". Run `Rscript scripts/ingest.R` first.", call. = FALSE)
}

hash <- params_hash(params_path)
run_id <- make_run_id(as_of_date, hash)

con <- db_connect(db_path)
on.exit(DBI::dbDisconnect(con), add = TRUE)

started <- Sys.time()

# ---------------------------------------------------------------------------
# 1. Publication gate
# ---------------------------------------------------------------------------
validation <- validate_universe(con, as_of_date, params, run_id = run_id)
cat("Validation:", nrow(validation$summary), "checks run,",
    sum(validation$summary$n_flagged > 0), "fired,",
    validation$n_fail, "FAIL rows.\n")
publish_gate(validation)

# A CSV mirror of the validation log lets the Excel/VBA front end reload the
# Validation sheet without a SQLite driver on the desktop.
validation_log <- db_query(
  con,
  "SELECT run_id, checked_at, series_id, check_name, severity, message
     FROM validation_log WHERE run_id = ? ORDER BY check_name, series_id",
  list(run_id)
)
ensure_dir(results_dir)
readr::write_csv(validation_log, file.path(results_dir, "validation_log.csv"))

# ---------------------------------------------------------------------------
# 2. Data and signals
# ---------------------------------------------------------------------------
prices <- db_query(con, "SELECT instrument_id, obs_date, px_last, carry_bp
                           FROM prices ORDER BY instrument_id, obs_date")
prices$obs_date <- as.Date(prices$obs_date)
instrument_meta <- db_query(con, "SELECT * FROM instrument_meta
                                  ORDER BY instrument_id")

signals <- build_signals(con, prices, instrument_meta, params, as_of_date)
signal_names <- c("carry", "momentum", "macro_surprise", "composite")
all_signals <- do.call(rbind, signals[signal_names])
row.names(all_signals) <- NULL

cat("Rebalance dates:", length(signals$dates),
    "| signals:", length(signal_names),
    "| signal rows:", nrow(all_signals), "\n")

# Persist signal values for the run (idempotent for the deterministic run id).
invisible(DBI::dbExecute(con, "DELETE FROM signal_values WHERE run_id = ?",
               params = list(run_id)))
db_write(con, "signal_values", data.frame(
  run_id = run_id,
  as_of_date = all_signals$as_of_date,
  instrument_id = all_signals$instrument_id,
  signal_name = all_signals$signal_name,
  raw_value = all_signals$raw_value,
  zscore = all_signals$zscore,
  rank = all_signals$rank,
  stringsAsFactors = FALSE
))

# ---------------------------------------------------------------------------
# 3. Backtests and diagnostics
# ---------------------------------------------------------------------------
backtests <- run_all_backtests(signals, prices, params, instrument_meta,
                               cost_bp = params$cost_bp)
metrics_df <- backtests$metrics
metrics_df$run_id <- run_id
metrics_df <- metrics_df[, c("run_id", setdiff(names(metrics_df), "run_id"))]
readr::write_csv(metrics_df, file.path(results_dir, "metrics.csv"))

curves <- backtests$curves
curves$run_id <- run_id
readr::write_csv(curves, file.path(results_dir, "equity_curve.csv"))

# Latest ranked cross-section.
latest_date <- max(all_signals$as_of_date)
latest <- all_signals[all_signals$as_of_date == latest_date, ]
wide <- tidyr::pivot_wider(
  latest[, c("instrument_id", "signal_name", "zscore")],
  id_cols = "instrument_id", names_from = "signal_name",
  values_from = "zscore"
)
wide <- merge(wide, instrument_meta[, c("instrument_id", "name", "asset_class")],
              by = "instrument_id", all.x = TRUE)
wide$rank <- rank_signal(wide$composite)
wide <- wide[order(wide$rank), ]
ranked <- data.frame(
  as_of_date = latest_date,
  instrument_id = wide$instrument_id,
  name = wide$name,
  asset_class = wide$asset_class,
  carry_z = wide$carry,
  momentum_z = wide$momentum,
  macro_surprise_z = wide$macro_surprise,
  composite_z = wide$composite,
  rank = wide$rank,
  stringsAsFactors = FALSE
)
readr::write_csv(ranked, file.path(results_dir, "ranked_latest.csv"))

# Cost sensitivity.
sensitivity <- cost_sensitivity(signals, prices, params, instrument_meta)
readr::write_csv(sensitivity, file.path(results_dir, "cost_sensitivity.csv"))

# ---------------------------------------------------------------------------
# 4. Persist metrics and run summary
# ---------------------------------------------------------------------------
invisible(DBI::dbExecute(con, "DELETE FROM backtest_metrics WHERE run_id = ?",
               params = list(run_id)))
metric_long <- do.call(rbind, lapply(seq_len(nrow(metrics_df)), function(i) {
  row <- metrics_df[i, ]
  basis <- row$basis
  vals <- c(
    ann_return = row$ann_return, ann_vol = row$ann_vol, sharpe = row$sharpe,
    max_dd = row$max_dd, calmar = row$calmar, hit_rate = row$hit_rate,
    ann_turnover = row$ann_turnover, avg_holding_days = row$avg_holding_days
  )
  data.frame(
    run_id = run_id,
    signal_name = row$signal_name,
    metric = paste0(names(vals), "_", basis),
    value = as.numeric(vals),
    stringsAsFactors = FALSE
  )
}))
db_write(con, "backtest_metrics", metric_long)

net <- metrics_df[metrics_df$basis == "net", ]
gross <- metrics_df[metrics_df$basis == "gross", ]
headline <- function(rows) {
  lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    list(
      ann_return = r$ann_return, ann_vol = r$ann_vol, sharpe = r$sharpe,
      max_dd = r$max_dd,
      dd_start = as.character(r$dd_start), dd_end = as.character(r$dd_end),
      calmar = r$calmar, hit_rate = r$hit_rate,
      ann_turnover = r$ann_turnover,
      avg_holding_days = r$avg_holding_days
    )
  })
}
net_headline <- headline(net)
gross_headline <- headline(gross)
names(net_headline) <- net$signal_name
names(gross_headline) <- gross$signal_name

counts <- db_table_counts(con)
summary_list <- list(
  run_id = run_id,
  as_of = as.character(as_of_date),
  params_hash = hash,
  params_file = params_path,
  seed = params$seed,
  r_version = R.version.string,
  platform = R.version$platform,
  counts = list(
    series = db_query(con, "SELECT COUNT(*) AS n FROM series_meta")$n[[1]],
    instruments = nrow(instrument_meta),
    observations = db_query(con, "SELECT COUNT(*) AS n FROM observations")$n[[1]],
    prices = nrow(prices),
    rebalances = length(signals$dates),
    signal_rows = nrow(all_signals),
    validation_rows = validation$n_fail + validation$n_warn + validation$n_info
  ),
  coverage = list(
    start = as.character(min(prices$obs_date)),
    end = as.character(max(prices$obs_date)),
    asset_classes = sort(unique(instrument_meta$asset_class))
  ),
  validation = list(
    checks_run = nrow(validation$summary),
    checks_fired = sum(validation$summary$n_flagged > 0),
    fail_rows = validation$n_fail,
    warn_rows = validation$n_warn,
    info_rows = validation$n_info,
    ok = validation$ok
  ),
  cost_bp = params$cost_bp,
  rebalance = params$rebalance,
  net = net_headline,
  gross = gross_headline,
  files = c("results/metrics.csv", "results/equity_curve.csv",
            "results/ranked_latest.csv", "results/cost_sensitivity.csv",
            "reports/out/research_report.html", "excel/MacroSignals_Desk.xlsx")
)
write_json(summary_list, file.path(results_dir, "run_summary.json"))

# ---------------------------------------------------------------------------
# 5. Console report, saved verbatim
# ---------------------------------------------------------------------------
console <- capture.output({
  cat("MacroSignals research run\n")
  cat("  run id     :", run_id, "\n")
  cat("  as of      :", format(as_of_date, "%Y-%m-%d"), "\n")
  cat("  params hash:", hash, "\n")
  cat("  seed       :", params$seed, "\n\n")
  cat("Validation summary\n")
  print(validation$summary, row.names = FALSE)
  cat("\nNet-of-cost performance (cost =", params$cost_bp, "bp)\n")
  show_cols <- c("signal_name", "ann_return", "ann_vol", "sharpe", "max_dd",
                 "dd_start", "dd_end", "calmar", "hit_rate",
                 "ann_turnover", "avg_holding_days")
  print(as.data.frame(net[, show_cols]), digits = 4, row.names = FALSE)
  cat("\nGross performance\n")
  print(as.data.frame(gross[, c("signal_name", "ann_return", "ann_vol",
                                "sharpe", "max_dd", "ann_turnover")]),
        digits = 4, row.names = FALSE)
  cat("\nCost sensitivity (net Sharpe)\n")
  sens <- tidyr::pivot_wider(
    sensitivity[, c("signal_name", "cost_bp", "sharpe")],
    id_cols = "signal_name", names_from = "cost_bp",
    values_from = "sharpe", names_prefix = "cost_"
  )
  print(sens, digits = 4, row.names = FALSE)
  cat("\nWrote results to", results_dir, "\n")
})
writeLines(console, file.path(results_dir, "run_console.txt"))
cat(console, sep = "\n")

elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
cat("\nElapsed:", sprintf("%.1f", elapsed), "seconds\n")
