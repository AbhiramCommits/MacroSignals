#!/usr/bin/env Rscript
# Build the SQLite vintage store from fixtures (default) or live FRED.
# Usage:
#   Rscript scripts/ingest.R [--source fixture|fred] [--db PATH] [--fixtures DIR]
#   Rscript scripts/ingest.R --help

args <- commandArgs(trailingOnly = TRUE)
if (file.exists("R/utils.R")) source("R/utils.R")
maybe_help(
  args,
  "Usage: Rscript scripts/ingest.R [--source fixture|fred] [--db PATH] [--fixtures DIR]",
  paste("fixture  deterministic committed CSVs in inst/fixtures (default)",
        "fred     live FRED macro data; requires MACROSIGNALS_SOURCE=fred and FRED_API_KEY",
        "The database is rebuilt from scratch on every run.", sep = "\n  ")
)

opts <- parse_args(args)
root <- project_root()
setwd(root)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

source_mode <- if (!is.null(opts$source)) opts$source else
  Sys.getenv("MACROSIGNALS_SOURCE", unset = "fixture")
if (!source_mode %in% c("fixture", "fred")) {
  stop("--source must be 'fixture' or 'fred'.", call. = FALSE)
}
if (source_mode == "fred" && !nzchar(Sys.getenv("FRED_API_KEY"))) {
  stop("MACROSIGNALS_SOURCE=fred requires FRED_API_KEY.", call. = FALSE)
}

db_path <- if (!is.null(opts$db)) opts$db else "data/macrosignals.sqlite"
fixtures_dir <- if (!is.null(opts$fixtures)) opts$fixtures else "inst/fixtures"
params <- load_params("config/params.yml")

if (file.exists(db_path)) {
  unlink(db_path)
  cat("Removed existing database:", db_path, "\n")
}
ensure_dir(dirname(db_path))

frames <- read_fixture_dir(fixtures_dir)
series_meta <- frames$series_meta
instrument_meta <- frames$instrument_meta
prices <- frames$prices
observations <- frames$observations

if (source_mode == "fred") {
  cat("Fetching live FRED macro releases (opt-in path)...\n")
  fred_map <- data.frame(
    series_id = c("US_CPI", "US_PAYROLLS", "US_PMI",
                  "EA_CPI", "EA_PMI", "JP_CPI"),
    source_id = c("CPIAUCSL", "PAYEMS", "NAPM",
                  "CP0000EZ19M086NEST", "BSCICP03EZM665S", "JPNCPIALLMINMEI"),
    publish_lag_days = c(12L, 5L, 2L, 15L, 3L, 18L),
    stringsAsFactors = FALSE
  )
  observations <- fetch_fred_universe(
    fred_map,
    start = as.character(params$start_date),
    end = as.character(params$as_of)
  )
  observations <- observations[observations$vintage_date <= as.character(params$as_of), ]

  # The FRED release timestamps are modelled at a fixed lag; retain the
  # version-controlled metadata and the fixture market panel.
  series_meta <- series_meta[series_meta$series_id %in% fred_map$series_id, ]
}

con <- db_connect(db_path)
on.exit(DBI::dbDisconnect(con), add = TRUE)
db_init(con, "sql/schema.sql")

counts_written <- ingest_frames(con, list(
  series_meta = series_meta,
  instrument_meta = instrument_meta,
  observations = observations,
  prices = prices
), source = source_mode)

counts <- db_table_counts(con)
cat("\nIngested source:", source_mode, "->", db_path, "\n")
print(counts, row.names = FALSE)

required_nonzero <- c("series_meta", "observations", "prices")
bad <- counts[counts$table %in% required_nonzero & counts$rows == 0, ]
if (nrow(bad) > 0) {
  stop("Ingestion produced zero rows for: ",
       paste(bad$table, collapse = ", "), call. = FALSE)
}

cat("\nDate range: ", format(min(prices$obs_date), "%Y-%m-%d"), " to ",
    format(max(prices$obs_date), "%Y-%m-%d"), "\n", sep = "")
cat("Instruments:", nrow(instrument_meta),
    "| macro series:", nrow(series_meta),
    "| observations:", nrow(observations),
    "| prices:", nrow(prices), "\n")
