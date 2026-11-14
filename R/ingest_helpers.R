# Shared ingestion helpers, used by both the CLI and the validation demo.

read_fixture_dir <- function(dir = "inst/fixtures") {
  needed <- c("series_meta.csv", "instrument_meta.csv",
              "observations.csv", "prices.csv")
  missing <- needed[!file.exists(file.path(dir, needed))]
  if (length(missing) > 0) {
    stop("Missing fixture file(s) in ", dir, ": ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  read <- function(name) {
    readr::read_csv(file.path(dir, name), show_col_types = FALSE)
  }
  list(
    series_meta = read("series_meta.csv"),
    instrument_meta = read("instrument_meta.csv"),
    observations = read("observations.csv"),
    prices = read("prices.csv")
  )
}

ingest_frames <- function(con, frames, source = "fixture") {
  counts <- c(
    series_meta = db_write(con, "series_meta", frames$series_meta),
    instrument_meta = db_write(con, "instrument_meta", frames$instrument_meta),
    observations = db_write(con, "observations", frames$observations),
    prices = db_write(con, "prices", frames$prices)
  )
  attr(counts, "source") <- source
  counts
}

#' Rebuild a SQLite store from a fixture directory.
build_fixture_db <- function(db_path, fixtures_dir = "inst/fixtures",
                             schema_path = "sql/schema.sql") {
  if (file.exists(db_path)) unlink(db_path)
  ensure_dir(dirname(db_path))
  frames <- read_fixture_dir(fixtures_dir)
  con <- db_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  db_init(con, schema_path)
  ingest_frames(con, frames)
  invisible(db_path)
}
