# SQLite access helpers. Every statement that carries user data is executed
# with bound parameters; table and column names are checked against the schema.

#' Connect to the MacroSignals SQLite store.
#'
#' @param path Path to the SQLite file. Parent directories are created.
#' @return An open DBI connection.
db_connect <- function(path = "data/macrosignals.sqlite") {
  path <- normalizePath(path, mustWork = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(RSQLite::SQLite(), path)
}

#' Execute a schema file.
#'
#' Splits the file on semicolons. The schema deliberately contains no triggers
#' or semicolons inside string literals.
db_init <- function(con, schema_path = "sql/schema.sql") {
  if (!file.exists(schema_path)) {
    stop("Schema file not found: ", schema_path, call. = FALSE)
  }
  sql <- paste(readLines(schema_path, warn = FALSE), collapse = "\n")
  statements <- strsplit(sql, ";", fixed = TRUE)[[1]]
  statements <- trimws(statements)
  statements <- statements[nzchar(statements)]
  for (statement in statements) {
    DBI::dbExecute(con, statement)
  }
  invisible(TRUE)
}

#' Validate an identifier against the live database schema.
db_check_name <- function(con, table, columns = NULL) {
  tables <- DBI::dbListTables(con)
  if (!table %in% tables) {
    stop("Unknown table: ", table, call. = FALSE)
  }
  if (!is.null(columns)) {
    fields <- DBI::dbListFields(con, table)
    bad <- setdiff(columns, fields)
    if (length(bad) > 0) {
      stop("Unknown column(s) in ", table, ": ", paste(bad, collapse = ", "),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Append a data frame to a table using one parameterised INSERT per row.
db_write <- function(con, table, df) {
  if (!is.data.frame(df)) {
    stop("db_write() expects a data frame.", call. = FALSE)
  }
  if (nrow(df) == 0) {
    return(invisible(0L))
  }
  db_check_name(con, table, names(df))

  # Dates and timestamps are stored as ISO text so SQL comparisons are
  # lexicographic and unambiguous.
  clean <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(clean)) {
    if (inherits(clean[[nm]], c("Date", "POSIXct", "POSIXlt"))) {
      clean[[nm]] <- format(clean[[nm]], "%Y-%m-%d")
    }
  }

  placeholders <- paste(rep("?", ncol(clean)), collapse = ", ")
  sql <- sprintf("INSERT INTO %s (%s) VALUES (%s)", table,
                 paste(names(clean), collapse = ", "), placeholders)

  DBI::dbWithTransaction(con, {
    for (i in seq_len(nrow(clean))) {
      params <- as.list(clean[i, , drop = FALSE])
      params <- lapply(params, function(x) {
        if (length(x) == 1 && is.na(x)) NA else x[[1]]
      })
      DBI::dbExecute(con, sql, params = unname(params))
    }
  })
  invisible(nrow(clean))
}

#' Run a parameterised read query.
db_query <- function(con, sql, params = list()) {
  if (length(params) == 0) {
    DBI::dbGetQuery(con, sql)
  } else {
    DBI::dbGetQuery(con, sql, params = params)
  }
}

#' Row counts for every user table, used by ingest and the report.
db_table_counts <- function(con) {
  tables <- c("series_meta", "instrument_meta", "observations", "prices",
              "validation_log", "signal_values", "backtest_metrics")
  counts <- vapply(tables, function(tbl) {
    DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", tbl))$n[[1]]
  }, numeric(1))
  data.frame(table = tables, rows = as.integer(counts), row.names = NULL)
}
