# Point-in-time (vintage-aware) reads.
#
# The core promise of the toolkit: a query made on date D may only see
# observation values that were actually published on or before D. For every
# (series, observation date) pair we take the most recent vintage that is
# visible at D.

empty_pit <- function() {
  data.frame(
    series_id = character(0),
    obs_date = as.Date(character(0)),
    vintage_date = as.Date(character(0)),
    value = numeric(0),
    revision_no = integer(0),
    stringsAsFactors = FALSE
  )
}

coerce_pit_dates <- function(df) {
  if (nrow(df) == 0) return(empty_pit())
  df$obs_date <- as.Date(df$obs_date)
  df$vintage_date <- as.Date(df$vintage_date)
  df$revision_no <- as.integer(df$revision_no)
  df
}

#' Return the vintage table visible at a point in time.
#'
#' @param con DBI connection to the MacroSignals store.
#' @param series_ids Character vector of series to read.
#' @param as_of_date Decision date. Only vintages dated on or before it count.
#' @return data.frame with one row per series and observation date, carrying
#'   the latest visible vintage.
as_of <- function(con, series_ids, as_of_date) {
  if (length(series_ids) == 0) return(empty_pit())
  as_of_date <- as.character(as.Date(as_of_date))
  placeholders <- paste(rep("?", length(series_ids)), collapse = ", ")
  sql <- sprintf(
    "SELECT o.series_id, o.obs_date, o.vintage_date, o.value, o.revision_no
       FROM observations o
       JOIN (
         SELECT series_id, obs_date, MAX(vintage_date) AS vintage_date
           FROM observations
          WHERE series_id IN (%s)
            AND vintage_date <= ?
          GROUP BY series_id, obs_date
       ) latest
         ON o.series_id = latest.series_id
        AND o.obs_date = latest.obs_date
        AND o.vintage_date = latest.vintage_date
      ORDER BY o.series_id, o.obs_date",
    placeholders
  )
  params <- c(as.list(series_ids), list(as_of_date))
  coerce_pit_dates(db_query(con, sql, params))
}

#' Latest visible vintage for every series in the store.
latest_vintage_table <- function(con, as_of_date) {
  as_of_date <- as.character(as.Date(as_of_date))
  sql <- "SELECT o.series_id, o.obs_date, o.vintage_date, o.value, o.revision_no
            FROM observations o
            JOIN (
              SELECT series_id, obs_date, MAX(vintage_date) AS vintage_date
                FROM observations
               WHERE vintage_date <= ?
               GROUP BY series_id, obs_date
            ) latest
              ON o.series_id = latest.series_id
             AND o.obs_date = latest.obs_date
             AND o.vintage_date = latest.vintage_date
           ORDER BY o.series_id, o.obs_date"
  coerce_pit_dates(db_query(con, sql, list(as_of_date)))
}

#' Full revision history for one series, newest vintage last.
revision_history <- function(con, series_id) {
  sql <- "SELECT series_id, obs_date, vintage_date, value, revision_no
            FROM observations
           WHERE series_id = ?
           ORDER BY obs_date, vintage_date"
  out <- db_query(con, sql, list(series_id))
  coerce_pit_dates(out)
}
