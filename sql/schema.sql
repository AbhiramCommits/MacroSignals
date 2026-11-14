-- MacroSignals SQLite schema.
-- The observations table is a vintage store: a single economic observation can
-- have several rows, one per vintage in which it was published.

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS series_meta (
    series_id        TEXT PRIMARY KEY,
    name             TEXT NOT NULL,
    asset_class      TEXT NOT NULL,
    country          TEXT NOT NULL,
    frequency        TEXT NOT NULL,
    unit             TEXT NOT NULL,
    source           TEXT NOT NULL,
    publish_lag_days INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS instrument_meta (
    instrument_id TEXT PRIMARY KEY,
    name          TEXT NOT NULL,
    asset_class   TEXT NOT NULL,
    country       TEXT NOT NULL,
    quote_ccy     TEXT NOT NULL,
    return_sign   INTEGER NOT NULL,
    us_weight     REAL NOT NULL DEFAULT 0,
    ea_weight     REAL NOT NULL DEFAULT 0,
    jp_weight     REAL NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS observations (
    series_id   TEXT NOT NULL,
    obs_date    TEXT NOT NULL,
    vintage_date TEXT NOT NULL,
    value       REAL NOT NULL,
    revision_no INTEGER NOT NULL,
    PRIMARY KEY (series_id, obs_date, vintage_date),
    FOREIGN KEY (series_id) REFERENCES series_meta (series_id)
);

CREATE TABLE IF NOT EXISTS prices (
    instrument_id TEXT NOT NULL,
    obs_date      TEXT NOT NULL,
    px_last       REAL NOT NULL,
    carry_bp      REAL NOT NULL,
    PRIMARY KEY (instrument_id, obs_date),
    FOREIGN KEY (instrument_id) REFERENCES instrument_meta (instrument_id)
);

CREATE TABLE IF NOT EXISTS validation_log (
    run_id     TEXT NOT NULL,
    checked_at TEXT NOT NULL,
    series_id  TEXT NOT NULL,
    check_name TEXT NOT NULL,
    severity   TEXT NOT NULL,
    message    TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS signal_values (
    run_id        TEXT NOT NULL,
    as_of_date    TEXT NOT NULL,
    instrument_id TEXT NOT NULL,
    signal_name   TEXT NOT NULL,
    raw_value     REAL,
    zscore        REAL,
    rank          INTEGER
);

CREATE TABLE IF NOT EXISTS backtest_metrics (
    run_id      TEXT NOT NULL,
    signal_name TEXT NOT NULL,
    metric      TEXT NOT NULL,
    value       REAL
);

CREATE INDEX IF NOT EXISTS idx_obs_series_vintage
    ON observations (series_id, vintage_date);
CREATE INDEX IF NOT EXISTS idx_obs_series_date
    ON observations (series_id, obs_date);
CREATE INDEX IF NOT EXISTS idx_prices_date
    ON prices (obs_date);
CREATE INDEX IF NOT EXISTS idx_validation_run
    ON validation_log (run_id);
CREATE INDEX IF NOT EXISTS idx_signals_run
    ON signal_values (run_id);
CREATE INDEX IF NOT EXISTS idx_metrics_run
    ON backtest_metrics (run_id);
