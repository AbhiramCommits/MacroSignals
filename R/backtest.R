# Cross-sectional long/short backtest with transaction costs and diagnostics.

daily_returns <- function(prices, instrument_meta = NULL) {
  prices <- prices[order(prices$instrument_id, prices$obs_date), ]
  out <- list()
  for (inst in sort(unique(prices$instrument_id))) {
    rows <- prices[prices$instrument_id == inst, ]
    rows <- rows[order(rows$obs_date), ]
    sign <- 1
    if (!is.null(instrument_meta)) {
      m <- instrument_meta[instrument_meta$instrument_id == inst, ]
      if (nrow(m) == 1) sign <- m$return_sign[[1]]
    }
    n <- nrow(rows)
    px_ret <- c(0, sign * (rows$px_last[-1] / rows$px_last[-n] - 1))
    carry_ret <- rows$carry_bp / 1e4 / 252
    out[[inst]] <- data.frame(
      instrument_id = inst,
      obs_date = rows$obs_date,
      ret = px_ret + carry_ret,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

#' Rank-based dollar-neutral weights with gross exposure exactly 1.
target_weights <- function(z) {
  instruments <- names(z)
  out <- stats::setNames(rep(0, length(z)), instruments)
  ok <- is.finite(z)
  if (sum(ok) < 2) return(out)
  r <- rank(z[ok], ties.method = "average")
  w <- r - (length(r) + 1) / 2
  denom <- sum(abs(w))
  if (!is.finite(denom) || denom < 1e-12) return(out)
  out[ok] <- w / denom
  out
}

#' Run a backtest for one signal.
#'
#' Signals are observed at the close of a rebalance date and earn returns from
#' the next trading day. Costs are charged on traded notional at the rebalance.
#'
#' @param signal_df Long signal table (as_of_date, instrument_id, signal_name,
#'   zscore).
#' @param prices Daily price/carry panel.
#' @param params Parameter list; cost_bp is read from it.
#' @param instrument_meta Optional metadata with return_sign.
#' @param signal_name Which signal to run (defaults to the first).
#' @param cost_bp Optional cost override in basis points.
#' @return A daily data frame with gross_pnl, net_pnl and turnover. The daily
#'   weight history and run settings are attached as attributes.
run_backtest <- function(signal_df, prices, params, instrument_meta = NULL,
                         signal_name = NULL, cost_bp = NULL) {
  if (is.null(signal_name)) signal_name <- unique(signal_df$signal_name)[[1]]
  sig <- signal_df[signal_df$signal_name == signal_name, ]
  if (nrow(sig) == 0) stop("No rows for signal ", signal_name, call. = FALSE)
  if (is.null(cost_bp)) cost_bp <- as.numeric(params$cost_bp %||% 0)
  cost_bp <- as.numeric(cost_bp)

  rets <- daily_returns(prices, instrument_meta)
  instruments <- sort(unique(rets$instrument_id))
  dates <- sort(unique(rets$obs_date))
  ret_mat <- matrix(0, nrow = length(dates), ncol = length(instruments),
                    dimnames = list(as.character(dates), instruments))
  ri <- match(rets$obs_date, dates)
  ci <- match(rets$instrument_id, instruments)
  ret_mat[cbind(ri, ci)] <- rets$ret

  sig_dates <- sort(unique(as.Date(sig$as_of_date)))
  targets <- list()
  for (i in seq_along(sig_dates)) {
    d <- sig_dates[[i]]
    rows <- sig[as.Date(sig$as_of_date) == d, ]
    z <- stats::setNames(rep(NA_real_, length(instruments)), instruments)
    z[rows$instrument_id] <- rows$zscore
    targets[[as.character(d)]] <- target_weights(z)
  }

  w_held <- stats::setNames(rep(0, length(instruments)), instruments)
  gross_pnl <- numeric(length(dates))
  net_pnl <- numeric(length(dates))
  turnover <- numeric(length(dates))
  cost <- numeric(length(dates))
  weight_rows <- vector("list", length(dates))

  for (i in seq_along(dates)) {
    d <- dates[[i]]
    r <- ret_mat[i, ]
    gross <- sum(w_held * r)
    gross_pnl[[i]] <- gross

    # Positions drift with returns between rebalances.
    denom <- 1 + gross
    if (is.finite(denom) && abs(denom) > 1e-12) {
      w_held <- w_held * (1 + r) / denom
    }

    key <- as.character(d)
    if (key %in% names(targets)) {
      target <- targets[[key]]
      common <- intersect(names(target), names(w_held))
      traded <- sum(abs(target[common] - w_held[common]))
      turnover[[i]] <- traded
      cost[[i]] <- traded * cost_bp / 1e4
      w_held <- target
    }
    net_pnl[[i]] <- gross - cost[[i]]

    weight_rows[[i]] <- data.frame(
      obs_date = d,
      instrument_id = instruments,
      weight = as.numeric(w_held),
      stringsAsFactors = FALSE
    )
  }

  pnl <- data.frame(
    date = dates,
    gross_pnl = gross_pnl,
    net_pnl = net_pnl,
    turnover = turnover,
    cost = cost,
    stringsAsFactors = FALSE
  )
  first_signal <- min(sig_dates)
  pnl <- pnl[pnl$date >= first_signal, ]
  row.names(pnl) <- NULL

  weights <- do.call(rbind, weight_rows)
  weights <- weights[weights$obs_date >= first_signal, ]
  weights <- weights[order(weights$obs_date, weights$instrument_id), ]
  row.names(weights) <- NULL

  attr(pnl, "weights") <- weights
  attr(pnl, "signal_name") <- signal_name
  attr(pnl, "cost_bp") <- cost_bp
  attr(pnl, "run_settings") <- list(
    rebalances = length(sig_dates),
    instruments = length(instruments),
    start = as.character(min(dates)),
    end = as.character(max(dates))
  )
  pnl
}

max_drawdown_info <- function(x, dates) {
  if (length(x) == 0) {
    return(list(max_dd = NA_real_, start = as.Date(NA), end = as.Date(NA)))
  }
  cum <- cumprod(1 + x)
  peak <- cummax(cum)
  dd <- cum / peak - 1
  trough <- which.min(dd)
  prior <- seq_len(trough)
  peak_idx <- max(prior[cum[prior] == peak[[trough]]])
  list(max_dd = dd[[trough]], start = dates[[peak_idx]],
       end = dates[[trough]])
}

holding_period <- function(weights) {
  if (is.null(weights) || nrow(weights) == 0) return(NA_real_)
  runs <- numeric(0)
  for (inst in unique(weights$instrument_id)) {
    w <- weights$weight[weights$instrument_id == inst]
    w <- w[order(weights$obs_date[weights$instrument_id == inst])]
    sign <- sign(round(w, 10))
    r <- rle(sign)
    runs <- c(runs, r$lengths[r$values != 0])
  }
  if (length(runs) == 0) return(NA_real_)
  mean(runs)
}

#' Performance and risk diagnostics for a backtest, gross and net.
metrics <- function(pnl, signal_name = attr(pnl, "signal_name"),
                    cost_bp = attr(pnl, "cost_bp")) {
  if (is.null(signal_name)) signal_name <- "signal"
  if (is.null(cost_bp)) cost_bp <- NA_real_
  n <- nrow(pnl)
  years <- n / 252
  weights <- attr(pnl, "weights")
  hp <- holding_period(weights)

  one_basis <- function(x) {
    ann_return <- if (any(1 + x <= 0)) {
      -1
    } else {
      prod(1 + x)^(252 / n) - 1
    }
    ann_vol <- stats::sd(x) * sqrt(252)
    sharpe <- if (is.finite(ann_vol) && ann_vol > 0) {
      mean(x) / stats::sd(x) * sqrt(252)
    } else {
      NA_real_
    }
    dd <- max_drawdown_info(x, pnl$date)
    data.frame(
      signal_name = signal_name,
      basis = NA_character_,
      ann_return = ann_return,
      ann_vol = ann_vol,
      sharpe = sharpe,
      max_dd = dd$max_dd,
      dd_start = dd$start,
      dd_end = dd$end,
      calmar = if (is.finite(dd$max_dd) && dd$max_dd < 0) {
        ann_return / abs(dd$max_dd)
      } else {
        NA_real_
      },
      hit_rate = mean(x > 0),
      ann_turnover = sum(pnl$turnover) / years,
      avg_holding_days = hp,
      n_days = n,
      cost_bp = cost_bp,
      stringsAsFactors = FALSE
    )
  }

  gross <- one_basis(pnl$gross_pnl)
  gross$basis <- "gross"
  net <- one_basis(pnl$net_pnl)
  net$basis <- "net"
  out <- rbind(gross, net)
  row.names(out) <- NULL
  out
}

#' Daily equity curves and drawdowns for plotting and export.
equity_curve <- function(pnl, signal_name = attr(pnl, "signal_name")) {
  if (is.null(signal_name)) signal_name <- "signal"
  cum_gross <- cumprod(1 + pnl$gross_pnl)
  cum_net <- cumprod(1 + pnl$net_pnl)
  peak <- cummax(cum_net)
  data.frame(
    signal_name = signal_name,
    date = pnl$date,
    gross_pnl = pnl$gross_pnl,
    net_pnl = pnl$net_pnl,
    cum_gross = cum_gross,
    cum_net = cum_net,
    drawdown_net = cum_net / peak - 1,
    stringsAsFactors = FALSE
  )
}

#' Run every signal and collect metrics and curves.
run_all_backtests <- function(signals, prices, params, instrument_meta = NULL,
                              cost_bp = NULL) {
  names_to_run <- intersect(c("carry", "momentum", "macro_surprise", "composite"),
                            names(signals))
  metrics_out <- list()
  curves_out <- list()
  pnl_out <- list()
  for (nm in names_to_run) {
    bt <- run_backtest(signals[[nm]], prices, params, instrument_meta,
                       signal_name = nm, cost_bp = cost_bp)
    metrics_out[[nm]] <- metrics(bt, signal_name = nm)
    curves_out[[nm]] <- equity_curve(bt, signal_name = nm)
    pnl_out[[nm]] <- bt
  }
  list(
    metrics = do.call(rbind, metrics_out),
    curves = do.call(rbind, curves_out),
    pnl = pnl_out
  )
}

#' Cost sensitivity: rerun every signal over a grid of one-way costs.
cost_sensitivity <- function(signals, prices, params, instrument_meta = NULL,
                             grid = NULL) {
  if (is.null(grid)) grid <- as.numeric(unlist(params$cost_grid_bp))
  names_to_run <- intersect(c("carry", "momentum", "macro_surprise", "composite"),
                            names(signals))
  out <- list()
  for (nm in names_to_run) {
    for (bp in grid) {
      bt <- run_backtest(signals[[nm]], prices, params, instrument_meta,
                         signal_name = nm, cost_bp = bp)
      m <- metrics(bt, signal_name = nm)
      m <- m[m$basis == "net", ]
      out[[length(out) + 1]] <- data.frame(
        signal_name = nm,
        cost_bp = bp,
        ann_return = m$ann_return,
        ann_vol = m$ann_vol,
        sharpe = m$sharpe,
        ann_turnover = m$ann_turnover,
        max_dd = m$max_dd,
        stringsAsFactors = FALSE
      )
    }
  }
  result <- do.call(rbind, out)
  row.names(result) <- NULL
  result
}
