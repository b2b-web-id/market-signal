require_symbol <- function(symbol, envir = parent.frame()) {
  if (is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
  }

  envir[[symbol]]
}

fNum <- function(x) {
  format(x, decimal.mark = ".", big.mark = " ", nsmall = 1)
}

display_num <- function(x) {
  if (length(x) == 0 || is.na(x[[1]])) {
    "N/A"
  } else {
    fNum(x)
  }
}

build_symbol <- function(symbol) {
  paste(symbol, "JK", sep = ".")
}

scalar_numeric <- function(x) {
  value <- suppressWarnings(as.numeric(x))

  if (!length(value) || is.na(value[[1]])) {
    NA_real_
  } else {
    value[[1]]
  }
}

quote_field <- function(quote_data, candidates, fallback_index = NULL) {
  if (!is.null(quote_data) && NROW(quote_data) >= 1) {
    for (candidate in candidates) {
      if (candidate %in% names(quote_data)) {
        return(scalar_numeric(quote_data[[candidate]][[1]]))
      }
    }

    if (!is.null(fallback_index) && fallback_index <= NCOL(quote_data)) {
      return(scalar_numeric(quote_data[[fallback_index]][[1]]))
    }
  }

  NA_real_
}

indicator_snapshot <- function(symbol_data) {
  close_data <- Cl(symbol_data)

  data.frame(
    sma10 = scalar_numeric(tail(SMA(close_data, n = 10), n = 1)),
    sma20 = scalar_numeric(tail(SMA(close_data, n = 20), n = 1)),
    sma30 = scalar_numeric(tail(SMA(close_data, n = 30), n = 1)),
    sma40 = scalar_numeric(tail(SMA(close_data, n = 40), n = 1)),
    sma50 = scalar_numeric(tail(SMA(close_data, n = 50), n = 1)),
    sma60 = scalar_numeric(tail(SMA(close_data, n = 60), n = 1)),
    ema10 = scalar_numeric(tail(EMA(close_data, n = 10), n = 1)),
    ema20 = scalar_numeric(tail(EMA(close_data, n = 20), n = 1)),
    ema30 = scalar_numeric(tail(EMA(close_data, n = 30), n = 1)),
    ema40 = scalar_numeric(tail(EMA(close_data, n = 40), n = 1)),
    ema50 = scalar_numeric(tail(EMA(close_data, n = 50), n = 1))
  )
}

make_chart <- function(symbol_data, symbol, chart_type = "candlesticks", subset = "2017-12-01/") {
  candleChart(
    symbol_data,
    type = chart_type,
    up.col = "blue",
    dn.col = "red",
    theme = "white",
    subset = subset,
    TA = c(
      addMACD(fast = 12, slow = 26, signal = 2),
      addRSI(n = 14),
      addBBands(n = 20, sd = 2, maType = "SMA"),
      addSMA(n = 10, col = "blue"),
      addEMA(n = 50, col = "green"),
      addSAR(),
      addVo()
    ),
    name = symbol
  )
}

lastDay <- function(symbol_data) {
  data.frame(
    open = tail(symbol_data, n = 1)[[1]],
    high = tail(symbol_data, n = 1)[[2]],
    low = tail(symbol_data, n = 1)[[3]],
    close = tail(symbol_data, n = 1)[[4]],
    volume = tail(symbol_data, n = 1)[[5]]
  )
}

today <- function(symbol) {
  mData <- tryCatch(
    getQuote(build_symbol(symbol), src = "yahoo"),
    error = function(e) NULL
  )

  if (is.null(mData) || NROW(mData) < 1) {
    return(NULL)
  }

  data.frame(
    open = quote_field(mData, c("Open", "Open Price"), fallback_index = 4),
    high = quote_field(mData, c("Day's High", "Days High", "High"), fallback_index = 5),
    low = quote_field(mData, c("Day's Low", "Days Low", "Low"), fallback_index = 6),
    last = quote_field(mData, c("Last Trade (Price Only)", "Last", "Trade Price"), fallback_index = 2),
    volume = quote_field(mData, c("Volume"), fallback_index = 7)
  )
}

summarize_window <- function(symbol_data, window) {
  mData <- na.omit(as.data.frame(tail(symbol_data, n = window)))
  names(mData) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

  data.frame(
    highest = max(mData[, "High"]),
    lowest = min(mData[, "Low"]),
    mean_price = mean(mData[, "Close"]),
    mean_volume = mean(mData[, "Vol"])
  )
}

feature_engine <- function(symbol_data) {
  close_data <- Cl(symbol_data)
  volume_data <- Vo(symbol_data)
  high_low_close <- HLC(symbol_data)

  close_now <- scalar_numeric(last(close_data))
  close_prev <- scalar_numeric(last(close_data, 2)[1])
  ema20_now <- scalar_numeric(last(EMA(close_data, n = 20)))
  ema50_now <- scalar_numeric(last(EMA(close_data, n = 50)))
  ema50_prev <- scalar_numeric(last(EMA(close_data, n = 50), 2)[1])
  sma20_now <- scalar_numeric(last(SMA(close_data, n = 20)))
  sma60_now <- scalar_numeric(last(SMA(close_data, n = 60)))
  atr14_now <- scalar_numeric(last(ATR(high_low_close, n = 14)$atr))
  avg_volume20 <- scalar_numeric(last(SMA(volume_data, n = 20)))
  volume_now <- scalar_numeric(last(volume_data))
  highest20_prev <- scalar_numeric(max(as.numeric(last(Lag(Hi(symbol_data), 1), 20)), na.rm = TRUE))
  lowest20_prev <- scalar_numeric(min(as.numeric(last(Lag(Lo(symbol_data), 1), 20)), na.rm = TRUE))

  distance_ema50_pct <- if (is.na(close_now) || is.na(ema50_now) || ema50_now == 0) {
    NA_real_
  } else {
    ((close_now / ema50_now) - 1) * 100
  }

  momentum_5d_pct <- if (NROW(close_data) < 6) {
    NA_real_
  } else {
    close_5d_ago <- scalar_numeric(last(close_data, 6)[1])
    if (is.na(close_5d_ago) || close_5d_ago == 0) NA_real_ else ((close_now / close_5d_ago) - 1) * 100
  }

  day_return_pct <- if (is.na(close_prev) || close_prev == 0) {
    NA_real_
  } else {
    ((close_now / close_prev) - 1) * 100
  }

  volume_ratio_20 <- if (is.na(avg_volume20) || avg_volume20 == 0) {
    NA_real_
  } else {
    volume_now / avg_volume20
  }

  atr_pct <- if (is.na(close_now) || close_now == 0) {
    NA_real_
  } else {
    (atr14_now / close_now) * 100
  }

  range_20d_pct <- if (is.na(highest20_prev) || is.na(lowest20_prev) || is.na(close_now) || close_now == 0) {
    NA_real_
  } else {
    ((highest20_prev - lowest20_prev) / close_now) * 100
  }

  data.frame(
    close = close_now,
    ema20 = ema20_now,
    ema50 = ema50_now,
    sma20 = sma20_now,
    sma60 = sma60_now,
    atr14 = atr14_now,
    atr_pct = atr_pct,
    volume = volume_now,
    avg_volume20 = avg_volume20,
    volume_ratio_20 = volume_ratio_20,
    distance_ema50_pct = distance_ema50_pct,
    momentum_5d_pct = momentum_5d_pct,
    day_return_pct = day_return_pct,
    range_20d_pct = range_20d_pct,
    ema50_slope_up = !is.na(ema50_now) && !is.na(ema50_prev) && ema50_now > ema50_prev,
    breakout_20d = !is.na(close_now) && !is.na(highest20_prev) && close_now > highest20_prev,
    breakdown_20d = !is.na(close_now) && !is.na(lowest20_prev) && close_now < lowest20_prev,
    price_above_ema20 = !is.na(close_now) && !is.na(ema20_now) && close_now > ema20_now,
    price_above_ema50 = !is.na(close_now) && !is.na(ema50_now) && close_now > ema50_now,
    sma_stack_bullish = !is.na(sma20_now) && !is.na(sma60_now) && sma20_now > sma60_now
  )
}

classify_regime <- function(features) {
  if (features$breakout_20d && features$price_above_ema50 && features$ema50_slope_up && !is.na(features$volume_ratio_20) && features$volume_ratio_20 >= 1.1) {
    return("bullish_breakout")
  }

  if (features$breakdown_20d && !features$price_above_ema50 && !is.na(features$volume_ratio_20) && features$volume_ratio_20 >= 1.1) {
    return("bearish_breakdown")
  }

  if (features$price_above_ema50 && features$ema50_slope_up && !is.na(features$atr_pct) && features$atr_pct >= 2 && features$atr_pct <= 5) {
    return("trend_up")
  }

  if (!features$price_above_ema50 && !features$ema50_slope_up && !is.na(features$atr_pct) && features$atr_pct >= 2 && features$atr_pct <= 5) {
    return("trend_down")
  }

  if (!is.na(features$atr_pct) && features$atr_pct >= 6) {
    return("high_volatility")
  }

  if (!is.na(features$range_20d_pct) && features$range_20d_pct <= 8 && !is.na(features$atr_pct) && features$atr_pct <= 3) {
    return("compression")
  }

  "range"
}

score_decision <- function(features, regime) {
  score <- 0

  score <- score + ifelse(features$price_above_ema20, 1, -1)
  score <- score + ifelse(features$price_above_ema50, 2, -2)
  score <- score + ifelse(features$ema50_slope_up, 1, -1)
  score <- score + ifelse(features$sma_stack_bullish, 1, -1)

  if (!is.na(features$momentum_5d_pct)) {
    score <- score + ifelse(features$momentum_5d_pct >= 4, 2, ifelse(features$momentum_5d_pct >= 1.5, 1, ifelse(features$momentum_5d_pct <= -4, -2, ifelse(features$momentum_5d_pct <= -1.5, -1, 0))))
  }

  if (!is.na(features$volume_ratio_20)) {
    score <- score + ifelse(features$volume_ratio_20 >= 2, 2, ifelse(features$volume_ratio_20 >= 1.2, 1, ifelse(features$volume_ratio_20 <= 0.6, -1, 0)))
  }

  score <- score + ifelse(features$breakout_20d, 2, 0)
  score <- score + ifelse(features$breakdown_20d, -2, 0)

  if (!is.na(features$distance_ema50_pct)) {
    score <- score + ifelse(features$distance_ema50_pct >= 8, 1, ifelse(features$distance_ema50_pct <= -8, -1, 0))
  }

  if (identical(regime, "high_volatility")) {
    score <- score - 2
  }

  if (identical(regime, "bullish_breakout")) {
    score <- score + 2
  }

  if (identical(regime, "bearish_breakdown")) {
    score <- score - 2
  }

  if (identical(regime, "compression")) {
    score <- ifelse(features$price_above_ema50, score + 1, score - 1)
  }

  label <- if (score >= 7) {
    "strong_bullish"
  } else if (score >= 3) {
    "bullish"
  } else if (score <= -7) {
    "strong_bearish"
  } else if (score <= -3) {
    "bearish"
  } else {
    "neutral"
  }

  action <- if (score >= 7) {
    "favor_long_bias"
  } else if (score >= 3) {
    "watch_long_setups"
  } else if (score <= -7) {
    "avoid_longs"
  } else if (score <= -3) {
    "defensive"
  } else {
    "wait"
  }

  data.frame(score = score, label = label, action = action, regime = regime)
}

sum360s <- function(symbol_data) {
  summarize_window(symbol_data, 360)
}

sum90s <- function(symbol_data) {
  summarize_window(symbol_data, 90)
}

sum60s <- function(symbol_data) {
  summarize_window(symbol_data, 60)
}

sum40s <- function(symbol_data) {
  summarize_window(symbol_data, 40)
}

sum20s <- function(symbol_data) {
  summarize_window(symbol_data, 20)
}

sum5s <- function(symbol_data) {
  summarize_window(symbol_data, 5)
}

summary360 <- function(kemarin, sum360) {
  vol_kemarin <- ifelse(kemarin$volume > sum360$mean_volume, "**atas**", "*bawah*")
  price_kemarin <- ifelse(kemarin$close > sum360$mean_price, "**atas**", "*bawah*")
  paste(
    sep = " ",
    "Berdasarkan data 360 hari terakhir, volume perdagangan kemarin di", vol_kemarin,
    "rata-rata volume perdagangan dan harga penutupan berada di",
    price_kemarin, "harga rata-rata penutupan"
  )
}

summary90 <- function(kemarin, sum90) {
  vol_kemarin <- ifelse(kemarin$volume > sum90$mean_volume, "**atas**", "*bawah*")
  price_kemarin <- ifelse(kemarin$close > sum90$mean_price, "**atas**", "*bawah*")
  paste(
    sep = " ",
    "Berdasarkan data 90 hari terakhir, volume perdagangan kemarin di", vol_kemarin,
    "rata-rata volume perdagangan dan harga penutupan berada di",
    price_kemarin, "harga rata-rata penutupan"
  )
}

summary20 <- function(kemarin, sum20) {
  vol_kemarin <- ifelse(kemarin$volume > sum20$mean_volume, "**atas**", "*bawah*")
  price_kemarin <- ifelse(kemarin$close > sum20$mean_price, "**atas**", "*bawah*")
  paste(
    sep = " ",
    "Berdasarkan data 20 hari terakhir, volume perdagangan kemarin di", vol_kemarin,
    "rata-rata volume perdagangan dan harga penutupan berada di",
    price_kemarin, "harga rata-rata penutupan"
  )
}

format_last_day <- function(snapshot) {
  paste(
    "[Yesterday] Open:", display_num(snapshot$open),
    "| High:", display_num(snapshot$high),
    "| Low:", display_num(snapshot$low),
    "| Close:", display_num(snapshot$close),
    "| Volume:", display_num(snapshot$volume)
  )
}

format_indicator_summary <- function(symbol_data) {
  indicators <- indicator_snapshot(symbol_data)

  paste(
    "[SMA] 10:", display_num(indicators$sma10),
    "| 20:", display_num(indicators$sma20),
    "| 30:", display_num(indicators$sma30),
    "| 40:", display_num(indicators$sma40),
    "| 50:", display_num(indicators$sma50),
    "| 60:", display_num(indicators$sma60)
  )
}

format_ema_summary <- function(symbol_data) {
  indicators <- indicator_snapshot(symbol_data)

  paste(
    "[EMA] 10:", display_num(indicators$ema10),
    "| 20:", display_num(indicators$ema20),
    "| 30:", display_num(indicators$ema30),
    "| 40:", display_num(indicators$ema40),
    "| 50:", display_num(indicators$ema50)
  )
}

format_today <- function(snapshot) {
  if (is.null(snapshot)) {
    return("[Today] Quote unavailable")
  }

  paste(
    "[Today] Last:", display_num(snapshot$last),
    "| Open:", display_num(snapshot$open),
    "| High:", display_num(snapshot$high),
    "| Low:", display_num(snapshot$low),
    "| Volume:", display_num(snapshot$volume)
  )
}

format_window_summary <- function(window, summary_data) {
  paste(
    paste0("[", window, " days] Lowest:"),
    display_num(summary_data$lowest),
    "| Highest:", display_num(summary_data$highest),
    "| Mean:", display_num(summary_data$mean_price),
    "| Vol Mean:", display_num(summary_data$mean_volume)
  )
}

format_feature_summary <- function(features) {
  paste(
    "[Features] Dist EMA50:",
    paste0(display_num(features$distance_ema50_pct), "%"),
    "| Mom 5D:", paste0(display_num(features$momentum_5d_pct), "%"),
    "| ATR%:", paste0(display_num(features$atr_pct), "%"),
    "| Range20D:", paste0(display_num(features$range_20d_pct), "%"),
    "| Vol x20:", display_num(features$volume_ratio_20),
    "| Breakout20D:", ifelse(features$breakout_20d, "yes", "no"),
    "| Breakdown20D:", ifelse(features$breakdown_20d, "yes", "no")
  )
}

format_regime_summary <- function(regime, features) {
  paste(
    "[Regime]", regime,
    "| EMA50 Slope:", ifelse(features$ema50_slope_up, "up", "down"),
    "| Above EMA50:", ifelse(features$price_above_ema50, "yes", "no"),
    "| SMA20>SMA60:", ifelse(features$sma_stack_bullish, "yes", "no")
  )
}

format_score_summary <- function(score_output) {
  paste(
    "[Decision] Score:", score_output$score,
    "| Label:", score_output$label,
    "| Action:", score_output$action
  )
}
