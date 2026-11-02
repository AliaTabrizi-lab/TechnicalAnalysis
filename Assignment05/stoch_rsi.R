stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)
  if (any(!is.finite(c(k_period,d_period))) || any(c(k_period,d_period)<1) || any(c(k_period,d_period)!=floor(c(k_period,d_period)))) stop("invalid smoothing periods")
  valid <- which(!is.na(rsi_values))
  if (length(valid)<k_period+d_period-1) stop("Not enough RSI observations")
  # Normalize using a rolling RSI range, avoiding future observations.
  raw_k <- rep(NA_real_,length(data))
  for (i in valid) {
    if (i >= valid[1]+period-1) {
      window <- rsi_values[(i-period+1):i]
      if (all(!is.na(window))) {
        lo <- min(window); hi <- max(window)
        raw_k[i] <- if (hi==lo) 0.5 else (rsi_values[i]-lo)/(hi-lo)
      }
    }
  }
  available <- which(!is.na(raw_k))
  k_line <- rep(NA_real_,length(data))
  if (length(available)>=k_period) for (i in available[k_period:length(available)]) {
    window <- raw_k[(i-k_period+1):i]
    if (all(!is.na(window))) k_line[i] <- sum(window)/k_period
  }
  d_line <- rep(NA_real_,length(data))
  valid_k <- which(!is.na(k_line))
  if (length(valid_k)>=d_period) for (i in valid_k[d_period:length(valid_k)]) {
    window <- k_line[(i-d_period+1):i]
    if (all(!is.na(window))) d_line[i] <- sum(window)/d_period
  }
  result <- list(k_line=k_line,d_line=d_line)
  return(result)
}
