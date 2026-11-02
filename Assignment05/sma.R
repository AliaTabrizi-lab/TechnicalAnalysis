sma <- function(data, period) {
  if (!is.numeric(data) || any(!is.finite(data))) stop("data must be finite numeric")
  if (length(period)!=1 || !is.finite(period) || period < 1 || period != floor(period)) stop("invalid period")
  if (length(data) < period) stop("Data length should be greater than or equal to the period")
  sma_values <- numeric(length(data)-period+1)
  for (i in seq_along(sma_values)) sma_values[i] <- sum(data[i:(i+period-1)])/period
  return(sma_values)
}
