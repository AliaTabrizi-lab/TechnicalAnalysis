ema <- function(data, period) {
  if (!is.numeric(data) || length(data)<1 || any(!is.finite(data))) stop("data must be nonempty finite numeric")
  if (length(period)!=1 || !is.finite(period) || period < 1 || period != floor(period)) stop("invalid period")
  multiplier <- 2/(period+1)
  ema_values <- numeric(length(data))
  ema_values[1] <- data[1]
  if (length(data)>1) for (i in 2:length(data)) ema_values[i] <- (data[i]-ema_values[i-1])*multiplier+ema_values[i-1]
  return(ema_values)
}
