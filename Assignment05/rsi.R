rsi <- function(data, period) {
  if (!is.numeric(data) || any(!is.finite(data))) stop("data must be finite numeric")
  if (length(period)!=1 || !is.finite(period) || period<1 || period!=floor(period)) stop("invalid period")
  n <- length(data)
  if (n<=period) stop("RSI needs at least period + 1 prices")
  changes <- diff(data)
  gains <- pmax(changes,0)
  losses <- pmax(-changes,0)
  avg_gain <- sum(gains[1:period])/period
  avg_loss <- sum(losses[1:period])/period
  rsi_values <- rep(NA_real_,n)
  calc_rsi <- function(g,l) { if (l==0 && g==0) return(50); if (l==0) return(100); 100-100/(1+g/l) }
  rsi_values[period+1] <- calc_rsi(avg_gain,avg_loss)
  if (n>period+1) for (i in (period+2):n) {
    avg_gain <- (avg_gain*(period-1)+gains[i-1])/period
    avg_loss <- (avg_loss*(period-1)+losses[i-1])/period
    rsi_values[i] <- calc_rsi(avg_gain,avg_loss)
  }
  return(rsi_values)
}
