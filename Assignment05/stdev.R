stdev <- function(data) {
  if (!is.numeric(data) || length(data)<1 || any(!is.finite(data))) stop("data must be nonempty finite numeric")
  mean_value <- sum(data)/length(data)
  diff_values <- data-mean_value
  squared_diff <- diff_values^2
  variance <- sum(squared_diff)/length(data) # population SD, as specified
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}
