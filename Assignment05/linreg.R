linreg <- function(regressionSource, regressionLength, regressionOffset) {
  n <- length(regressionSource)
  if (!is.numeric(regressionSource) || any(!is.finite(regressionSource))) stop("regressionSource must be finite numeric")
  if (length(regressionLength)!=1 || !is.finite(regressionLength) || regressionLength!=floor(regressionLength) || regressionLength<2) stop("regressionLength must be integer >= 2")
  if (length(regressionOffset)!=1 || !is.finite(regressionOffset) || regressionOffset!=floor(regressionOffset) || regressionOffset<0) stop("regressionOffset must be nonnegative integer")
  if (regressionLength>n) stop("regressionLength cannot be greater than the number of elements in regressionSource")
  if (regressionOffset>=regressionLength) stop("regressionOffset must be less than regressionLength")
  start_index <- max(1,n-regressionLength+regressionOffset)
  end_index <- min(n,n-regressionOffset)
  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)
  mean_index <- sum(index_values)/length(index_values)
  mean_source <- sum(source_subset)/length(source_subset)
  numerator <- sum((index_values-mean_index)*(source_subset-mean_source))
  denominator <- sum((index_values-mean_index)^2)
  slope <- numerator/denominator
  intercept <- mean_source-slope*mean_index
  predicted_values <- slope*index_values+intercept
  result <- list(slope=slope, intercept=intercept, predicted_values=predicted_values)
  return(result)
}
