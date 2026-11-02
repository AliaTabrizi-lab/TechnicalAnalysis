crossover <- function(arr1, arr2) {
  if (length(arr1)!=length(arr2)) stop("Both arrays should have the same length")
  if (!is.numeric(arr1) || !is.numeric(arr2)) stop("Both arrays must be numeric")
  crossover_signals <- rep(FALSE,length(arr1))
  if (length(arr1)>1) for (i in 2:length(arr1)) {
    if (all(is.finite(c(arr1[i],arr2[i],arr1[i-1],arr2[i-1]))))
      crossover_signals[i] <- arr1[i]>arr2[i] && arr1[i-1]<=arr2[i-1]
  }
  return(crossover_signals)
}
