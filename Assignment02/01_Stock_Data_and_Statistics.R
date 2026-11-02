
# CDI College - BDA400 Assignment 2
# Student: Alia GhahremaniTabrizi
# Technical Analysis Using R - Preliminary Stage
# Script 01: Stock Data and Statistics

library(quantmod)
library(TTR)

# Read portfolio and import historical stock data
load_stock_data <- function(portfolio_file) {
  
  symbols <- trimws(readLines(portfolio_file))
  symbols <- symbols[nzchar(symbols)]
  
  stock_data <- list()
  
  for (symbol in symbols) {
    
    cat("Downloading:", symbol, "\n")
    
    stock <- tryCatch(
      getSymbols(
        symbol,
        src = "yahoo",
        from = "2025-01-01",
        auto.assign = FALSE
      ),
      error = function(e) {
        message("Failed: ", symbol, " - ", e$message)
        NULL
      }
    )
    
    if (!is.null(stock)) {
      stock_data[[symbol]] <- as.data.frame(stock)
    }
  }
  
  return(stock_data)
}

# Calculate the most frequent observed value
calculate_mode <- function(x) {
  
  x <- na.omit(x)
  
  if (length(x) == 0) return(NA_real_)
  
  values <- unique(x)
  
  values[which.max(tabulate(match(x, values)))]
}

# Calculate descriptive statistics and moving average
calculate_statistics <- function(stock_df, symbol) {
  
  price_col <- grep(
    "\\.Adjusted$",
    names(stock_df),
    value = TRUE
  )
  
  if (length(price_col) == 0) {
    price_col <- grep(
      "\\.Close$",
      names(stock_df),
      value = TRUE
    )
  }
  
  if (length(price_col) == 0) {
    stop("No closing price column found for ", symbol)
  }
  
  prices <- as.numeric(stock_df[[price_col[1]]])
  prices <- na.omit(prices)
  
  sma20 <- TTR::SMA(prices, n = 20)
  
  data.frame(
    Symbol = symbol,
    Mean = mean(prices),
    Median = median(prices),
    Mode = calculate_mode(prices),
    Std_Dev = sd(prices),
    Latest_SMA20 = tail(na.omit(sma20), 1)
  )
}

# Import the portfolio data
stock_data <- load_stock_data(
  "TechnicalAnalysis/Assignment02/portfolio.txt"
)

# Calculate statistics for each stock
statistics_results <- do.call(
  rbind,
  lapply(names(stock_data), function(symbol) {
    calculate_statistics(stock_data[[symbol]], symbol)
  })
)

# Display results
print(statistics_results)

# Save results to CSV
write.csv(
  statistics_results,
  "TechnicalAnalysis/Assignment02/Stock_Statistics.csv",
  row.names = FALSE
)

cat("Stock analysis completed successfully.\n")
