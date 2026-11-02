
# CDI College - BDA400 Assignment 2
# Student: Alia GhahremaniTabrizi
# Script 02: Stock Data Visualizations

library(TTR)

# Check that stock data is available
if (!exists("stock_data")) {
  stop("Run Script 01 before running Script 02.")
}

output_folder <- "TechnicalAnalysis/Assignment02"

# Generate charts for each stock
for (symbol in names(stock_data)) {
  
  stock_df <- stock_data[[symbol]]
  
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
  
  prices <- as.numeric(stock_df[[price_col[1]]])
  dates <- as.Date(rownames(stock_df))
  sma20 <- TTR::SMA(prices, n = 20)
  
  chart_file <- file.path(
    output_folder,
    paste0(symbol, "_Chart.png")
  )
  
  png(
    filename = chart_file,
    width = 1200,
    height = 700
  )
  
  plot(
    dates,
    prices,
    type = "l",
    col = "blue",
    main = paste(symbol, "Stock Price and 20-Day SMA"),
    xlab = "Date",
    ylab = "Stock Price (USD)"
  )
  
  lines(dates, sma20, col = "red", lwd = 2)
  
  legend(
    "topleft",
    legend = c("Stock Price", "20-Day SMA"),
    col = c("blue", "red"),
    lty = 1,
    bty = "n"
  )
  
  dev.off()
  
  cat("Chart saved for:", symbol, "\n")
}

cat("All stock visualizations completed.\n")
