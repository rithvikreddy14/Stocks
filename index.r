# Load necessary libraries
install.packages("forecast")
install.packages("tseries")

library(forecast)
library(tseries)

# Function to read input data and predict stock prices
stock_prediction <- function() {
  
  # Read the data from CSV
  data <- read.csv("Datasets/yahoo_stock.csv")
  
  # Convert the Date column to Date type
  data$Date <- as.Date(data$Date, format="%d-%m-%Y")
  
  # Create a time series object for the 'Close' column (stock price)
  stock_ts <- ts(data$Close, frequency=252)  # Assume daily stock data, with 252 trading days in a year
  
  # Plot the original time series data
  plot(stock_ts, main="Stock Price Time Series", xlab="Time", ylab="Stock Price", col="blue")
  
  # Check if the data is stationary
  adf_test <- adf.test(stock_ts)
  print(adf_test)
  
  # If not stationary, make it stationary by differencing
  if (adf_test$p.value > 0.05) {
    stock_ts <- diff(stock_ts)
    plot(stock_ts, main="Differenced Stock Price Time Series", xlab="Time", ylab="Differenced Stock Price", col="blue")
  }
  
  # Fit an ARIMA model to the time series
  fit <- auto.arima(stock_ts)
  summary(fit)
  
  # Forecast the stock price for the next 10 days (you can change this number)
  forecasted_values <- forecast(fit, h=10)
  
  # Plot the forecasted values
  plot(forecasted_values, main="Stock Price Forecast", xlab="Time", ylab="Forecasted Stock Price", col="red")
  
  # Print the forecasted values
  print(forecasted_values)
}

# Call the function to run the stock price prediction
stock_prediction()
