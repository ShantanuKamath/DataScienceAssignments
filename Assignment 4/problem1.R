# =======================================================
# Forecasting in Business through Time Series Model
# ARIMA = Auto-Regressive Integrated Moving Average
# =======================================================

# Install and load essentials
# install.packages("tseries")
# install.packages("forecast")
# install.packages("dplyr")

library(tseries)
library(forecast)
library(dplyr)

# Load and explore the dataset
CryptoCurrencies <- read.csv("crypto-markets.csv", header = TRUE)[, c("symbol", "date",  "open", "close")]
CryptoCurrencies$date <- as.Date(CryptoCurrencies$date)

# Creating helpful name lists
TSData <- list()
TSNames <- c("BTCOpen", "BTCCLose", "ETHOpen", "ETHClose", "XRPOpen", "XRPClose", "BCHOpen", "BCHClose", "LTCOpen", "LTCClose")
# Extracting all Time Series Relevant Data
j = 1
for (i in c("BTC", "ETH", "XRP", "BCH", "LTC")){
  data <- subset(CryptoCurrencies, symbol == i & date >= "2017-04-01" & date <= "2018-03-25")
  TSData[[TSNames[j]]] <- as.ts(data$open)
  TSData[[TSNames[j+1]]] <- as.ts(data$close)
  j = j+2
}

# =======================================================
# ARIMA(p,d,q) modeling of Non-Seasonal Time Series

# Standardize the Time Series
for (i in 1:10){
  print(adf.test(TSData[[TSNames[i]]]))
  TSData[[TSNames[i]]] <- log(TSData[[TSNames[i]]])
  TSData[[TSNames[i]]] <- diff(TSData[[TSNames[i]]])
  print(adf.test(TSData[[TSNames[i]]]))
  
}
# -------------------------------------------------------
# Modeling "TSData[[TSNames[i]]]" (stationary) using ARIMA

# We already know d = 0 for ARIMA
# Try to determine p and q values

# Auto-Correlation Visualized
lag.plot(TSData[[TSNames[i]]], set.lags = 0:8,
         layout = NULL, do.lines = FALSE)

# Auto-Correlation Function (ACF)
# helps identify the value of q
acf(TSData[[TSNames[i]]])       

# Partial Auto-Correlation Function (PACF)
# helps identify the value of p
pacf(TSData[[TSNames[i]]])

# Guess possible values of p and q
# Possible value of q = 1 in MA(q)
# Possible value of p = 3 in AR(p)

# Try fitting the guessed ARIMA(p,d,q) model
arimaFit <- arima(TSData[[TSNames[i]]], order = c(3,0,1))
arimaFit            # check the coefficients
plot(arimaFit)      # check the AR/MA roots


# -------------------------------------------------------
# Forecast using fitted ARIMA

# Within the training dataset
arimaFitFC <- forecast(TSData[[TSNames[i]]], model = arimaFit, h = 10)
plot(TSData[[TSNames[i]]], type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "red", lwd = 2)

# Beyond the training dataset
plot(arimaFitFC)


# -------------------------------------------------------
# Find the "optimal" ARIMA parameters

auto.arima(TSData[[TSNames[6]]],                   # the dataset
           seasonal = FALSE,            # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported
# Fit the "optimal" ARIMA(p,d,q) model
arimaOpt <- arima(TSData[[TSNames[6]]], order = c(3,0,0))
arimaOpt
plot(arimaOpt)

# Forecast using optimal ARIMA
# Within the training dataset
arimaOptFC <- forecast(TSData[[TSNames[6]]], model = arimaOpt, h = 21)
plot(TSData[[TSNames[6]]], type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)
# Beyond the training dataset
plot(arimaOptFC)
tsdisplay(residuals(arimaOptFC), lag.max = 80)

# -------------------------------------------------------
# Compare between guessed and optimal ARIMA models

plot(TSData[[TSNames[i]]], type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaFitFC$fitted, type="l", col = "darkgreen", lwd = 2)
points(arimaOptFC$fitted, type="l", col = "red", lwd = 2)



# =======================================================
# ARIMA(p,d,q)(P,D,Q)[s] modeling of Seasonal Time Series

# Plot the Time Series
plot(TSData[[TSNames[i]]], type="l", col = "blue", 
     lwd = 3, xlab = "", ylab = "")

# Find the "optimal" ARIMA parameters
auto.arima(TSData[[TSNames[i]]],                   # the dataset
           seasonal = TRUE,             # seasonality
           stationary = TRUE,           # stationarity
           max.p = 5, max.q = 5,        # range of p,q (non-seasonal)
           max.P = 5, max.Q = 5,        # range of P,Q (seasonal)
           max.d = 2, max.D = 2,        # range of d,D (differences)
           start.p = 1, start.q = 1,    # start for stepwise search
           start.P = 1, start.Q = 1,    # start for stepwise search 
           ic = "aic",                  # criteria to compare
           stepwise = TRUE,             # stepwise selection (faster)
           trace = TRUE)                # all ARIMA models reported

# Fit the "optimal" ARIMA(p,d,q)(P,D,Q)[s] model
arimaSea <- arima(TSData[[TSNames[i]]], order = c(0,0,1),          # p, d, q
                  seasonal = list(order = c(1, 0, 1),   # P, D, Q
                                  period = 4))          # seasonality
arimaSea
plot(arimaSea)


# -------------------------------------------------------
# Forecast using seasonal ARIMA

# Within the training dataset
arimaSeaFC <- forecast(TSData[[TSNames[i]]], model = arimaSea, h = 10)
plot(TSData[[TSNames[i]]], type="l", col = "lightblue", 
     lwd = 3, xlab = "", ylab = "")
points(arimaSeaFC$fitted, type="l", col = "red", lwd = 2)

# Beyond the training dataset
plot(arimaSeaFC)

# Check the performance/accuracy
tsdisplay(residuals(arimaSea), lag.max = 80)
# Residuals look like "White Noise" -- as desired! :-)


