library(forecast)
library(tseries)


source("colera_data.R")
source("colera_ts.R")


# constants ---------------------------------------------------------------


df_colera.groupByProvinciaFechaAV <- subset(df_colera.groupByProvinciaFecha,
  Provincia %in% c("zaragoza", "teruel", "huesca", "alicante", "castellon", "valencia"))

PERIOD <- 7 # weekly frequency 
HORIZON <- 30 # 30 days to predict
ARIMA_MINSAMPLE <- 50 # 50 rows (minimal data elements for ARIMA model)
STATIONARY_STR <- "stationary"


# functions ---------------------------------------------------------------


arima_sampleData <- function(df_colera, county) {
  
  ts_invasiones.tmp <- colera_ts(df_colera, INVASIONES_STR, county, NULL) # original data
  rownames(ts_invasiones.tmp) <- 1:nrow(ts_invasiones.tmp)
  
  first_case.tmp <- which(ts_invasiones.tmp$Total_invasiones!=0)[1] # row of first_case
  
  return(ts_invasiones.tmp[first_case.tmp:(first_case.tmp+ARIMA_MINSAMPLE)-1,]) # minimal data
  
}


run_arima <- function(ts_colera, county) {
  
  # calculation of the seasonal component  
  # by smoothing and adjust the original series by subtracting the seasonality
  count_ts <- ts(ts_colera$Total_invasiones, frequency = PERIOD)
  decomp <- stl(count_ts, s.window = "periodic")
  deseasonal_invasiones <- seasadj(decomp)
  plot(decomp, main = paste0("decomposition of ts_", county))
  
  # not stationary
  adf.test(count_ts, alternative = STATIONARY_STR) 
  acf(count_ts, main = paste0("autocorrelation with seasonality (ts_", county, ")"))
  pacf(count_ts, main = paste0("partial autocorrelation with seasonality (ts_", county, ")"))
  
  # differentiation
  count_ts.d1 <- diff(deseasonal_invasiones, differences = 1)
  plot(count_ts.d1, main = paste0("ts_", county, " without seasonality"))
  
  # stationary
  adf.test(count_ts.d1, alternative = STATIONARY_STR) 
  acf(count_ts.d1, main = paste0("autocorrelation with seasonality (ts_", county, ")"))
  pacf(count_ts.d1, main = paste0("partial autocorrelation with seasonality (ts_", county, ")"))
  
  # data split
  trainset <- subset(count_ts.d1, end = length(count_ts.d1) - PERIOD)
  testset <- subset(count_ts.d1, start = length(count_ts.d1) - PERIOD + 1)
  print(trainset)
  print(testset)
  
  # fit the model
  fitARIMA <- auto.arima(trainset)
  orderARIMA <- as.vector(arimaorder(fitARIMA)[1:3])
  print(fitARIMA)
  
  # prediction
  forecastARIMA <- forecast(fitARIMA)
  
  # plotting the results
  print(
    autoplot(trainset, xlab = "día-mes", ylab = "número invasiones") +
      autolayer(testset) +
      autolayer(
        forecastARIMA,
        series = paste0("ARIMA (", orderARIMA[1], ",", orderARIMA[2], ",", orderARIMA[3], ")"),
        PI = FALSE
      ) +
      ggtitle(paste0("prediction of ARIMA model for ts_", county))
  )
  
  # model validation
  accuracy(forecastARIMA, count_ts.d1)
  
}


# main --------------------------------------------------------------------


# ARIMA -------------------------------------------------------------------


ts_invasiones.zaragoza <- arima_sampleData(df_colera.groupByProvinciaFechaAV, "zaragoza")
ts_invasiones.teruel <- arima_sampleData(df_colera.groupByProvinciaFechaAV, "teruel")
ts_invasiones.huesca <- arima_sampleData(df_colera.groupByProvinciaFechaAV, "huesca")
ts_invasiones.alicante <- arima_sampleData(df_colera.groupByProvinciaFechaAV, "alicante")
ts_invasiones.castellon <- arima_sampleData(df_colera.groupByProvinciaFechaAV, "castellon")
ts_invasiones.valencia <- arima_sampleData(df_colera.groupByProvinciaFechaAV, "valencia")

run_arima(ts_invasiones.zaragoza, "zaragoza")
run_arima(ts_invasiones.teruel, "teruel")
run_arima(ts_invasiones.huesca, "huesca")
run_arima(ts_invasiones.alicante, "alicante")
run_arima(ts_invasiones.castellon, "castellon")
run_arima(ts_invasiones.valencia, "valencia")
