library(forecast)
library(tseries)


source("colera_data.R")


# constants ---------------------------------------------------------------


colera_provincias <- unique(df_colera.groupByProvinciaFecha$Provincia)
colera_municipios <- as.factor(unique(df_colera.groupByProvinciaFechaMunicipo$Municipio))

EPICENTRE_MAX <- 100 # more than 100 invasiones/defunciones
EPICENTRE_LIMIT <- 13 # number of rows corresponding to the first month
PERIOD <- 7 # weekly
HORIZON <- 30


# functions ---------------------------------------------------------------


colera_epicentres <- function(df_colera, cause, county=NULL, city=NULL) {
  
  if (is.null(city)) { # counties 
    
    df_colera.tmp <- subset(df_colera, Provincia == county)
    c <- county
    
  } else if (is.null(county)) { # cities 
    
    df_colera.tmp <- subset(df_colera, Municipio == city)
    c <- city
  }
  
  if (cause == INVASIONES_STR) {
    
    if (sum(df_colera.tmp$Total_invasiones[1:EPICENTRE_LIMIT]) > EPICENTRE_MAX && !is.na(sum(df_colera.tmp$Total_invasiones[1:EPICENTRE_LIMIT]))) { # is epicentre (more than 100 invasiones)
      
      print(paste(INVASIONES_STR, "en", c, ":", sum(df_colera.tmp$Total_invasiones[1:EPICENTRE_LIMIT]), sep = " "))
    }
    
  } else if (cause == DEFUNCIONES_STR) {
    
    if (sum(df_colera.tmp$Total_defunciones[1:EPICENTRE_LIMIT]) > EPICENTRE_MAX && !is.na(sum(df_colera.tmp$Total_defunciones[1:EPICENTRE_LIMIT]))) { # is epicentre (more than 100 defunciones)
      
      print(paste(DEFUNCIONES_STR, "en", c, ":", sum(df_colera.tmp$Total_defunciones[1:EPICENTRE_LIMIT]), sep = " "))
    }
  }
}


coleraTSbyCounty <- function(df_colera, cause, county) {
  
  df_colera.tmp <- subset(df_colera, Provincia == county)

  if (cause == INVASIONES_STR) {
    
    aes <- aes(x = Fecha, y = Total_invasiones)
    
  } else {
    
    aes <- aes(x = Fecha, y = Total_defunciones)
  }
  
  # print(
  ggplot(df_colera.tmp, aes) + 
    geom_line() +
    xlab("día-mes") +
    ylab("número") +
    ggtitle(paste0("colera ", cause, " en ", county, ", ", ANO_STR)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_continuous(
      breaks = as.numeric(df_colera.tmp$Fecha),
      labels = format(df_colera.tmp$Fecha, "%d - %m")
    )
  # )
  
  ggsave(paste0(COLERA_PLOTS_DIR, "/ts.colera_", cause, "_", county, ".png"),
    width = 15.7,
    height = 4.5,
    dpi = 300,
    limitsize = TRUE)

  return(df_colera.tmp)
  
}


# main --------------------------------------------------------------------


# epicentres & plots ------------------------------------------------------


for (provincia in colera_provincias) { # for each "Provincia"
  
  # invasiones by county
  colera_epicentres(df_colera.groupByProvinciaFecha, INVASIONES_STR, provincia, NULL)
  coleraTSbyCounty(df_colera.groupByProvinciaFecha, INVASIONES_STR, provincia)
  
  # defunciones by county
  colera_epicentres(df_colera.groupByProvinciaFecha, DEFUNCIONES_STR, provincia, NULL)
  coleraTSbyCounty(df_colera.groupByProvinciaFecha, DEFUNCIONES_STR, provincia)
  
}


for (municipio in colera_municipios) { # for each "Municipio"
  
  # invasiones by city
  colera_epicentres(df_colera.groupByProvinciaFechaMunicipo, INVASIONES_STR, NULL, municipio)
  
  # defunciones by city
  colera_epicentres(df_colera.groupByProvinciaFechaMunicipo, DEFUNCIONES_STR, NULL, municipio)
  
  # TODO: coleraTSbyCity
  
}


# ARIMA -------------------------------------------------------------------


# original data
ts_invasiones.valencia <- coleraTSbyCounty(df_colera.groupByProvinciaFecha, INVASIONES_STR, "valencia")
rownames(ts_invasiones.valencia) <- 1:nrow(ts_invasiones.valencia)

# minimal data
ts_invasiones.valencia.min <- ts_invasiones.valencia[1:44,] 
ggplot(ts_invasiones.valencia.min, aes(Fecha, Total_invasiones)) + 
  geom_line() + 
  xlab("día-mes") +
  ylab("número") +
  ggtitle(paste0("colera ", INVASIONES_STR, " en valencia, junio-julio del ", ANO_STR))

# calculation of the seasonal component  
# by smoothing and adjust the original series by subtracting the seasonality
count_ts <- ts(ts_invasiones.valencia.min$Total_invasiones, frequency = PERIOD)
# autoplot(count_ts)
decomp <- stl(count_ts, s.window = "periodic")
deseasonal_invasiones <- seasadj(decomp)
plot(decomp)

# not stationary p-value = 0.4225
adf.test(count_ts, alternative = "stationary") 
acf(count_ts, main = "Autocorrelación con estacionalidad")
pacf(count_ts, main = "Autocorrelación parcial con estacionalidad")

# differentiation
count_ts.d1 <- diff(deseasonal_invasiones, differences = 1)
# autoplot(count_ts.d1)
plot(count_ts.d1)

# stationary p-value = 0.01
adf.test(count_ts.d1, alternative = "stationary") 
acf(count_ts.d1, main = "Autocorrelación sin estacionalidad")
pacf(count_ts.d1, main = "Autocorrelación parcial sin estacionalidad")

# fit the model
fitARIMA <- auto.arima(deseasonal_invasiones, D = 1)

# ACF y PACF for the residuals of the model
checkresiduals(fitARIMA)
tsdisplay(residuals(fitARIMA), lag.max = 10, main = "Model Residuals")

# Ljung-Box test on the residuals of the model, p-value = 0.4102
Box.test(fitARIMA$residuals, type = "Ljung-Box") 

# prediction
forecastARIMA <- forecast(fitARIMA, h = HORIZON)
plot(forecastARIMA)
grid()

fitARIMA %>% forecast(h = 30) %>% autoplot()


plot(deseasonal_invasiones, type = "l")
lines(fitARIMA[["fitted"]], type = "l", col = "darkorange")


# train and test ----------------------------------------------------------


trainset <- subset(count_ts.d1, end = length(count_ts.d1) - PERIOD)
testset <- subset(count_ts.d1, start = length(count_ts.d1) - PERIOD + 1)

fitARIMA <- auto.arima(trainset)
forecastARIMA <- forecast(fitARIMA)

autoplot(trainset, xlab = "día-mes", ylab = "número invasiones") +
  autolayer(testset) +
  autolayer(forecastARIMA, series = "ARIMA(3,1,0)(2,1,0)[7]", PI = FALSE) +
  ggtitle("Predicción obtenida del modelo ARIMA")
