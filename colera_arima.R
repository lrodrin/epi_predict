library(forecast)
library(tseries)


source("colera_data.R")


# constants ---------------------------------------------------------------


colera_provincias <- unique(df_colera.groupByProvinciaFecha$Provincia)
colera_municipios <- as.factor(unique(df_colera.groupByProvinciaFechaMunicipo$Municipio))

EPICENTRE_MAX <- 100


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
    
    if (sum(df_colera.tmp$Total_invasiones[1:13]) > EPICENTRE_MAX && !is.na(sum(df_colera.tmp$Total_invasiones[1:13]))) { # is epicentre (more than 100 invasiones)
      
      print(paste(INVASIONES_STR, "en", c, ":", sum(df_colera.tmp$Total_invasiones[1:13]), sep = " "))
    }
    
  } else if (cause == DEFUNCIONES_STR) {
    
    if (sum(df_colera.tmp$Total_defunciones[1:13]) > EPICENTRE_MAX && !is.na(sum(df_colera.tmp$Total_defunciones[1:13]))) { # is epicentre (more than 100 defunciones)
      
      print(paste(DEFUNCIONES_STR, "en", c, ":", sum(df_colera.tmp$Total_defunciones[1:13]), sep = " "))
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
  
  print(
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
  )
  
  # ggsave(paste0(COLERA_PLOTS_DIR, "/ts.colera_", cause, "_", county, ".png"),
  #   width = 15.7,
  #   height = 4.5,
  #   dpi = 300,
  #   limitsize = TRUE)
  
  return(df_colera.tmp)
  
}


# main --------------------------------------------------------------------


# epicentres & plots ------------------------------------------------------


for (provincia in colera_provincias) { # for each Provincia"
  
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
}


# ARIMA -------------------------------------------------------------------


# original data
ts_invasiones.valencia <- coleraTSbyCounty(df_colera.groupByProvinciaFecha, INVASIONES_STR, "valencia")
# ts_defunciones.valencia <- coleraTSbyCounty(df_colera.groupByProvinciaFecha, DEFUNCIONES_STR, "valencia")

rownames(ts_invasiones.valencia) <- 1:nrow(ts_invasiones.valencia)
# rownames(ts_defunciones.valencia) <- 1:nrow(ts_defunciones.valencia)

# minimal data
ts_invasiones.valencia.min <- ts_invasiones.valencia[1:44,]
ggplot(ts_invasiones.valencia.min, aes(Fecha, Total_invasiones)) + 
  geom_line() + 
  scale_x_date('day') + 
  ylab("número invasiones") +
  xlab("")

# tomar la media móvil semanal, suavizando la serie en algo más estable y, por lo tanto, más predecible.
# ts_invasiones.valencia.min$Total_invasiones_ma = ma(ts_invasiones.valencia.min$Total_invasiones, order=7) # using the clean count with no outliers
# ggplot() +
#   geom_line(data = ts_invasiones.valencia.min, aes(x = Fecha, y = Total_invasiones, colour = "Counts")) +
#   geom_line(data = ts_invasiones.valencia.min, aes(x = Fecha, y = Total_invasiones_ma, colour = "Weekly Moving Average"))  +
#   ylab("número invasiones")

# calculamos el componente estacional del uso de datos stl(). A continuación,hallamos el 
# componente estacional de la serie mediante suavizado y ajusta la serie original restando la estacionalidad.
count_ma = ts(na.omit(ts_invasiones.valencia.min$Total_invasiones), frequency=7)
decomp = stl(count_ma, s.window="periodic")
deseasonal_apartamento <- seasadj(decomp)
plot(decomp)

# no estacionaria
adf.test(count_ma, alternative = "stationary") 
acf(count_ma, main = "")
pacf(count_ma, main = "")

# diferenciación
count_d1 <- diff(deseasonal_apartamento, differences=1)
plot(count_d1)

# estacionaria 
adf.test(count_d1, alternative = "stationary") 
acf(count_d1, main = "ACF for Differenced Series")
pacf(count_d1, main = "PACF for Differenced Series")

# ts_invasiones.valencia.train <- ts(
#   ts_invasiones.valencia$Total_invasiones,
#   start = decimal_date(as.Date("1885-06-18")),
#   end = decimal_date(as.Date("1885-07-31")),
#   frequency = 365
# )
# ts_invasiones.valencia.test <- ts(
#   ts_invasiones.valencia$Total_invasiones,
#   start = decimal_date(as.Date("1885-08-01")),
#   end = decimal_date(as.Date("1885-11-18")),
#   frequency = 365
# )

# ajustamos el modelo
fitARIMA <- auto.arima(deseasonal_apartamento, D=1)

# examinamos los gráficos ACF y PACF para los residuos del modelo
tsdisplay(residuals(fitARIMA), lag.max=10, main="Model Residuals")

# Box.test(fitARIMA$residuals, type = "Ljung-Box") 

# prediccion
forecastARIMA <- forecast(fitARIMA, h=30)
plot(forecastARIMA)
grid()
