library(readxl)
library(data.table)
library(zoo)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(scales)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
TEMPE_PLOTS_DIR <- "tempe_plots"
dir.create(TEMPE_PLOTS_DIR, showWarnings = FALSE)
SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)

TEMPERATURA_STR <- "temperatura"
TEMPERATURA_FACTOR_STR <- "temperatura_factor"
ANO_STR <- "1885"
DATE_FORMAT <- "%Y-%m-%d"
CODIGOINE_STR <- "CODIGOINE"
NUMMONTHS_LIST <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
NAMEMONTHS_LIST <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")
PLOT_LABELS <- c("low", "mid-low", "mid-high", "high")
PLOT_COLORS_BY_LABELS <- c("low" = "#FEE4D8", "mid-low" = "#FCB195", "mid-high" = "#FB795A", "high" = "#BB1419")


# functions ---------------------------------------------------------------


create_tempeTS <- function(df_temperatures, location) {
  #' Create time series plot for temperature data.
  #'
  #' This function generates a time series plot for temperature data at a specific location.
  #'
  #' @param df_temperatures Data frame containing temperature data.
  #' @param location Location for which to create the time series plot.
  
  df_temperatures.tmp <- subset(df_temperatures, localidad == location)
  
  #
  # TODO: create ts
  #
  
  # print(
  ggplot(df_temperatures.tmp, aes(x = mes, y = temperatura)) + 
    geom_line() + 
    ylab("grados (ºC)") + 
    ggtitle(paste0(TEMPERATURA_STR, " mensual ", location, ", ", ANO_STR)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_x_continuous(breaks = as.numeric(df_temperatures.tmp$mes), labels = format(df_temperatures.tmp$mes, "%b"))
  # )
    
  ggsave(paste0(TEMPE_PLOTS_DIR, "/ts.temperatures_", location, ".png"), dpi = 300, limitsize = TRUE)
  
}


factorize <- function(df_colera, var_col, factor_col, breaks) {
  #' Factorize numeric values into discrete intervals.
  #'
  #' This function takes a numeric column from a data frame and converts it into factors
  #' by dividing it into discrete intervals specified by the parameters.
  #'
  #' @param df_colera Data frame containing the data.
  #' @param var_col Name of the column to be factorized.
  #' @param factor_col Name of the new factor column.
  #' @param breaks Vector defining the interval breakpoints.
  #'
  #' @return The data frame with the new factor column added.
  
  df_colera[[factor_col]] <- cut(df_colera[[var_col]], breaks = breaks, labels = PLOT_LABELS)
  return(df_colera)
  
}


plot_observationsByMonth <- function(df_colera, var_col, var_colname) {
  #' Create a list of plots displaying observations by month on spatial maps.
  #'
  #' This function generates a list of plots, one for each specified month, displaying observations on spatial maps.
  #' It uses the provided data frame and variable column to visualize the observations.
  #'
  #' @param df_colera Data frame containing the observations.
  #' @param var_col Column with numeric values for visualization.
  #' @param var_colname Name of the variable used for colour representation.
  #'
  #' @return A list of plots displaying observations by month on spatial maps.
  
  plot_list <- list()
  
  for (month in as.integer(NUMMONTHS_LIST)) {  
    
    month_data <- df_colera[df_colera$mes == month, ]
    
    plot <- ggplot() +
      geom_sf(data = mapS.municipios, aes(), fill = "white", color = "darkgray") +
      geom_sf(data = month_data, aes(col = !!sym(var_col), fill = !!sym(var_col)), color = "black") +
      geom_text(data = subset(month_data, NAMEUNIT %in% mapS.municipios$NAMEUNIT), aes(x = longitud, y = latitud, label = NAMEUNIT), size = 2, color = "black", nudge_y = 0.1) + 
      scale_fill_manual(values = PLOT_COLORS_BY_LABELS) +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
      labs(color = var_colname, fill = var_colname) +
      theme_void() +
      ggtitle(paste("month:", month))
    
    
    plot_list[[as.character(month)]] <- plot
    
  }
  
  return(plot_list)
  
}


# main --------------------------------------------------------------------


# read temperatures
df_temperatures <- read_excel(paste(DATA_DIR, "Temperaturas 1885 BBVA Leonardo.xlsx", sep = "/"), sheet = TEMPERATURA_STR, range = cell_rows(4:83))

# remove NA
df_temperatures <- na.omit(df_temperatures) 

# change format of month's columns from chr to num
df_temperatures[, 2:13] <- sapply(df_temperatures[, 2:13], as.numeric)

# create new column "mes"
df_temperatures.parsed <-
  melt(
    setDT(df_temperatures[, 1:16]),
    id.vars = c("localidad", "longitud", "latitud", "codigo ine"),
    variable.name = "mes",
    value.name = TEMPERATURA_STR
  )

# order by "localidad"
df_temperatures.parsed <- df_temperatures.parsed[order(df_temperatures.parsed$localidad),]

# format as character "mes" column
df_temperatures.parsed$mes <- as.character(df_temperatures.parsed$mes)

# rename months from name to number
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[1]] <- paste(ANO_STR, NUMMONTHS_LIST[1], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[2]] <- paste(ANO_STR, NUMMONTHS_LIST[2], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[3]] <- paste(ANO_STR, NUMMONTHS_LIST[3], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[4]] <- paste(ANO_STR, NUMMONTHS_LIST[4], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[5]] <- paste(ANO_STR, NUMMONTHS_LIST[5], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[6]] <- paste(ANO_STR, NUMMONTHS_LIST[6], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[7]] <- paste(ANO_STR, NUMMONTHS_LIST[7], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[8]] <- paste(ANO_STR, NUMMONTHS_LIST[8], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[9]] <- paste(ANO_STR, NUMMONTHS_LIST[9], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[10]] <- paste(ANO_STR, NUMMONTHS_LIST[10], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[11]] <- paste(ANO_STR, NUMMONTHS_LIST[11], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[12]] <- paste(ANO_STR, NUMMONTHS_LIST[12], sep = "-")

# remove NA values
df_temperatures.parsed <- na.omit(df_temperatures.parsed)

if(.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "English")
} else {
  Sys.setlocale("LC_TIME", "C")
}

# format as yearmon "mes" column
df_temperatures.parsed$mes <- as.yearmon(df_temperatures.parsed$mes) 

# save temperatures 
write.csv(df_temperatures.parsed, "temperatures.csv", fileEncoding = "UTF-8", row.names = FALSE)

# generate all time series of each "localidad"
tempe_localidades <- unique(df_temperatures.parsed$localidad)

for (localidad in tempe_localidades) {
  
  create_tempeTS(df_temperatures.parsed, localidad)
  
}

# barplot
ggplot(df_temperatures.parsed, aes(x = localidad, y = temperatura, fill = localidad)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ mes, nrow = 3, scales = "free_x") +  
  labs(x = "localidad", y = "grados (ºC)") +
  scale_y_continuous(breaks = seq(-5, 30, 5), limits = c(-5, 30), labels = number) +
  ggtitle(paste0("Temperatura mensual por meses en España, ", ANO_STR)) +
  geom_text(aes(label = temperatura), hjust = -0.2, size = 3, angle = 90) +
  theme_bw(base_size = 10) +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste0(TEMPE_PLOTS_DIR, "/barplot_temperaturesXmunicipios.png"), width = 20, height = 10, dpi = 300, limitsize = TRUE)

if(.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "Catalan_Spain.1252")
} else {
  Sys.setlocale("LC_TIME", "ca_ES.UTF-8")
}


# map ---------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7") # remove Canary Islands
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) # remove "Ceuta" and "Melilla"
head(mapS.municipios)
                      
df_temperatures.parsed$mes <- month(as.POSIXlt(df_temperatures.parsed$mes, format = DATE_FORMAT))
df_temperatures.parsed <- df_temperatures.parsed %>%
  mutate(
    latitud = ifelse(latitud < 36.0, 36.0, ifelse(latitud > 43.0, 43.0, latitud)),
    longitud = ifelse(longitud < -10.0, -10.0, ifelse(longitud > 4.0, 4.0, longitud))
  )

df_temperatures.merged <- merge(mapS.municipios, df_temperatures.parsed, by.x = CODIGOINE_STR, by.y = "codigo ine")
df_temperatures.merged <- factorize(
  df_temperatures.merged,
  TEMPERATURA_STR,
  TEMPERATURA_FACTOR_STR,
  c(-5, 9, 14, 19, Inf)
)

head(df_temperatures.merged)

plot_list.df_temperatures <- plot_observationsByMonth(df_temperatures.merged, TEMPERATURA_FACTOR_STR, TEMPERATURA_STR)

for (month in names(plot_list.df_temperatures)) {
  # print(plot_list.df_temperatures[[month]])
  ggsave(paste0(TEMPE_PLOTS_DIR, "/map_temperaturesXmunicipios_", month, ".png"), plot_list.df_temperatures[[month]], width = 7, height = 7, dpi = 300, limitsize = TRUE)
}


# clean environment -------------------------------------------------------


rm(df_temperatures, df_temperatures.merged, plot_list.df_temperatures, mapS.municipios, factorize, plot_observationsByMonth)
