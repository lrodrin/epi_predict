library(readxl)
library(data.table)
library(zoo)
library(sf)
library(dplyr)
library(tidyr)
library(tmap)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
TEMPE_PLOTS_DIR <- "tempe_plots"
dir.create(TEMPE_PLOTS_DIR, showWarnings = FALSE)
RAIN_PLOTS_DIR <- "rain_plots"
dir.create(RAIN_PLOTS_DIR, showWarnings = FALSE)
SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)

TEMPE_AND_RAIN_FILENAME <- "Temperaturas 1885 BBVA Leonardo.xlsx"
TEMPE_STR <- "temperatura"
RAIN_STR <- "lluvia"
ANO_STR <- "1885"
DATE_FORMAT <- "%Y-%m-%d"
CODIGOINE_STR <- "CODIGOINE"
NUMMONTHS_STR <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
NAMEMONTHS_STR <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre", "Diciembre")


# functions ---------------------------------------------------------------


create_covariatesTS <- function(df_cov, varcov_col, location, pdir, plabel) {
  #' Create time series plots for covariate data.
  #'
  #' This function generates time series plots for covariate data based on the provided parameters.
  #'
  #' @param df_cov A data frame containing covariate data.
  #' @param varcov_col The name of the column in df_cov to be used for the time series.
  #' @param location The location or locality for which the time series is plotted.
  #' @param pdir The directory where the plot will be saved.
  #' @param plabel The label for the y-axis in the plot.
  #'
  #' @return A time series plot of the specified covariate data.
  
  df_cov.tmp <- subset(df_cov, localidad == location)
  
  #
  # TODO: create ts
  #
  
  # print(
  ggplot(df_cov.tmp, aes(x = mes, y = !!sym(varcov_col))) + 
    geom_line() + 
    ylab(plabel) + 
    ggtitle(paste0(varcov_col, " mensual ", location, ", ", ANO_STR)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_x_continuous(breaks = as.numeric(df_cov.tmp$mes), labels = format(df_cov.tmp$mes, "%b"))
  # )
    
  ggsave(paste0(pdir, "/ts.", varcov_col, "_", location, ".png"), dpi = 300, limitsize = TRUE)
  
}


create_tmap <- function(df_mes, mes, map, var_col, style, coords) {
  #' Create a thematic map using the tmap package.
  #'
  #' This function generates a thematic map using the tmap package. It allows you to visualize
  #' spatial data with various styles and legends.
  #'
  #' @param df_mes A data frame containing spatial data to be plotted.
  #' @param mes The label or title for the map panel.
  #' @param map A shapefile or spatial object used as the background map.
  #' @param var_col The column in the data frame to be used for coloring the map.
  #' @param style The style of coloring for the map.
  #' @param coords The bounding box coordinates (bbox) for the map view.
  #'
  #' @return A thematic map visualization.
  
  return(
    tm_shape(df_mes) +
      tm_polygons(
        col = var_col,
        border.col = NULL,
        title = "",
        palette = "Reds",
        style = style,
        legend.is.portrait = FALSE
      ) +
      tm_shape(map) + tm_borders() +
      tm_layout(
        legend.position =  c("right", "bottom"),
        inner.margins = c(0, 0, 0, 0),
        panel.labels = mes,
        panel.label.size = 1.5, panel.label.color = "black",
        panel.label.bg.color = "gray", panel.label.height = 1.1
      ) +
      tm_view(bbox = coords)
  )
}


# main --------------------------------------------------------------------


# read temperatures, rain and distances
df_temperatures <- read_excel(paste(DATA_DIR, TEMPE_AND_RAIN_FILENAME, sep = "/"), sheet = TEMPE_STR, range = cell_rows(4:83))
df_rain <- read_excel(paste(DATA_DIR, TEMPE_AND_RAIN_FILENAME, sep = "/"), sheet = RAIN_STR, range = cell_rows(4:83))

options(scipen = 999)

df_distances <- read.table(paste(DATA_DIR, "TaulaCovar_2.txt", sep = "/"), sep = ";", header = TRUE, fileEncoding = "UTF-8", quote = "")
df_distances <- df_distances %>% select(COD_INE, COD_PROV, PROVINCIA, NOMBRE_ACT, LONGITUD_E, LATITUD_ET, ALTITUD, CapProv, Dist_CapPr, Dist_Stat, Dist_Rail, Dist_Rius, Dist_Road, Dist_coas, Dist_Port)
colnames(df_distances)[c(4:6, 8:15)] <- c("MUNICIPIO", "LONGITUD", "LATITUD", "isCapProv", "covdist_caprov", "covdist_station", "covdist_rail", "covdist_river", "covdist_road", "covdist_coast", "covdist_port")

# remove NA
df_temperatures <- na.omit(df_temperatures) 
df_rain <- na.omit(df_rain) 

# format
df_temperatures[, 2:13] <- sapply(df_temperatures[, 2:13], as.numeric)
df_rain[, 2:13] <- sapply(df_rain[, 2:13], as.numeric)
df_temperatures$`codigo ine` <- ifelse(nchar(df_temperatures$`codigo ine`) == 4, paste0("0", df_temperatures$`codigo ine`), df_temperatures$`codigo ine`)
df_rain$`codigo ine` <- ifelse(nchar(df_rain$`codigo ine`) == 4, paste0("0", df_rain$`codigo ine`), df_rain$`codigo ine`)

df_distances[c(9:15)] <- round(df_distances[c(9:15)], 2)
df_distances$COD_INE <- as.numeric(substr(as.character(df_distances$COD_INE), 1, 5))
df_distances$PROVINCIA <- gsub(".*/", "", df_distances$PROVINCIA)
df_distances$MUNICIPIO <- gsub("/.*$", "", df_distances$MUNICIPIO)

options(scipen = 000)

# create new column "mes"
df_temperatures.parsed <-
  melt(
    setDT(df_temperatures[, 1:16]),
    id.vars = c("localidad", "longitud", "latitud", "codigo ine"),
    variable.name = "mes",
    value.name = TEMPE_STR
  )
df_rain.parsed <-
  melt(
    setDT(df_rain[, 1:16]),
    id.vars = c("localidad", "longitud", "latitud", "codigo ine"),
    variable.name = "mes",
    value.name = RAIN_STR
  )

# order by "localidad"
df_temperatures.parsed <- df_temperatures.parsed[order(df_temperatures.parsed$localidad),]
df_rain.parsed <- df_rain.parsed[order(df_rain.parsed$localidad),]

# format as character "mes" column
df_temperatures.parsed$mes <- as.character(df_temperatures.parsed$mes)
df_rain.parsed$mes <- as.character(df_rain.parsed$mes)

# rename months from name to number
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[1]] <- paste(ANO_STR, NUMMONTHS_STR[1], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[2]] <- paste(ANO_STR, NUMMONTHS_STR[2], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[3]] <- paste(ANO_STR, NUMMONTHS_STR[3], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[4]] <- paste(ANO_STR, NUMMONTHS_STR[4], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[5]] <- paste(ANO_STR, NUMMONTHS_STR[5], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[6]] <- paste(ANO_STR, NUMMONTHS_STR[6], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[7]] <- paste(ANO_STR, NUMMONTHS_STR[7], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[8]] <- paste(ANO_STR, NUMMONTHS_STR[8], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[9]] <- paste(ANO_STR, NUMMONTHS_STR[9], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[10]] <- paste(ANO_STR, NUMMONTHS_STR[10], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[11]] <- paste(ANO_STR, NUMMONTHS_STR[11], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_STR[12]] <- paste(ANO_STR, NUMMONTHS_STR[12], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[1]] <- paste(ANO_STR, NUMMONTHS_STR[1], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[2]] <- paste(ANO_STR, NUMMONTHS_STR[2], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[3]] <- paste(ANO_STR, NUMMONTHS_STR[3], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[4]] <- paste(ANO_STR, NUMMONTHS_STR[4], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[5]] <- paste(ANO_STR, NUMMONTHS_STR[5], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[6]] <- paste(ANO_STR, NUMMONTHS_STR[6], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[7]] <- paste(ANO_STR, NUMMONTHS_STR[7], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[8]] <- paste(ANO_STR, NUMMONTHS_STR[8], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[9]] <- paste(ANO_STR, NUMMONTHS_STR[9], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[10]] <- paste(ANO_STR, NUMMONTHS_STR[10], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[11]] <- paste(ANO_STR, NUMMONTHS_STR[11], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_STR[12]] <- paste(ANO_STR, NUMMONTHS_STR[12], sep = "-")

# remove NA values
df_temperatures.parsed <- na.omit(df_temperatures.parsed)
df_rain.parsed <- na.omit(df_rain.parsed)

if(.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "English")
} else {
  Sys.setlocale("LC_TIME", "C")
}

# format as yearmon "mes" column
df_temperatures.parsed$mes <- as.yearmon(df_temperatures.parsed$mes) 
df_rain.parsed$mes <- as.yearmon(df_rain.parsed$mes) 

# save temperatures, rain and distances
write.csv(df_temperatures.parsed, "temperatures.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv(df_rain.parsed, "rain.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv(df_distances, "distances.csv", fileEncoding = "UTF-8", row.names = FALSE)

# generate all time series of each "localidad"
tempe_localidades <- unique(df_temperatures.parsed$localidad)
rain_localidades <- unique(df_rain.parsed$localidad)

for (localidad in tempe_localidades) {
  
  create_covariatesTS(df_temperatures.parsed, TEMPE_STR, localidad, TEMPE_PLOTS_DIR, "grados (ºC)")
  
}
for (localidad in rain_localidades) {
  
  create_covariatesTS(df_rain.parsed, RAIN_STR, localidad, RAIN_PLOTS_DIR, "mm")
  
}

# barplots
ggplot(df_temperatures.parsed, aes(x = localidad, y = !!sym(TEMPE_STR), fill = localidad)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ mes, nrow = 3, scales = "free_x") +  
  labs(x = "localidad", y = "grados (ºC)") +
  scale_y_continuous(breaks = seq(-5, 30, 5), limits = c(-5, 30), labels = number) +
  ggtitle(paste0("Temperatura mensual por meses en España, ", ANO_STR)) +
  geom_text(aes(label = temperatura), hjust = -0.2, size = 3, angle = 90) +
  theme_bw(base_size = 10) +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste0(TEMPE_PLOTS_DIR, "/barplot_temperaturesXmunicipios.png"), width = 20, height = 10, dpi = 300, limitsize = TRUE)

ggplot(df_rain.parsed, aes(x = localidad, y = !!sym(RAIN_STR), fill = localidad)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ mes, nrow = 3, scales = "free_x") +
  labs(x = "localidad", y = "mm") +
  scale_y_continuous(breaks = seq(0, 365, 65), limits = c(0, 365), labels = number) +
  ggtitle(paste0("Precipitación mensual por meses en España, ", ANO_STR)) +
  geom_text(aes(label = lluvia), hjust = -0.2, size = 3, angle = 90) +
  theme_bw(base_size = 10) +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste0(RAIN_PLOTS_DIR, "/barplot_lluviaXmunicipios.png"), width = 20, height = 10, dpi = 300, limitsize = TRUE)

if(.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "Catalan_Spain.1252")
} else {
  Sys.setlocale("LC_TIME", "ca_ES.UTF-8")
}

# maps
mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7") # remove Canary Islands
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) # remove "Ceuta" and "Melilla"
head(mapS.municipios)
                      
df_temperatures.parsed$mes <- month(as.POSIXlt(df_temperatures.parsed$mes, format = DATE_FORMAT))
df_rain.parsed$mes <- month(as.POSIXlt(df_rain.parsed$mes, format = DATE_FORMAT))

# merge mapS.municipios with df_temperatures.parsed and df_rain.parsed
df_temperatures.parsed.merged <- merge(mapS.municipios, df_temperatures.parsed, by.x = CODIGOINE_STR, by.y = "codigo ine")
df_rain.parsed.merged <- merge(mapS.municipios, df_rain.parsed, by.x = CODIGOINE_STR, by.y = "codigo ine")
head(df_temperatures.parsed.merged)
head(df_rain.parsed.merged)

for (month in as.integer(NUMMONTHS_STR)) {
  
  map_tempe <-
    create_tmap(
      df_temperatures.parsed.merged[df_temperatures.parsed.merged$mes == month,],
      c(NUMMONTHS_STR[month]), 
      mapS.municipios, 
      TEMPE_STR, 
      "jenks",
      c(min(df_temperatures.parsed.merged$long), min(df_temperatures.parsed.merged$lat), max(df_temperatures.parsed.merged$long), max(df_temperatures.parsed.merged$lat)))
      
  map_rain <-
    create_tmap(
      df_rain.parsed.merged[df_rain.parsed.merged$mes == month,],
      c(NUMMONTHS_STR[month]),
      mapS.municipios,
      RAIN_STR,
      "jenks",
      c(min(df_rain.parsed.merged$long), min(df_rain.parsed.merged$lat), max(df_rain.parsed.merged$long), max(df_rain.parsed.merged$lat)))
  
  
  tmap_save(map_tempe, filename = paste(TEMPE_PLOTS_DIR, paste0("tmap.tempe", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_rain, filename = paste(RAIN_PLOTS_DIR, paste0("tmap.rain", month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")

}


# clean environment -------------------------------------------------------


rm(df_temperatures, df_temperatures.parsed.merged, df_rain, df_rain.parsed.merged, mapS.municipios, map_tempe, map_rain)


# TODO: remember to adapt the months during the cholera epidemic
