library(readxl)
library(ggplot2)
library(data.table)
library(zoo)
library(dplyr)
library(tidyr)
library(tmap)
library(stringi)
library(scales)


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
CODIGO_INE_STR <- "Codigo Ine"
PROVINCIA_STR <- "Provincia"
MUNICIPIO_STR <- "Municipio"
COVTEMP_STR <- "covtemp"
COVPREC_STR <- "covprec"
LONG_STR <- "long"
LAT_STR <- "lat"
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
  
  df_cov.tmp <- subset(df_cov, Municipio == location)
  
  if (varcov_col == COVTEMP_STR) { varcov_name <- "temperature" }
  else { varcov_name <- "rain" }
  

  ggplot(df_cov.tmp, aes(x = Fecha, y = !!sym(varcov_col))) + 
    geom_line() + 
    xlab("Month") +
    ylab(plabel) + 
    ggtitle(paste0("Monthly ", varcov_name, " ", location, ", ", ANO_STR)) +
    scale_x_continuous(breaks = as.numeric(df_cov.tmp$Fecha), labels = format(df_cov.tmp$Fecha, "%b")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

  ggsave(paste0(pdir, "/ts.", varcov_name, "_", location, ".png"), dpi = 300, limitsize = TRUE)
  
}


create_tmap <- function(df_mes, mes, map, var_col, style) {
  #' Create a thematic map using the tmap package.
  #'
  #' This function generates a thematic map using the tmap package. It allows you to visualize
  #' spatial data with various styles and legends.
  #'
  #' @param df_mes A data frame containing spatial data to be plotted.
  #' @param mes The label or title for the map panel.
  #' @param map A shape file or spatial object used as the background map.
  #' @param var_col The column in the data frame to be used for colouring the map.
  #' @param style The style of colouring for the map.
  #'
  #' @return A thematic map visualization.
  
  return(
    tm_shape(df_mes, bbox = map) +
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
      )
  )
}


# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


# read temperatures, rain and distances
df_temperatures <- read_excel(paste(DATA_DIR, TEMPE_AND_RAIN_FILENAME, sep = "/"), sheet = "temperatura", range = cell_rows(4:83))
df_rain <- read_excel(paste(DATA_DIR, TEMPE_AND_RAIN_FILENAME, sep = "/"), sheet = "lluvia", range = cell_rows(4:83))
colnames(df_temperatures)[c(1, 14:16)] <- c(MUNICIPIO_STR, LONG_STR, LAT_STR, CODIGO_INE_STR)
colnames(df_rain)[c(1, 14:16)] <- c(MUNICIPIO_STR, LONG_STR, LAT_STR, CODIGO_INE_STR)

col_classes <- rep(NA, 27)
df_distances <- read.table(paste(DATA_DIR, "TaulaCovar_2.txt", sep = "/"), sep = ";", header = TRUE, fileEncoding = "UTF-8", quote = "", colClasses = col_classes[2] <- "character")
df_distances <- df_distances %>% select(COD_INE, PROVINCIA, NOMBRE_ACT, LONGITUD_E, LATITUD_ET, Dist_CapPr, Dist_Stat, Dist_Rail, Dist_Rius, Dist_Road, Dist_coas, Dist_Port)
colnames(df_distances) <- c(CODIGO_INE_STR, PROVINCIA_STR, MUNICIPIO_STR, LONG_STR, LAT_STR, "covdist_caprov", "covdist_station", "covdist_rail", "covdist_river", "covdist_road", "covdist_coast", "covdist_port")


# data preparation --------------------------------------------------------


# remove NA
df_temperatures <- df_temperatures[!is.na(df_temperatures$Municipio) & !is.na(df_temperatures$`Codigo Ine`), ]
df_rain <- df_rain[!is.na(df_rain$Municipio) & !is.na(df_rain$`Codigo Ine`), ]

# replace all NA values with 0
df_temperatures <- df_temperatures %>% replace(is.na(.), 0)
df_rain <- df_rain %>% replace(is.na(.), 0)

# add the first 0 in "Codigo Ine" for numbers with 4 elements
df_temperatures$`Codigo Ine` <- ifelse(nchar(df_temperatures$`Codigo Ine`) == 4, paste0("0", df_temperatures$`Codigo Ine`), df_temperatures$`Codigo Ine`)
df_rain$`Codigo Ine` <- ifelse(nchar(df_rain$`Codigo Ine`) == 4, paste0("0", df_rain$`Codigo Ine`), df_rain$`Codigo Ine`)

# format
df_distances[, c(4:12)] <- sapply(df_distances[, c(4:12)], as.numeric) # columns to numeric
df_distances$`Codigo Ine` <- substr(as.character(df_distances$`Codigo Ine`), 1, 5) # remove unnecessary numbers of "Codigo Ine"
df_distances$Provincia <- gsub(".*/", "", df_distances$Provincia) # remove Spanish/Catalan names, we keep Spanish names
df_distances$Municipio <- gsub("/.*$", "", df_distances$Municipio)

df_temperatures$Municipio <- tolower(stri_trans_general(df_temperatures$Municipio, "Latin-ASCII")) # remove accents and to lower
df_rain$Municipio <- tolower(stri_trans_general(df_rain$Municipio, "Latin-ASCII")) 
df_distances$Provincia <- tolower(stri_trans_general(df_distances$Provincia, "Latin-ASCII")) 
df_distances$Municipio <- tolower(stri_trans_general(df_distances$Municipio, "Latin-ASCII")) 

# create new column "Fecha"
df_temperatures.parsed <-
  melt(
    setDT(df_temperatures[, 1:16]),
    id.vars = c(MUNICIPIO_STR, LONG_STR, LAT_STR, CODIGO_INE_STR),
    variable.name = "Fecha",
    value.name = COVTEMP_STR
  )
df_rain.parsed <-
  melt(
    setDT(df_rain[, 1:16]),
    id.vars = c(MUNICIPIO_STR, LONG_STR, LAT_STR, CODIGO_INE_STR),
    variable.name = "Fecha",
    value.name = COVPREC_STR
  )


# format as character "Fecha" column
df_temperatures.parsed$Fecha <- as.character(df_temperatures.parsed$Fecha)
df_rain.parsed$Fecha <- as.character(df_rain.parsed$Fecha)

# rename months from name to number
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[1]] <- paste(ANO_STR, NUMMONTHS_STR[1], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[2]] <- paste(ANO_STR, NUMMONTHS_STR[2], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[3]] <- paste(ANO_STR, NUMMONTHS_STR[3], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[4]] <- paste(ANO_STR, NUMMONTHS_STR[4], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[5]] <- paste(ANO_STR, NUMMONTHS_STR[5], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[6]] <- paste(ANO_STR, NUMMONTHS_STR[6], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[7]] <- paste(ANO_STR, NUMMONTHS_STR[7], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[8]] <- paste(ANO_STR, NUMMONTHS_STR[8], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[9]] <- paste(ANO_STR, NUMMONTHS_STR[9], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[10]] <- paste(ANO_STR, NUMMONTHS_STR[10], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[11]] <- paste(ANO_STR, NUMMONTHS_STR[11], sep = "-")
df_temperatures.parsed$Fecha[df_temperatures.parsed$Fecha == NAMEMONTHS_STR[12]] <- paste(ANO_STR, NUMMONTHS_STR[12], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[1]] <- paste(ANO_STR, NUMMONTHS_STR[1], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[2]] <- paste(ANO_STR, NUMMONTHS_STR[2], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[3]] <- paste(ANO_STR, NUMMONTHS_STR[3], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[4]] <- paste(ANO_STR, NUMMONTHS_STR[4], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[5]] <- paste(ANO_STR, NUMMONTHS_STR[5], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[6]] <- paste(ANO_STR, NUMMONTHS_STR[6], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[7]] <- paste(ANO_STR, NUMMONTHS_STR[7], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[8]] <- paste(ANO_STR, NUMMONTHS_STR[8], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[9]] <- paste(ANO_STR, NUMMONTHS_STR[9], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[10]] <- paste(ANO_STR, NUMMONTHS_STR[10], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[11]] <- paste(ANO_STR, NUMMONTHS_STR[11], sep = "-")
df_rain.parsed$Fecha[df_rain.parsed$Fecha == NAMEMONTHS_STR[12]] <- paste(ANO_STR, NUMMONTHS_STR[12], sep = "-")

# order by "Codigo Ine"
df_temperatures.parsed <- df_temperatures.parsed[, c(4, 1, 5, 6, 2:3)]
df_rain.parsed <- df_rain.parsed[, c(4, 1, 5, 6, 2:3)]
df_temperatures.parsed <- df_temperatures.parsed[order(df_temperatures.parsed$`Codigo Ine`),]
df_rain.parsed <- df_rain.parsed[order(df_rain.parsed$`Codigo Ine`),]

if(.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "English")
} else {
  Sys.setlocale("LC_TIME", "C")
}

# format as yearmon "Fecha" column
df_temperatures.parsed$Fecha <- as.yearmon(df_temperatures.parsed$Fecha) 
df_rain.parsed$Fecha <- as.yearmon(df_rain.parsed$Fecha) 

# save temperatures, rain and distances data frames
write.csv(df_temperatures.parsed, "temperatures.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv(df_rain.parsed, "rain.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv(df_distances, "distances.csv", fileEncoding = "UTF-8", row.names = FALSE)


# plots -------------------------------------------------------------------


# generate all time series of each "Municipio"
tempe_municipios <- unique(df_temperatures.parsed$Municipio)
rain_municipios <- unique(df_rain.parsed$Municipio)

for (municipio in tempe_municipios) {
  
  create_covariatesTS(df_temperatures.parsed, COVTEMP_STR, municipio, TEMPE_PLOTS_DIR, "degrees (ºC)")
  
}
for (municipio in rain_municipios) {
  
  create_covariatesTS(df_rain.parsed, COVPREC_STR, municipio, RAIN_PLOTS_DIR, "mm")
  
}

# generate barplots
ggplot(subset(df_temperatures.parsed, covtemp != 0), aes(x = Municipio, y = !!sym(COVTEMP_STR), fill = Municipio)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Fecha, nrow = 3, scales = "free_x") +  
  labs(x = "Municipality", y = "degrees (ºC)") +
  scale_y_continuous(breaks = seq(-5, 30, 5), limits = c(-5, 30), labels = number) +
  ggtitle(paste0("Monthly temperature by month in Spain, ", ANO_STR)) +
  geom_text(aes(label = covtemp), hjust = -0.2, size = 3, angle = 90) +
  theme_bw(base_size = 10) +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste0(TEMPE_PLOTS_DIR, "/barplot_temperaturesXmunicipios.png"), width = 20, height = 10, dpi = 300, limitsize = TRUE)

ggplot(subset(df_rain.parsed, covprec != 0), aes(x = Municipio, y = !!sym(COVPREC_STR), fill = Municipio)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Fecha, nrow = 3, scales = "free_x") +
  labs(x = "Municipality", y = "mm") +
  scale_y_continuous(breaks = seq(0, 365, 65), limits = c(0, 365), labels = number) +
  ggtitle(paste0("Monthly rain by month in Spain, ", ANO_STR)) +
  geom_text(aes(label = covprec), hjust = -0.2, size = 3, angle = 90) +
  theme_bw(base_size = 10) +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste0(RAIN_PLOTS_DIR, "/barplot_lluviaXmunicipios.png"), width = 20, height = 10, dpi = 300, limitsize = TRUE)


# maps --------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7") # remove Canary Islands
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) # remove "Ceuta" and "Melilla"
head(mapS.municipios)
  
# extract the month from yearmon in "Fecha"                    
df_temperatures.parsed$Fecha <- month(as.POSIXlt(df_temperatures.parsed$Fecha, format = DATE_FORMAT))
df_rain.parsed$Fecha <- month(as.POSIXlt(df_rain.parsed$Fecha, format = DATE_FORMAT))

# merge mapS.municipios with df_temperatures.parsed and df_rain.parsed
mapS.temperatures <- merge(mapS.municipios, df_temperatures.parsed, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
mapS.rain <- merge(mapS.municipios, df_rain.parsed, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.temperatures)
head(mapS.rain)

for (month in as.integer(NUMMONTHS_STR[6:11])) {
  
  map_tempe <-
    create_tmap(
      mapS.temperatures[mapS.temperatures$Fecha == month,],
      c(NUMMONTHS_STR[month]), 
      mapS.municipios, 
      COVTEMP_STR, 
      "jenks")
      
  map_rain <-
    create_tmap(
      mapS.rain[mapS.rain$Fecha == month,],
      c(NUMMONTHS_STR[month]),
      mapS.municipios,
      COVPREC_STR,
      "jenks")
  
  
  tmap_save(map_tempe, filename = paste(TEMPE_PLOTS_DIR, paste0("tmap.tempe", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_rain, filename = paste(RAIN_PLOTS_DIR, paste0("tmap.rain", month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")

}

if(.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "Catalan_Spain.1252")
} else {
  Sys.setlocale("LC_TIME", "ca_ES.UTF-8")
}


# clean environment -------------------------------------------------------


rm(df_temperatures, df_rain, df_rain.parsed.merged, mapS.municipios, mapS.temperatures, mapS.rain, map_tempe, map_rain, col_classes)
