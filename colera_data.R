library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)
library(stringi)


source("covariates.R") # load covariates.R


# constants ---------------------------------------------------------------


COLERA_DATASET <- "Base colera harmo_codigos_newlatitudlongitud.xlsx"
POPULATION_DATASET <- "poblaciones habitantes municipios de colera.xlsx"

DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
COLERA_DATA_DIR <- "colera_data"
dir.create(COLERA_DATA_DIR, showWarnings = FALSE)
COLERA_PLOTS_DIR <- "colera_plots"
dir.create(COLERA_PLOTS_DIR, showWarnings = FALSE)

CODIGO_INE_STR <- "Codigo Ine"
PROVINCIA_STR <- "Provincia"
MUNICIPIO_STR <- "Municipio"
FECHA_STR <- "Fecha"
INVASIONES_STR <- "invasiones"
DEFUNCIONES_STR <- "defunciones"
CASES_STR <- "Cases"
DEATHS_STR <- "Deaths"
LONG_STR <- "long"
LAT_STR <- "lat"
ANO_STR <- "1885"
DATE_FORMAT <- "%Y-%m-%d"
WEEK_FORMAT <- "%V"
FILE_ENCODING <- "UTF-8"
WIDTH <- 20
HEIGHT <- 10
DPI <- 300


# functions ---------------------------------------------------------------


generate_totalsByMonth <- function(df_colera_invasiones, df_colera_defunciones, df_colera_poblacion, timevar) {
    #' Generate totals by month, week or day
    #'
    #' This function generates totals by month, week or day for cholera "invasiones" and "defunciones" data.
    #'
    #' @param df_colera_invasiones A data frame containing cholera "invasiones" data.
    #' @param df_colera_defunciones A data frame containing cholera "defunciones" data.
    #' @param df_colera_poblacion A data frame containing population data.
    #' @param timevar A character string indicating the time interval to be used for aggregating cholera data. Possible values are "months", "weeks" and "days".
    #'
    #' @return A data frame with aggregated cholera data by month or other specified time intervals.

    df_colera_invasiones.copy <- df_colera_invasiones
    df_colera_defunciones.copy <- df_colera_defunciones
    colnames(df_colera_invasiones.copy)[5] <- paste("Total", INVASIONES_STR, sep = "_") # change column name "invasiones" to "Total_invasiones"
    colnames(df_colera_defunciones.copy)[5] <- paste("Total", DEFUNCIONES_STR, sep = "_") # change column name "defunciones" to "Total_defunciones"

    if (timevar == "months") {
      startFecha <- 6
      endFecha <- 11
      byFecha <- 1
      df_colera_invasiones.copy$Fecha <- month(as.POSIXlt(df_colera_invasiones.copy$Fecha, format = DATE_FORMAT)) # extract month from "Fecha"
      df_colera_defunciones.copy$Fecha <- month(as.POSIXlt(df_colera_defunciones.copy$Fecha, format = DATE_FORMAT))
      df_colera_invasiones.copy$Fecha <- as.numeric(df_colera_invasiones.copy$Fecha) # convert month to numeric
      df_colera_defunciones.copy$Fecha <- as.numeric(df_colera_defunciones.copy$Fecha)
    }
    else if (timevar == "weeks") {
      startFecha <- 25
      endFecha <- 46
      byFecha <- 1
      df_colera_invasiones.copy$Fecha <- strftime(df_colera_invasiones.copy$Fecha, format = WEEK_FORMAT) # extract week from "Fecha"
      df_colera_defunciones.copy$Fecha <- strftime(df_colera_defunciones.copy$Fecha, format = WEEK_FORMAT)
      df_colera_invasiones.copy$Fecha <- as.numeric(df_colera_invasiones.copy$Fecha) # convert week to numeric
      df_colera_defunciones.copy$Fecha <- as.numeric(df_colera_defunciones.copy$Fecha)
    }
    else if (timevar == "days") {
      startFecha <- as.Date("1885-06-18")
      endFecha <- as.Date("1885-11-09")
      byFecha <- "day"
    }

    # group "invasiones" and "defunciones" by "Codigo Ine" and "Fecha" as "Total_invasiones" and "Total_defunciones"
    df_colera_invasiones.copy <- df_colera_invasiones.copy %>%
      group_by(`Codigo Ine`, Fecha) %>%
      summarize(Total_invasiones = sum(Total_invasiones)) %>%
      complete(Fecha = seq(startFecha, endFecha, by = byFecha), fill = list(Total_invasiones = 0)) # complete missing dates with 0

    df_colera_defunciones.copy <- df_colera_defunciones.copy %>%
      group_by(`Codigo Ine`, Fecha) %>%
      summarize(Total_defunciones = sum(Total_defunciones)) %>%
      complete(Fecha = seq(startFecha, endFecha, by = byFecha), fill = list(Total_defunciones = 0))

    # merge df_colera_invasiones and df_colera_defunciones by "Codigo Ine" and "Fecha" as df_colera
    df_colera <- merge(df_colera_invasiones.copy, df_colera_defunciones.copy, by = c(CODIGO_INE_STR, FECHA_STR))

    # merge df_colera and df_colera_poblacion by "Codigo Ine" as df_colera.merged
    df_colera.merged <- merge(df_colera, df_colera_poblacion, by = CODIGO_INE_STR)

    return(df_colera.merged)
}


# main --------------------------------------------------------------------


# read data ---------------------------------------------------------------


# read "colera" dataset
df_colera <- read_excel(paste(DATA_DIR, COLERA_DATASET, sep = "/"), sheet = "Capitales_Pueblos")

# read "poblacion" dataset
Pob1887 <- read_excel(paste(DATA_DIR, POPULATION_DATASET, sep = "/"))[, c(1, 3)]
Pob1887$`habitantes 1887` <- as.numeric(Pob1887$`habitantes 1887`) # convert "habitantes 1887" to numeric
colnames(Pob1887)[2] <- "Total_poblacion" # change column name "habitantes 1887" to "Total_poblacion"


# data preparation --------------------------------------------------------


# remove columns "observaciones_1", "observaciones_2", "Fichero", "Municipio", "LAT_POB" and "LNG_POB"
df_colera[, c("observaciones_1", "observaciones_2", "Fichero", MUNICIPIO_STR, "LAT_POB", "LNG_POB")] <- NULL

# remove rows with "Codigo Ine" equal to "100000", "99999", "99998" and "9999"
df_colera <-
  df_colera[(
    !(df_colera[,CODIGO_INE_STR] == "100000") &
      !(df_colera[,CODIGO_INE_STR] == "99999") &
      !(df_colera[,CODIGO_INE_STR] == "99998") &
      !(df_colera[,CODIGO_INE_STR] == "9999")
  ), ]

# remove rows with "Codigo Ine" equal to "0"
df_colera$`Codigo Ine` <- ifelse(nchar(df_colera$`Codigo Ine`) == 4, paste0("0", df_colera$`Codigo Ine`), as.character(df_colera$`Codigo Ine`))
Pob1887$`Codigo Ine` <- ifelse(nchar(Pob1887$`Codigo Ine`) == 4, paste0("0", Pob1887$`Codigo Ine`), as.character(Pob1887$`Codigo Ine`))

# remove rows with "Codigo Ine" equal to "0"
df_colera <- na.omit(df_colera)
Pob1887 <- na.omit(Pob1887)

df_colera$ano <- ANO_STR # add column "ano" with year "1885"
df_colera$Fecha <- as.Date(with(df_colera, paste(ano, mes, dia, sep = "-")), DATE_FORMAT) # format "Fecha" as year-month-day
df_colera$dia <- NULL # remove column "dia"
df_colera$mes <- NULL # remove column "mes"
df_colera$ano <- NULL # remove column "ano"

colnames(df_colera)[4] <- MUNICIPIO_STR # change column name "poblacion" to "Municipio"
df_colera$Municipio <- tolower(stri_trans_general(df_colera$Municipio, "Latin-ASCII")) # remove accents and special characters
df_colera$Municipio <- gsub("/.*", "", df_colera$Municipio) # remove "/" and everything after it

# divide df_colera by "Causa (Invasion, Defuncion)" as df_colera_invasiones and df_colera_defunciones
df_colera_invasiones <- subset(df_colera, `Causa (Invasion, Defuncion)` == INVASIONES_STR)
df_colera_defunciones <- subset(df_colera, `Causa (Invasion, Defuncion)` == DEFUNCIONES_STR)

# rename columns "invasiones" and "defunciones" as "Total_invasiones" and "Total_defunciones" and remove column "Causa (Invasion, Defuncion)"
colnames(df_colera_invasiones)[c(3, 6:7)] <- c(INVASIONES_STR, LAT_STR, LONG_STR)
colnames(df_colera_defunciones)[c(3, 6:7)] <- c(DEFUNCIONES_STR, LAT_STR, LONG_STR)
df_colera_invasiones$`Causa (Invasion, Defuncion)` <- NULL
df_colera_defunciones$`Causa (Invasion, Defuncion)` <- NULL

# order columns by "Codigo Ine", "Fecha", "Provincia", "Municipio", "invasiones", "lat" and "long"
df_colera_invasiones <- df_colera_invasiones[, c(4, 1, 3, 7, 2, 5:6)]
df_colera_defunciones <- df_colera_defunciones[, c(4, 1, 3, 7, 2, 5:6)]
df_colera_invasiones <- df_colera_invasiones[order(df_colera_invasiones$`Codigo Ine`, df_colera_invasiones$Fecha),]
df_colera_defunciones <- df_colera_defunciones[order(df_colera_defunciones$`Codigo Ine`, df_colera_defunciones$Fecha),]


# TOTALES -----------------------------------------------------------------


# group "invasiones" and "defunciones" by "Fecha" as "Total_invasiones" and "Total_defunciones"
df_colera_invasiones.groupByFecha <- df_colera_invasiones %>% group_by(Fecha) %>% summarize(Total_invasiones = sum(invasiones))
df_colera_defunciones.groupByFecha <- df_colera_defunciones %>% group_by(Fecha) %>% summarize(Total_defunciones = sum(defunciones))

# save total "invasiones" and "defunciones" by "Fecha"
write.csv(df_colera_invasiones.groupByFecha, paste(COLERA_DATA_DIR, "colera_total_invasiones.csv", sep = "/"), row.names = FALSE, fileEncoding = FILE_ENCODING)
write.csv(df_colera_defunciones.groupByFecha, paste(COLERA_DATA_DIR, "colera_total_defunciones.csv", sep = "/"), row.names = FALSE, fileEncoding = FILE_ENCODING)

# plot "Total_invasiones" and "Total_defunciones" by "Fecha"
ggplot() +
  geom_line(data = df_colera_invasiones.groupByFecha, aes(Fecha, Total_invasiones, color = CASES_STR)) +
  geom_line(data = df_colera_defunciones.groupByFecha, aes(Fecha, Total_defunciones, color = DEATHS_STR)) +
  ylab(paste0(CASES_STR, "/", DEATHS_STR)) + xlab("") +
  scale_y_continuous(breaks = seq(0, 6070, 100), limits = c(0, 6070)) +
  scale_x_continuous(
    breaks = df_colera_invasiones.groupByFecha$Fecha,
    labels = df_colera_invasiones.groupByFecha$Fecha
  ) +
  scale_color_manual(values = c(CASES_STR = "blue", DEATHS_STR = "black")) +
  labs(color = "", linetype = "Legend") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")

ggsave(paste(COLERA_PLOTS_DIR, "colera_total_invasiones&defunciones.png", sep = "/"), width = WIDTH, height = HEIGHT, dpi = DPI, limitsize = TRUE) # save plot


# TOTALES - PROVINCIA and CCAA --------------------------------------------


# group "invasiones" and "defunciones" by "Provincia" and "Fecha" as "Total_invasiones" and "Total_defunciones"
df_colera_invasiones.groupByProvinciaFecha <- df_colera_invasiones %>% group_by(Provincia, Fecha) %>% summarize(Total_invasiones = sum(invasiones))
df_colera_defunciones.groupByProvinciaFecha <- df_colera_defunciones %>% group_by(Provincia, Fecha) %>% summarize(Total_defunciones = sum(defunciones))

# save "Total_invasiones" and "Total_defunciones" by "Provincia" and "Fecha"
write.csv(df_colera_invasiones.groupByProvinciaFecha, paste(COLERA_DATA_DIR, "colera_total_invasionesXprovincia.csv", sep = "/"), row.names = FALSE, fileEncoding = FILE_ENCODING)
write.csv(df_colera_defunciones.groupByProvinciaFecha, paste(COLERA_DATA_DIR, "colera_total_defuncionesXprovincia.csv", sep = "/"), row.names = FALSE, fileEncoding = FILE_ENCODING)

# merge "Total_invasiones" and "Total_defunciones" by "Provincia" and "Fecha" as df_colera.groupByProvinciaFecha
df_colera.groupByProvinciaFecha <- merge(df_colera_invasiones.groupByProvinciaFecha, df_colera_defunciones.groupByProvinciaFecha, by = c(PROVINCIA_STR, FECHA_STR))

# add column "CCAA" to df_colera.groupByProvinciaFecha
df_colera.groupByProvinciaFecha <- df_colera.groupByProvinciaFecha %>% mutate(CCAA = case_when(
  (Provincia %in% c("almeria", "cadiz", "cordoba", "granada", "jaen", "malaga")) ~ "andalucia",
  (Provincia %in% c("huesca", "teruel", "zaragoza")) ~ "aragon",
  (Provincia %in% c("avila", "burgos", "palencia", "salamanca", "segovia", "soria", "valladolid", "zamora")) ~ "castilla-y-leon",
  (Provincia %in% c("albacete", "ciudad real", "cuenca", "guadalajara", "toledo")) ~ "castilla-la-mancha",
  (Provincia %in% c("barcelona", "gerona", "lerida", "tarragona"))  ~ "cataluña",
  (Provincia %in% c("alicante", "castellon", "valencia")) ~ "comunitat-valenciana",
  (Provincia %in% c("badajoz", "caceres")) ~ "extremadura",
  (Provincia == "gipuzkoa") ~ "pais-vasco",
  (Provincia == "santa cruz de tenerife") ~ "islas-canarias",
  (Provincia == "madrid") ~ "madrid",
  (Provincia == "murcia") ~ "murcia",
  (Provincia == "navarra") ~ "navarra",
  (Provincia == "cantabria") ~ "cantabria",
  (Provincia == "la rioja") ~ "la-rioja"))

# order columns by "CCAA", "Provincia", "Fecha", "Total_invasiones" and "Total_defunciones"
df_colera.groupByProvinciaFecha <- df_colera.groupByProvinciaFecha[, c(5, 1:4)]

# plot "Total_invasiones" for "Provincia"
gg_prov <- ggplot(subset(df_colera.groupByProvinciaFecha, !(Provincia %in% c("avila"))), aes(x = Fecha, y = Total_invasiones, group = Provincia, colour = Provincia)) +
  geom_line() +
  scale_color_discrete(name = PROVINCIA_STR) +
  ylab(CASES_STR) + xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ Provincia, scales = "free_y", ncol = 4) +
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "colera_total_invasionesXprovincia.png", sep = "/"), gg_prov, width = WIDTH, height = HEIGHT, dpi = DPI, limitsize = TRUE) # save plot

# plot "Total_defunciones" for "Provincia"
gg_prov <- ggplot(subset(df_colera.groupByProvinciaFecha, !(Provincia %in% c("avila"))), aes(x = Fecha, y = Total_defunciones, group = Provincia, colour = Provincia)) +
  geom_line() +
  scale_color_discrete(name = PROVINCIA_STR) +
  ylab(DEATHS_STR) + xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ Provincia, scales = "free_y", ncol = 4) +
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "colera_total_defuncionesXprovincia.png", sep = "/"), gg_prov, width = WIDTH, height = HEIGHT, dpi = DPI, limitsize = TRUE)

# group "Total_invasiones" and "Total_defunciones" by "CCAA" and "Fecha" as "Total_invasiones" and "Total_defunciones"
df_colera.groupByCCAAFecha <- df_colera.groupByProvinciaFecha %>% group_by(CCAA, Fecha) %>% summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))

# plot "Total_invasiones" for "CCAA"
gg_ccaa <- ggplot(subset(df_colera.groupByCCAAFecha, CCAA != "pais-vasco"), aes(x = Fecha, y = Total_invasiones, group = CCAA, colour = CCAA)) +
  geom_line() +
  scale_color_discrete(name = "CCAA") +
  ylab("Invasions") + xlab("Day-Month") +
  ggtitle(paste0("Total of invasions for autonomous community, ", ANO_STR)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ CCAA, scales = "free_y", ncol = 4) +
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "colera_total_invasionesXccaa.png", sep = "/"), gg_ccaa, width = WIDTH, height = HEIGHT, dpi = DPI, limitsize = TRUE)

# plot "Total_defunciones" for "CCAA"
gg_ccaa <- ggplot(subset(df_colera.groupByCCAAFecha, CCAA != "pais-vasco"), aes(x = Fecha, y = Total_defunciones, group = CCAA, colour = CCAA)) +
  geom_line() +
  scale_color_discrete(name = "CCAA") +
  ylab("Invasions") + xlab("Day-Month") +
  ggtitle(paste0("Total of invasions for autonomous community, ", ANO_STR)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ CCAA, scales = "free_y", ncol = 4) +
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "colera_total_defuncionesXccaa.png", sep = "/"), gg_ccaa, width = WIDTH, height = HEIGHT, dpi = DPI, limitsize = TRUE)

# group "Total_invasiones" and "Total_defunciones" by "Provincia" and "CCAA" as "Total_invasiones" and "Total_defunciones"
df_colera.groupByProvincia <- df_colera.groupByProvinciaFecha %>% group_by(Provincia) %>% summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))
df_colera.groupByCCAA <- df_colera.groupByCCAAFecha %>% group_by(CCAA) %>% summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))

# barplot "Total_invasiones" for "Provincia"
ggplot(df_colera.groupByProvincia, aes(x = Total_invasiones, y = Provincia, fill = Provincia)) +
  geom_bar(stat = "identity") +
  ylab("Province") + xlab(CASES_STR) +
  scale_x_continuous(breaks = seq(0, max(df_colera.groupByProvincia$Total_invasiones), 1000), limits = c(0, max(df_colera.groupByProvincia$Total_invasiones)), labels = number) +
  geom_text(aes(label = Total_invasiones), hjust = -0.2, size = 3) +
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_invasionesXprovincia.png", sep = "/"), width = 15, dpi = DPI, limitsize = TRUE)

# barplot "Total_defunciones" for "Provincia"
ggplot(df_colera.groupByProvincia, aes(x = Total_defunciones, y = Provincia, fill = Provincia)) +
  geom_bar(stat = "identity") +
  ylab("Province") + xlab(DEATHS_STR) +
  scale_x_continuous(breaks = seq(0, max(df_colera.groupByProvincia$Total_defunciones), 500), limits = c(0, max(df_colera.groupByProvincia$Total_defunciones)), labels = number) +
  geom_text(aes(label = Total_defunciones), hjust = -0.2, size = 3) +
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_defuncionesXprovincia.png", sep = "/"), width = 15, dpi = DPI, limitsize = TRUE)

# barplot "Total_invasiones" for "CCAA"
ggplot(df_colera.groupByCCAA, aes(x = Total_invasiones, y = CCAA, fill = CCAA)) +
  geom_bar(stat = "identity") +
  ylab("Autonomous Community") + xlab("Invasions") +
  scale_x_continuous(breaks = seq(0, max(df_colera.groupByCCAA$Total_invasiones), 2000), limits = c(0, max(df_colera.groupByCCAA$Total_invasiones)), labels = number) +
  geom_text(aes(label = Total_invasiones), hjust = -0.2, size = 3) +
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_invasionesXccaa.png", sep = "/"), width = 15, dpi = DPI, limitsize = TRUE)

# barplot "Total_defunciones" for "CCAA"
ggplot(df_colera.groupByCCAA, aes(x = Total_defunciones, y = CCAA, fill = CCAA)) +
  geom_bar(stat = "identity") +
  ylab("Autonomous Community") + xlab(DEATHS_STR) +
  scale_x_continuous(breaks = seq(0, max(df_colera.groupByCCAA$Total_invasiones), 1000), limits = c(0, 33835), labels = number) +
  geom_text(aes(label = Total_defunciones), hjust = -0.2, size = 3) +
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_defuncionesXccaa.png", sep = "/"), width = 15, dpi = DPI, limitsize = TRUE)


# TOTALES - MUNICIPIOS ----------------------------------------------------


# group "invasiones" and "defunciones" by "Municipio" and "Fecha" as "Total_invasiones" and "Total_defunciones"
df_colera_invasiones.groupByMunicipioFecha <- df_colera_invasiones %>% group_by(Municipio, Fecha) %>% summarize(Total_invasiones = sum(invasiones))
df_colera_defunciones.groupByMunicipioFecha <- df_colera_defunciones %>% group_by(Municipio, Fecha) %>% summarize(Total_defunciones = sum(defunciones))

# save "Total_invasiones" and "Total_defunciones" by "Municipio" and "Fecha"
write.csv(df_colera_invasiones.groupByMunicipioFecha, paste(COLERA_DATA_DIR, "colera_total_invasionesXmunicipio.csv", sep = "/"), row.names = FALSE, fileEncoding = FILE_ENCODING)
write.csv(df_colera_defunciones.groupByMunicipioFecha, paste(COLERA_DATA_DIR, "colera_total_defuncionesXmunicipio.csv", sep = "/"), row.names = FALSE, fileEncoding = FILE_ENCODING)


# TOTALES - MUNICIPIOS and POBLACION --------------------------------------


# create three data frames with "invasiones", "defunciones" and "poblacion" by month, week and day
df_colera.merged.month <- generate_totalsByMonth(df_colera_invasiones, df_colera_defunciones, Pob1887, "months")
df_colera.merged.week <- generate_totalsByMonth(df_colera_invasiones, df_colera_defunciones, Pob1887, "weeks")
df_colera.merged.day <- generate_totalsByMonth(df_colera_invasiones, df_colera_defunciones, Pob1887, "days")

save.image("colera_data.RData") # save workspace

rm(list = ls()) # remove all objects from workspace
