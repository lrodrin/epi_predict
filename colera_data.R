library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(stringi)


source("covariates.R")


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
COLERA_DATA_DIR <- "colera_data"
dir.create(COLERA_DATA_DIR, showWarnings = FALSE)
COLERA_PLOTS_DIR <- "colera_plots"
dir.create(COLERA_PLOTS_DIR, showWarnings = FALSE)

CODIGO_INE_STR <- "Codigo Ine"
INVASIONES_STR <- "invasiones"
DEFUNCIONES_STR <- "defunciones"
PROVINCIA_STR <- "Provincia"
LONG_STR <- "long"
LAT_STR <- "lat"
ANO_STR <- "1885"
DATE_FORMAT <- "%Y-%m-%d"
TOTAL_POBLACION_STR <- "Total_poblacion"
TOTAL_INVASIONES_STR <- paste("Total", INVASIONES_STR, sep = "_")
TOTAL_DEFUNCIONES_STR <- paste("Total", DEFUNCIONES_STR, sep = "_")


# functions ---------------------------------------------------------------


add_rates <- function(df_colera, timevar) {

  # add rates of "invasiones" and "defunciones" as columns "Tasa_incidencia" and "Tasa_mortalidad"
  df_colera$Tasa_incidencia <- (df_colera$Total_invasiones / df_colera$Total_poblacion) * 100 # express the rate per 100 people
  df_colera$Tasa_mortalidad <- (df_colera$Total_defunciones / df_colera$Total_poblacion) * 100
  df_colera$Tasa_incidencia <- round(df_colera$Tasa_incidencia, 4)
  df_colera$Tasa_mortalidad <- round(df_colera$Tasa_mortalidad, 4)
  
  # reorder column names 
  df_colera <- df_colera[, c(1:6, 10:11, 7:9)]
  
  # save df_colera as CSV
  write.csv(df_colera, paste(COLERA_DATA_DIR, paste0("tasa_colera_totalXmunicipio.", timevar, ".csv"), sep = "/"), row.names = FALSE)
  
  return(df_colera)
  
}


# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


# read "colera" dataset
df_colera <- read_excel(paste(DATA_DIR, "Base colera harmo_codigos_newlatitudlongitud.xlsx", sep = "/"), sheet = "Capitales_Pueblos")
# df_colera.subset <- read_excel(paste(DATA_DIR, "na_df_colera_corregit.xlsx", sep = "/")) # TODO

# read "poblaciones del 1887" dataset
Pob1887 <- read_excel(paste(DATA_DIR, "poblaciones habitantes municipios de colera.xlsx", sep = "/"))[, c(1, 3)]
Pob1887 <- na.omit(Pob1887)
Pob1887$`habitantes 1887` <- as.numeric(Pob1887$`habitantes 1887`)
Pob1887$`Codigo Ine` <- ifelse(nchar(Pob1887$`Codigo Ine`) == 4, paste0("0", Pob1887$`Codigo Ine`), as.character(Pob1887$`Codigo Ine`))
colnames(Pob1887)[2] <- TOTAL_POBLACION_STR


# data preparation --------------------------------------------------------


# remove columns "observaciones_1", "observaciones_2", "Fichero", "Municipio", "LAT_POB" and "LNG_POB"
df_colera[, c("observaciones_1", "observaciones_2", "Fichero", "Municipio", "LAT_POB", "LNG_POB")] <- NULL

# remove "Codigo Ine" 100000, 99999, 99998 and 9999
df_colera <-
  df_colera[(
      !(df_colera[,CODIGO_INE_STR] == "100000") &
      !(df_colera[,CODIGO_INE_STR] == "99999") &
      !(df_colera[,CODIGO_INE_STR] == "99998") &
      !(df_colera[,CODIGO_INE_STR] == "9999")
  ), ]

# remove NA
df_colera <- na.omit(df_colera)

# add column "año"
df_colera$ano <- ANO_STR

# format "Fecha" as year-month-day
df_colera$Fecha <- as.Date(with(df_colera, paste(ano, mes, dia, sep = "-")), DATE_FORMAT)
df_colera$dia <- NULL
df_colera$mes <- NULL
df_colera$ano <- NULL

# fix column "Provincia" using df_distances
df_provincias <- df_distances[, c(1, 3)] %>% group_by(COD_INE)
df_provincias$PROVINCIA <- tolower(stri_trans_general(df_provincias$PROVINCIA, "Latin-ASCII"))
colnames(df_provincias) <- c(CODIGO_INE_STR, PROVINCIA_STR)
df_colera <- df_colera %>% 
  mutate(Provincia = ifelse(`Codigo Ine` %in% df_provincias$`Codigo Ine`, df_provincias$Provincia[match(`Codigo Ine`, df_provincias$`Codigo Ine`)], Provincia))

# format column "municipio_harmo"  
colnames(df_colera)[4] <- "Municipio"
df_colera$Municipio <- tolower(stri_trans_general(df_colera$Municipio, "Latin-ASCII"))
df_colera$Municipio <- gsub("/.*", "", df_colera$Municipio)

# divide dataset for "invasiones" and "defunciones"
rows_odd <- seq_len(nrow(df_colera)) %% 2
df_colera_invasiones <- df_colera[rows_odd == 1,]
df_colera_defunciones <- df_colera[rows_odd == 0,]

# create columns with number of "invasiones" and "defunciones" 
colnames(df_colera_invasiones)[c(3, 6:7)] <- c(INVASIONES_STR, LAT_STR, LONG_STR)
colnames(df_colera_defunciones)[c(3, 6:7)] <- c(DEFUNCIONES_STR, LAT_STR, LONG_STR)
df_colera_invasiones$`Causa (Invasion, Defuncion)` <- NULL
df_colera_defunciones$`Causa (Invasion, Defuncion)` <- NULL

# order df_colera_invasiones and df_colera_defunciones by "Provincia" and "Fecha"
df_colera_invasiones <- df_colera_invasiones[, c(4, 1, 3, 7, 2, 5:6)]
df_colera_defunciones <- df_colera_defunciones[, c(4, 1, 3, 7, 2, 5:6)]
df_colera_invasiones <- df_colera_invasiones[order(df_colera_invasiones$`Codigo Ine`, df_colera_invasiones$Fecha),]
df_colera_defunciones <- df_colera_defunciones[order(df_colera_defunciones$`Codigo Ine`, df_colera_defunciones$Fecha),]

rm(df_provincias, df_colera, rows_odd)


# TOTALES -----------------------------------------------------------------


# group "invasiones" and "defunciones" by Fecha" as "Total_invasiones" and "Total_defunciones"
df_colera_invasiones.groupByFecha <- df_colera_invasiones %>%
  group_by(Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) 

df_colera_defunciones.groupByFecha <- df_colera_defunciones %>%
  group_by(Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) 

# save "Total_invasiones" and "Total_defunciones" by Fecha" 
write.csv(df_colera_invasiones.groupByFecha, paste(COLERA_DATA_DIR, "colera_total_invasiones.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_colera_defunciones.groupByFecha, paste(COLERA_DATA_DIR, "colera_total_defunciones.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

# plot "Total_invasiones" and "Total_defunciones"
ggplot() + 
  geom_line(data = df_colera_invasiones.groupByFecha, aes(Fecha, Total_invasiones, color = "Invasions")) +
  geom_line(data = df_colera_defunciones.groupByFecha, aes(Fecha, Total_defunciones, color = "Deaths")) +
  ylab("Invasions/Deaths") + xlab("Day-Month") +
  ggtitle(paste0("Total of deaths and invasions, ", ANO_STR)) +
  scale_y_continuous(breaks=seq(0, 6000, 100), limits=c(0, 6000)) +
  scale_x_continuous(
    breaks = df_colera_invasiones.groupByFecha$Fecha,
    labels = df_colera_invasiones.groupByFecha$Fecha
  ) +
  scale_color_manual(values = c("Invasions" = "blue", "Deaths" = "black")) + 
  labs(color = "", linetype = "Legend") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")

ggsave(paste(COLERA_PLOTS_DIR, "colera_total_invasiones&defunciones.png", sep = "/"), width = 20, height = 10, dpi = 300, limitsize = TRUE)

rm(df_colera_invasiones.groupByFecha, df_colera_defunciones.groupByFecha)


# TOTALES - PROVINCIA and CCAAA -------------------------------------------


# group "invasiones" and "defunciones" by "Provincia" and "Fecha" as "Total_invasiones" and "Total_defunciones"
df_colera_invasiones.groupByProvinciaFecha <- df_colera_invasiones %>%
  group_by(Provincia, Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) 

df_colera_defunciones.groupByProvinciaFecha <- df_colera_defunciones %>%
  group_by(Provincia, Fecha) %>%
  summarize(Total_defunciones = sum(defunciones))

# save "Total_invasiones" and "Total_defunciones" by "Provincia" and "Fecha" 
write.csv(df_colera_invasiones.groupByProvinciaFecha, paste(COLERA_DATA_DIR, "colera_total_invasionesXprovincia.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_colera_defunciones.groupByProvinciaFecha, paste(COLERA_DATA_DIR, "colera_total_defuncionesXprovincia.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

# merge grouped "Total_invasiones" and "Total_defunciones" as df_colera.groupByProvinciaFecha
df_colera.groupByProvinciaFecha <- merge(df_colera_invasiones.groupByProvinciaFecha, df_colera_defunciones.groupByProvinciaFecha, by = c(PROVINCIA_STR, "Fecha"))
rm(df_colera_invasiones.groupByProvinciaFecha, df_colera_defunciones.groupByProvinciaFecha)

# add CCAA names in df_colera.groupByProvinciaFecha to order plots by CCAA
df_colera.groupByProvinciaFecha <- df_colera.groupByProvinciaFecha %>% mutate(CCAA = case_when(
    (Provincia %in% c("almeria", "cadiz", "cordoba", "granada", "jaen", "malaga")) ~ "andalucia",
    (Provincia %in% c("huesca", "teruel", "zaragoza")) ~ "aragon",
    (Provincia %in% c("avila", "burgos", "palencia", "salamanca", "segovia", "soria", "valladolid", "zamora")) ~ "castilla-y-leon",
    (Provincia %in% c("albacete", "ciudad real", "cuenca", "guadalajara", "toledo")) ~ "castilla-la-mancha", 
    (Provincia %in% c("barcelona", "girona", "lleida", "tarragona"))  ~ "cataluña",
    (Provincia %in% c("alicante", "castellon", "valencia")) ~ "comunitat-valenciana",
    (Provincia %in% c("badajoz", "caceres")) ~ "extremadura",
    (Provincia == "gipuzkoa") ~ "pais-vasco",
    (Provincia == "santa cruz de tenerife") ~ "islas-canarias",
    (Provincia == "madrid") ~ "madrid",
    (Provincia == "murcia") ~ "murcia",
    (Provincia == "navarra") ~ "navarra",
    (Provincia == "cantabria") ~ "cantabria",
    (Provincia == "la rioja") ~ "la-rioja"))

# order df_colera.groupByProvinciaFecha 
df_colera.groupByProvinciaFecha <- df_colera.groupByProvinciaFecha[, c(5, 1:4)]

# plot "Total_invasiones" for "Provincia"
gg_prov <- ggplot(subset(df_colera.groupByProvinciaFecha, !(Provincia %in% c("caceres", "gipuzkoa"))), aes(x = Fecha, y = Total_invasiones, group = Provincia, colour = Provincia)) + 
  geom_line() +
  scale_color_discrete(name = PROVINCIA_STR) +
  ylab("Invasions") + xlab("Day-Month") +
  ggtitle(paste0("Total of invasions for province, ", ANO_STR)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ Provincia, scales = "free_y", ncol = 4) +  
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "colera_total_invasionesXprovincia.png", sep = "/"), gg_prov, width = 20, height = 10, dpi = 300, limitsize = TRUE)

# plot "Total_defunciones" for "Provincia"
gg_prov <- ggplot(subset(df_colera.groupByProvinciaFecha, !(Provincia %in% c("caceres", "gipuzkoa"))), aes(x = Fecha, y = Total_defunciones, group = Provincia, colour = Provincia)) + 
  geom_line() +
  scale_color_discrete(name = PROVINCIA_STR) +
  ylab("Deaths") + xlab("Day-Month") +
  ggtitle(paste0("Total of deaths for province, ", ANO_STR)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ Provincia, scales = "free_y", ncol = 4) +  
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "colera_total_defuncionesXprovincia.png", sep = "/"), gg_prov, width = 20, height = 10, dpi = 300, limitsize = TRUE)

# group "Total_invasiones" and "Total_defunciones" by "CCAA" and "Fecha"
df_colera.groupByCCAAFecha <- df_colera.groupByProvinciaFecha %>%
  group_by(CCAA, Fecha) %>%
  summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones)) 

# plot "Total_invasiones" for "CCAA"
gg_ccaa <- ggplot(subset(df_colera.groupByCCAAFecha, CCAA != "pais-vasco"), aes(x = Fecha, y = Total_invasiones, group = CCAA, colour = CCAA)) + 
  geom_line() +
  scale_color_discrete(name = "CCAA") +
  ylab("Invasions") + xlab("Day-Month") +
  ggtitle(paste0("Total of invasions for autonomous community, ", ANO_STR)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ CCAA, scales = "free_y", ncol = 4) +  
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "colera_total_invasionesXccaa.png", sep = "/"), gg_ccaa, width = 20, height = 10, dpi = 300, limitsize = TRUE)

# plot "Total_defunciones" for "CCAA"
gg_ccaa <- ggplot(subset(df_colera.groupByCCAAFecha, CCAA != "pais-vasco"), aes(x = Fecha, y = Total_defunciones, group = CCAA, colour = CCAA)) + 
  geom_line() +
  scale_color_discrete(name = "CCAA") +
  ylab("Invasions") + xlab("Day-Month") +
  ggtitle(paste0("Total of invasions for autonomous community, ", ANO_STR)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ CCAA, scales = "free_y", ncol = 4) +  
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "colera_total_defuncionesXccaa.png", sep = "/"), gg_ccaa, width = 20, height = 10, dpi = 300, limitsize = TRUE)

# group "Total_invasiones" and "Total_defunciones" by "Provincia or CCAA"
df_colera.groupByProvincia <- df_colera.groupByProvinciaFecha %>%
  group_by(Provincia) %>%
  summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))

df_colera.groupByCCAA <- df_colera.groupByCCAAFecha %>%
  group_by(CCAA) %>%
  summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))

# barplot "Total_invasiones" for "Provincia"
ggplot(df_colera.groupByProvincia, aes(x = Total_invasiones, y = Provincia, fill = Provincia)) +
  geom_bar(stat = "identity") + 
  ylab("Province") + xlab("Invasions") +
  scale_x_continuous(breaks = seq(0, 26150, 1000), limits = c(0, 26150), labels = number) +
  geom_text(aes(label = Total_invasiones), hjust = -0.2, size = 3) + 
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_invasionesXprovincia.png", sep = "/"), width = 15, dpi = 300, limitsize = TRUE)

# barplot "Total_invasiones" for "Provincia"
ggplot(df_colera.groupByProvincia, aes(x = Total_defunciones, y = Provincia, fill = Provincia)) +
  geom_bar(stat = "identity") + 
  ylab("Province") + xlab("Deaths") +
  scale_x_continuous(breaks = seq(0, 12160, 500), limits = c(0, 12160), labels = number) +
  geom_text(aes(label = Total_defunciones), hjust = -0.2, size = 3) + 
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_defuncionesXprovincia.png", sep = "/"), width = 15, dpi = 300, limitsize = TRUE)

# barplot "Total_invasiones" for "CCAA"
ggplot(df_colera.groupByCCAA, aes(x = Total_invasiones, y = CCAA, fill = CCAA)) +
  geom_bar(stat = "identity") + 
  ylab("Autonomous Community") + xlab("Invasions") +
  scale_x_continuous(breaks = seq(0, 48675, 2000), limits = c(0, 48675), labels = number) +
  geom_text(aes(label = Total_invasiones), hjust = -0.2, size = 3) + 
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_invasionesXccaa.png", sep = "/"), width = 15, dpi = 300, limitsize = TRUE)

# barplot "Total_invasiones" for "CCAA"
ggplot(df_colera.groupByCCAA, aes(x = Total_defunciones, y = CCAA, fill = CCAA)) +
  geom_bar(stat = "identity") + 
  ylab("Autonomous Community") + xlab("Deaths") +
  scale_x_continuous(breaks = seq(0, 21980, 1000), limits = c(0, 21980), labels = number) +
  geom_text(aes(label = Total_defunciones), hjust = -0.2, size = 3) + 
  theme_bw() + theme(legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_defuncionesXccaa.png", sep = "/"), width = 15, dpi = 300, limitsize = TRUE)

rm(df_colera.groupByCCAA, df_colera.groupByCCAAFecha, df_colera.groupByProvincia, df_colera.groupByProvinciaFecha, gg_ccaa, gg_prov)


# TOTALES - MUNICIPIOS ----------------------------------------------------


# group "invasiones" and "defunciones" by "Municipio" and "Fecha" as "Total_invasiones" and "Total_defunciones"
df_colera_invasiones.groupByMunicipioFecha <- df_colera_invasiones %>%
  group_by(Municipio, Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) 

df_colera_defunciones.groupByMunicipioFecha <- df_colera_defunciones %>%
  group_by(Municipio, Fecha) %>%
  summarize(Total_defunciones = sum(defunciones))

# save "Total_invasiones" and "Total_defunciones" by "Municipio" and "Fecha" 
write.csv(df_colera_invasiones.groupByMunicipioFecha, paste(COLERA_DATA_DIR, "colera_total_invasionesXmunicipio.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_colera_defunciones.groupByMunicipioFecha, paste(COLERA_DATA_DIR, "colera_total_defuncionesXunicipio.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

rm(df_colera_invasiones.groupByMunicipioFecha, df_colera_defunciones.groupByMunicipioFecha)


# POBLACION ---------------------------------------------------------------


df_colera_invasiones$`Codigo Ine` <- ifelse(nchar(df_colera_invasiones$`Codigo Ine`) == 4, paste0("0", df_colera_invasiones$`Codigo Ine`), as.character(df_colera_invasiones$`Codigo Ine`))
df_colera_defunciones$`Codigo Ine` <- ifelse(nchar(df_colera_defunciones$`Codigo Ine`) == 4, paste0("0", df_colera_defunciones$`Codigo Ine`), as.character(df_colera_defunciones$`Codigo Ine`))

# order values by "Codigo Ine" and "Fecha"
df_colera_invasiones <- df_colera_invasiones[with(df_colera_invasiones, order(`Codigo Ine`, Fecha)),]
df_colera_defunciones <- df_colera_defunciones[with(df_colera_defunciones, order(`Codigo Ine`, Fecha)),]

# merge "invasiones" and "defunciones"
df_colera <- bind_cols(df_colera_invasiones, df_colera_defunciones[, c(DEFUNCIONES_STR)])
df_colera <- df_colera[, c(1:5, 8, 6:7)]
colnames(df_colera)[5:6] <- c(TOTAL_INVASIONES_STR, TOTAL_DEFUNCIONES_STR)

# merge "poblacion"
df_colera <- merge(df_colera, Pob1887, by = CODIGO_INE_STR)
df_colera <- df_colera[, c(1:6, 9, 7:8)]


# TASAS - MUNICIPIOS ------------------------------------------------------


# by weeks

df_colera.merged.week <- df_colera
df_colera.merged.week$Fecha <- strftime(df_colera.merged.week$Fecha, format = "%V")
df_colera.merged.week$Fecha <- as.numeric(df_colera.merged.week$Fecha)
df_colera.merged.week <- df_colera.merged.week %>%
  group_by(`Codigo Ine`, Provincia, Municipio, Fecha, Total_poblacion, lat, long) %>%
  summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))

df_colera.merged.week <- df_colera.merged.week[, c(1:4, 8:9, 5:7)]
df_colera.merged.week <- add_rates(df_colera.merged.week, "week")
  

# by months

df_colera.merged.month <- df_colera
df_colera.merged.month$Fecha <- month(as.POSIXlt(df_colera.merged.month$Fecha, format = DATE_FORMAT))
df_colera.merged.month$Fecha <- as.numeric(df_colera.merged.month$Fecha)
df_colera.merged.month <- df_colera.merged.month %>%
  group_by(`Codigo Ine`, Provincia, Municipio, Fecha, Total_poblacion, lat, long) %>%
  summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))

df_colera.merged.month <- df_colera.merged.month[, c(1:4, 8:9, 5:7)]
df_colera.merged.month <- add_rates(df_colera.merged.month, "month")


# by days

df_colera.merged.day <- df_colera
df_colera.merged.day <- df_colera.merged.day %>%
  group_by(`Codigo Ine`, Provincia, Municipio, Fecha, Total_poblacion, lat, long) %>%
  summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))

df_colera.merged.day <- df_colera.merged.day[, c(1:4, 8:9, 5:7)]
df_colera.merged.day <- add_rates(df_colera.merged.day, "day")


rm(df_colera_invasiones, df_colera_defunciones, df_colera, Pob1887)
