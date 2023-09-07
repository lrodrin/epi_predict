library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
COLERA_DATA_DIR <- "colera_data"
dir.create(COLERA_DATA_DIR, showWarnings = FALSE)
COLERA_PLOTS_DIR <- "colera_plots"
dir.create(COLERA_PLOTS_DIR, showWarnings = FALSE)

CODIGO_INE_STR <- "Codigo Ine"
FECHA_STR <- "Fecha"
MUNICIPIO_STR <- "Municipio"
INVASIONES_STR <- "invasiones"
DEFUNCIONES_STR <- "defunciones"
PROVINCIA_STR <- "Provincia"
DAY_STR <- "day"
ANO_STR <- "1885"
START_DATE <- "1885-06-18"
END_DATE <- "1885-11-18"
DATE_FORMAT <- "%Y-%m-%d"
TOTAL_POBLACION_STR <- "Total_poblacion"
TOTAL_INVASIONES_STR <- paste("Total", INVASIONES_STR, sep = "_")
TOTAL_DEFUNCIONES_STR <- paste("Total", DEFUNCIONES_STR, sep = "_")
START_MONTH <- 6
END_MONTH <- 11
START_WEEK <- 25
END_WEEK <- 46
WEEK_FORMAT <- "%V"


# functions ---------------------------------------------------------------


mergeData_withPob1887 <- function(df_colera_invasiones, df_colera_defunciones, Pob1887, timevar) {
  #' Merge cholera data with 1887 population data and calculate rates.
  #'
  #' This function merges cholera data ("invasiones" and "defunciones") with 1887 population data,
  #' calculates incidence and mortality rates, and writes the merged data to a CSV file.
  #'
  #' @param df_colera_invasiones Data frame containing "invasiones" data.
  #' @param df_colera_defunciones Data frame containing "defunciones" data.
  #' @param Pob1887 Data frame containing 1887 population data.
  #' @param timevar Time variable used for naming the output CSV file.
  #'
  #' @return A data frame with merged data and calculated rates.
  
  # merge grouped "invasiones" and "defunciones" as df_colera.groupByProvinciaFechaCodigoINE
  df_colera.groupByProvinciaFechaCodigoINE <- merge(df_colera_invasiones, df_colera_defunciones)
  
  # merge df_colera.groupByProvinciaFechaCodigoINE with Pob1887
  df_colera.merged <- merge(df_colera.groupByProvinciaFechaCodigoINE, Pob1887, by = CODIGO_INE_STR)
  
  # order values by "Codigo Ine", "Municipio", "Provincia" and "Fecha"
  df_colera.merged <- df_colera.merged[with(df_colera.merged, order(`Codigo Ine`, Municipio, Provincia, Fecha)),]
  
  # add rates of "invasiones" and "defunciones" as columns "Tasa_incidencia" and "Tasa_mortalidad"
  df_colera.merged$Tasa_incidencia <- (df_colera.merged$Total_invasiones / df_colera.merged$Total_poblacion) * 100 # express the rate per 100 people
  df_colera.merged$Tasa_mortalidad <- (df_colera.merged$Total_defunciones / df_colera.merged$Total_poblacion) * 100
  df_colera.merged$Tasa_incidencia <- round(df_colera.merged$Tasa_incidencia, 4)
  df_colera.merged$Tasa_mortalidad <- round(df_colera.merged$Tasa_mortalidad, 4)
  
  # reorder column names by index
  df_colera.merged <- df_colera.merged[, c(1, 2, 6, 3, 4, 8, 5, 9, 7)]
  
  # change the index numbers
  rownames(df_colera.merged) <- 1:nrow(df_colera.merged)
  
  write.csv(df_colera.merged, paste(COLERA_DATA_DIR, paste0("tasa_colera_totalXmunicipio.", timevar, ".csv"), sep = "/"), row.names = FALSE)
  
  return(df_colera.merged)
  
}


# main --------------------------------------------------------------------


# read "colera" dataset
df_colera <-
  read_excel(paste(DATA_DIR, "Base colera harmo_codigos_newlatitudlongitud.xlsx", sep = "/"),
             sheet = "Capitales_Pueblos")

# remove columns "observaciones_1", "observaciones_2" and "Fichero"
df_colera$observaciones_1 <- NULL
df_colera$observaciones_2 <- NULL
df_colera$Fichero <- NULL

# remove "Codigo Ine" 99999, 99998 and 9999
df_colera <-
  df_colera[(
    !(df_colera[,CODIGO_INE_STR] == "99999") &
      !(df_colera[,CODIGO_INE_STR] == "99998") &
      !(df_colera[,CODIGO_INE_STR] == "9999")
  ), ]

# add column "ano"
df_colera$ano <- ANO_STR

# format "Fecha" as year-month-day
df_colera$Fecha <- as.Date(with(df_colera, paste(ano, mes, dia, sep = "-")), DATE_FORMAT)
df_colera$dia <- NULL
df_colera$mes <- NULL
df_colera$ano <- NULL

# divide dataset for "invasiones" and "defunciones"
rows_odd <- seq_len(nrow(df_colera)) %% 2
df_colera_invasiones <- df_colera[rows_odd == 1,]
df_colera_defunciones <- df_colera[rows_odd == 0,]

# create columns with number of "invasiones" and "defunciones" 
colnames(df_colera_invasiones)[4] = INVASIONES_STR
colnames(df_colera_defunciones)[4] = DEFUNCIONES_STR
df_colera_invasiones$`Causa (Invasion, Defuncion)` <- NULL
df_colera_defunciones$`Causa (Invasion, Defuncion)` <- NULL


# TOTALES -----------------------------------------------------------------


# group "invasiones" and "defunciones" by Fecha" 
df_colera_invasiones.groupByFecha <- df_colera_invasiones %>%
  group_by(Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) %>%
  na.omit(df_colera_invasiones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = DAY_STR),
    fill = list(Total_invasiones = 0)
  ) # add missing dates

df_colera_defunciones.groupByFecha <- df_colera_defunciones %>%
  group_by(Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = DAY_STR),
    fill = list(Total_defunciones = 0)
  ) # add missing dates

# save total "invasiones" and "defunciones" by Fecha" 
write.csv(df_colera_invasiones.groupByFecha, paste(COLERA_DATA_DIR, "colera_total_invasiones.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_colera_defunciones.groupByFecha, paste(COLERA_DATA_DIR, "colera_total_defunciones.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

# merge grouped "invasiones" and "defunciones" as df_colera.groupByFecha
df_colera.groupByFecha <- merge(df_colera_invasiones.groupByFecha, df_colera_defunciones.groupByFecha)

# plot total "invasiones" and "defunciones"
ggplot(df_colera.groupByFecha, aes(Fecha)) + 
  geom_line(aes(y = Total_invasiones, colour = INVASIONES_STR)) +
  geom_line(aes(y = Total_defunciones, colour = DEFUNCIONES_STR)) +
  scale_color_discrete(name = "causa") +
  ylab("número") +
  xlab("día-mes") +
  ggtitle(paste0("total ", INVASIONES_STR, "/", DEFUNCIONES_STR, ", ", ANO_STR)) +
  scale_y_continuous(breaks=seq(0, 7000, 1000), limits=c(0, 7000)) +
  scale_x_continuous(
    breaks = as.numeric(df_colera.groupByFecha[, FECHA_STR]),
    labels = format(df_colera.groupByFecha[, FECHA_STR], "%d - %m"),
    expand = c(0,0)
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom")

# save the plot
ggsave(paste(COLERA_PLOTS_DIR, "colera_total_invasiones&defunciones.png", sep = "/"),
       width = 14,
       height = 4.5,
       dpi = 300,
       limitsize = TRUE)


# TOTALES - PROVINCIA -----------------------------------------------------


# group "invasiones" and "defunciones" by "Provincia" and "Fecha" 
df_colera_invasiones.groupByProvinciaFecha <- df_colera_invasiones %>%
  group_by(Provincia, Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) %>%
  na.omit(df_colera_invasiones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = DAY_STR),
    fill = list(Total_invasiones = 0)
  ) # add missing dates

df_colera_defunciones.groupByProvinciaFecha <- df_colera_defunciones %>%
  group_by(Provincia, Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = DAY_STR),
    fill = list(Total_defunciones = 0)
  ) # add missing dates

# save total "invasiones" and "defunciones" by "Provincia" and "Fecha" 
write.csv(df_colera_invasiones.groupByProvinciaFecha, paste(COLERA_DATA_DIR, "colera_total_invasionesXprovincia.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_colera_defunciones.groupByProvinciaFecha, paste(COLERA_DATA_DIR, "colera_total_defuncionesXprovincia.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

# merge grouped "invasiones" and "defunciones" as df_colera.groupByProvinciaFecha
df_colera.groupByProvinciaFecha <- merge(df_colera_invasiones.groupByProvinciaFecha, df_colera_defunciones.groupByProvinciaFecha)

# add CCAA names in df_colera.groupByProvinciaFecha to order plots by CCAA
df_colera.groupByProvinciaFecha <- df_colera.groupByProvinciaFecha %>% mutate(ccaa = case_when(
    (Provincia %in% c("almeria", "cadiz", "cordoba", "granada", "jaen", "malaga")) ~ "andalucia",
    (Provincia %in% c("huesca", "teruel", "zaragoza")) ~ "aragon",
    (Provincia %in% c("burgos", "palencia", "salamanca", "segovia", "soria", "valladolid", "zamora")) ~ "castilla-y-leon",
    (Provincia %in% c("albacete", "ciudad real", "cuenca", "guadalajara", "toledo")) ~ "castilla-la-mancha", 
    (Provincia %in% c("barcelona", "gerona", "lerida", "tarragona"))  ~ "cataluña",
    (Provincia %in% c("alicante", "castellon", "valencia")) ~ "comunitat-valenciana",
    (Provincia == "badajoz") ~ "extremadura",
    (Provincia == "madrid") ~ "madrid",
    (Provincia == "murcia") ~ "murcia",
    (Provincia == "navarra") ~ "navarra",
    (Provincia == "santander") ~ "cantabria",
    (Provincia == "logroño") ~ "la-rioja"))

# order df_colera.groupByProvinciaFecha by CCAA
df_colera.groupByProvinciaFecha <- with(df_colera.groupByProvinciaFecha, df_colera.groupByProvinciaFecha[order(ccaa),])

# assign "Provincia" as factor with defined levels
df_colera.groupByProvinciaFecha$Provincia <- with(df_colera.groupByProvinciaFecha, factor(as.character(Provincia), levels = unique(Provincia)))

# plot total "invasiones" for "Provincia"
ggplot(df_colera.groupByProvinciaFecha, aes(x = Fecha, y = Total_invasiones, group = Provincia, colour = Provincia)) + 
  geom_line() +
  scale_color_discrete(name = PROVINCIA_STR) +
  ylab("número") +
  xlab("") +
  ggtitle(paste0("total ", INVASIONES_STR, " por ", PROVINCIA_STR, ", ", ANO_STR)) +
  scale_y_continuous(breaks=seq(0, 2000, 350), limits=c(0, 2000)) +
  theme(legend.position = "none") +
  facet_wrap(~ Provincia, scales = 'free_x', ncol = 4) 

# save the plot
ggsave(paste(COLERA_PLOTS_DIR, "colera_total_invasionesXprovincia.png", sep = "/"),
       width = 14,
       height = 9,
       dpi = 300,
       limitsize = TRUE)

# plot total defunciones" for "Provincia"
ggplot(df_colera.groupByProvinciaFecha, aes(x = Fecha, y = Total_defunciones, group = Provincia, colour = Provincia)) + 
  geom_line() +
  scale_color_discrete(name = PROVINCIA_STR) +
  ylab("número") +
  xlab("") +
  ggtitle(paste0("total ", DEFUNCIONES_STR, " por ", PROVINCIA_STR, ", ", ANO_STR)) +
  scale_y_continuous(breaks=seq(0, 600, 150), limits=c(0, 600)) +
  theme(legend.position = "none") +
  facet_wrap(~ Provincia, scales = 'free_x', ncol = 4)

# save the plot
ggsave(paste(COLERA_PLOTS_DIR, "colera_total_defuncionesXprovincia.png", sep = "/"),
       width = 14,
       height = 9,
       dpi = 300,
       limitsize = TRUE)

# bar plots total "invasiones" and "defunciones" by "Provincia" and "Fecha" 
ggplot(df_colera.groupByProvinciaFecha, aes(x = Total_invasiones, y = Provincia, fill = Provincia)) +
  geom_bar(stat = "identity") + 
  ylab(PROVINCIA_STR) +
  xlab(paste("número", INVASIONES_STR, sep = " ")) +
  theme(legend.position="none")

# save the bar plot
ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_invasionesXprovincia.png", sep = "/"), dpi = 300, limitsize = TRUE)

ggplot(df_colera.groupByProvinciaFecha, aes(x = Total_defunciones, y = Provincia, fill = Provincia)) +
  geom_bar(stat = "identity") + 
  ylab(PROVINCIA_STR) +
  xlab(paste("número", DEFUNCIONES_STR, sep = " ")) +
  theme(legend.position="none")

# save the plot
ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_defuncionesXprovincia.png", sep = "/"), dpi = 300, limitsize = TRUE)

# remove column "ccaa"
df_colera.groupByProvinciaFecha$ccaa <- NULL


# TOTALES - MUNICIPIOS ----------------------------------------------------


# group "invasiones" and "defunciones" by "Provincia", "Fecha" and "Municipio"
df_colera_invasiones.groupByProvinciaFechaMunicipio <- df_colera_invasiones %>%
  group_by(Provincia, Municipio, Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) %>%
  na.omit(df_colera_invasiones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = DAY_STR),
    fill = list(Total_invasiones = 0)
  ) # add missing dates

df_colera_defunciones.groupByProvinciaFechaMunicipio <- df_colera_defunciones %>%
  group_by(Provincia, Municipio, Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = DAY_STR),
    fill = list(Total_defunciones = 0)
  ) # add missing dates

# save total "invasiones" and "defunciones" by "Provincia", "Fecha" and "Municipio"
write.csv(df_colera_invasiones.groupByProvinciaFechaMunicipio, paste(COLERA_DATA_DIR, "colera_total_invasionesXmunicipio.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_colera_defunciones.groupByProvinciaFechaMunicipio, paste(COLERA_DATA_DIR, "colera_total_defuncionesXmunicipio.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")

# merge grouped "invasiones" and "defunciones" as df_colera.groupByProvinciaFechaMunicipo
df_colera.groupByProvinciaFechaMunicipo <- merge(df_colera_invasiones.groupByProvinciaFechaMunicipio, df_colera_defunciones.groupByProvinciaFechaMunicipio)


# TASA - MUNICIPIOS -------------------------------------------------------


Pob1887 <- read_excel(paste(DATA_DIR, "poblaciones habitantes municipios de colera.xlsx", sep = "/"))
Pob1887$`habitantes 1887` <- as.numeric(Pob1887$`habitantes 1887`)
Pob1887 <- na.omit(Pob1887)
names(Pob1887) <- c(CODIGO_INE_STR, MUNICIPIO_STR, TOTAL_POBLACION_STR) # change column names

# by weeks

df_colera_invasiones.copy <- df_colera_invasiones
df_colera_defunciones.copy <- df_colera_defunciones

df_colera_invasiones.copy$Fecha <- strftime(df_colera_invasiones.copy$Fecha, format = WEEK_FORMAT)
df_colera_defunciones.copy$Fecha <- strftime(df_colera_defunciones.copy$Fecha, format = WEEK_FORMAT)
df_colera_invasiones.copy$Fecha <- as.numeric(df_colera_invasiones.copy$Fecha)
df_colera_defunciones.copy$Fecha <- as.numeric(df_colera_defunciones.copy$Fecha)

# group "invasiones" and "defunciones" by "Provincia", "Fecha" and "Codigo Ine"
df_colera_invasiones.groupByProvinciaFechaCodigoINE.week <- df_colera_invasiones.copy %>%
  group_by(Provincia, `Codigo Ine`, Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) %>%
  na.omit(df_colera_invasiones) %>%
  complete(
    Fecha = seq(START_WEEK, END_WEEK, by = 1),
    fill = list(Total_invasiones = 0)
  ) # add missing weeks

df_colera_defunciones.groupByProvinciaFechaCodigoINE.week <- df_colera_defunciones.copy %>%
  group_by(Provincia, `Codigo Ine`, Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones) %>%
  complete(
    Fecha = seq(START_WEEK, END_WEEK, by = 1),
    fill = list(Total_defunciones = 0)
  ) # add missing weeks

df_colera.merged.week <- mergeData_withPob1887(
  df_colera_invasiones.groupByProvinciaFechaCodigoINE.week,
  df_colera_defunciones.groupByProvinciaFechaCodigoINE.week,
  Pob1887,
  "week"
)
  
# by months

df_colera_invasiones.copy <- df_colera_invasiones
df_colera_defunciones.copy <- df_colera_defunciones
  
df_colera_invasiones.copy$Fecha <- month(as.POSIXlt(df_colera_invasiones.copy$Fecha, format = DATE_FORMAT))
df_colera_defunciones.copy$Fecha <- month(as.POSIXlt(df_colera_defunciones.copy$Fecha, format = DATE_FORMAT))
df_colera_invasiones.copy$Fecha <- as.numeric(df_colera_invasiones.copy$Fecha)
df_colera_defunciones.copy$Fecha <- as.numeric(df_colera_defunciones.copy$Fecha)

# group "invasiones" and "defunciones" by "Provincia", "Fecha" and "Codigo Ine"
df_colera_invasiones.groupByProvinciaFechaCodigoINE.month <- df_colera_invasiones.copy %>%
  group_by(Provincia, `Codigo Ine`, Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) %>%
  na.omit(df_colera_invasiones) %>%
  complete(
    Fecha = seq(START_MONTH, END_MONTH, by = 1),
    fill = list(Total_invasiones = 0)
  ) # add missing days

df_colera_defunciones.groupByProvinciaFechaCodigoINE.month <- df_colera_defunciones.copy %>%
  group_by(Provincia, `Codigo Ine`, Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones) %>%
  complete(
    Fecha = seq(START_MONTH, END_MONTH, by = 1),
    fill = list(Total_defunciones = 0)
  ) # add missing days

df_colera.merged.month <- mergeData_withPob1887(
  df_colera_invasiones.groupByProvinciaFechaCodigoINE.month,
  df_colera_defunciones.groupByProvinciaFechaCodigoINE.month,
  Pob1887,
  "month"
)

# by days

df_colera_invasiones.copy <- df_colera_invasiones
df_colera_defunciones.copy <- df_colera_defunciones

# group "invasiones" and "defunciones" by "Provincia", "Fecha" and "Codigo Ine"
df_colera_invasiones.groupByProvinciaFechaCodigoINE.day <- df_colera_invasiones.copy %>%
  group_by(Provincia, `Codigo Ine`, Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) %>%
  na.omit(df_colera_invasiones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = DAY_STR),
    fill = list(Total_invasiones = 0)
  ) # add missing days

df_colera_defunciones.groupByProvinciaFechaCodigoINE.day <- df_colera_defunciones.copy %>%
  group_by(Provincia, `Codigo Ine`, Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = DAY_STR),
    fill = list(Total_defunciones = 0)
  ) # add missing days

df_colera.merged.day <- mergeData_withPob1887(
  df_colera_invasiones.groupByProvinciaFechaCodigoINE.day,
  df_colera_defunciones.groupByProvinciaFechaCodigoINE.day,
  Pob1887,
  "day"
)


# clean environment -------------------------------------------------------


rm(df_colera_invasiones.copy, df_colera_defunciones.copy)
