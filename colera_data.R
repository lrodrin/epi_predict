library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
PLOTS_DIR <- "plots"
CODIGO_INE_STR <- "Codigo Ine"
FECHA_STR <- "Fecha"
INVASIONES_STR <- "invasiones"
DEFUNCIONES_STR <- "defunciones"
PROVINCIA_STR <- "provincia"
ANO_STR <- "1885"
START_DATE <- "1885-06-18"
END_DATE <- "1885-11-09"


# main --------------------------------------------------------------------


# read "colera" dataset
df_colera <- read_excel(paste(DATA_DIR, "Base colera harmo_codigos.xlsx", sep = "/"),
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
df_colera$Fecha <- as.Date(with(df_colera, paste(ano, mes, dia, sep = "-")), "%Y-%m-%d")
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
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = "day"),
    fill = list(Total_invasiones = 0)
  ) # add missing dates

df_colera_defunciones.groupByFecha <- df_colera_defunciones %>%
  group_by(Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = "day"),
    fill = list(Total_defunciones = 0)
  ) # add missing dates

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
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom")

# save the plot
ggsave(paste(PLOTS_DIR, "colera_total_invasiones&defunciones.png", sep = "/"),
       width = 14,
       height = 4.5,
       dpi = 300,
       limitsize = TRUE)


# PROVINCIA ---------------------------------------------------------------


# group "invasiones" and "defunciones" by "Provincia" and "Fecha" 
df_colera_invasiones.groupByProvinciaFecha <- df_colera_invasiones %>%
  group_by(Provincia, Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) %>%
  na.omit(df_colera_invasiones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = "day"),
    fill = list(Total_invasiones = 0)
  ) # add missing dates

df_colera_defunciones.groupByProvinciaFecha <- df_colera_defunciones %>%
  group_by(Provincia, Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = "day"),
    fill = list(Total_defunciones = 0)
  ) # add missing dates

# merge grouped "invasiones" and "defunciones" as df_colera.groupByProvinciaFecha
df_colera.groupByProvinciaFecha <- merge(df_colera_invasiones.groupByProvinciaFecha, df_colera_defunciones.groupByProvinciaFecha)

# plot total "invasiones" for "Provincia"
ggplot(df_colera.groupByProvinciaFecha, aes(x = Fecha, y = Total_invasiones, group = Provincia, colour = Provincia)) + 
  geom_line() +
  scale_color_discrete(name = PROVINCIA_STR) +
  ylab("número") +
  xlab("día-mes") +
  ggtitle(paste0("total ", INVASIONES_STR, " por ", PROVINCIA_STR, ", ", ANO_STR)) +
  scale_y_continuous(breaks=seq(0, 2000, 200), limits=c(0, 2000)) +
  scale_x_continuous(
    breaks = as.numeric(df_colera.groupByProvinciaFecha[, FECHA_STR]),
    labels = format(df_colera.groupByProvinciaFecha[, FECHA_STR], "%d - %m"),
    expand = c(0,0)
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom")

# save the plot
ggsave(paste(PLOTS_DIR, "colera_total_invasionesXprovincia.png", sep = "/"),
       width = 14,
       height = 7,
       dpi = 300,
       limitsize = TRUE)

# plot total defunciones" for "Provincia"
ggplot(df_colera.groupByProvinciaFecha, aes(x = Fecha, y = Total_defunciones, group = Provincia, colour = Provincia)) + 
  geom_line() +
  scale_color_discrete(name = PROVINCIA_STR) +
  ylab("número") +
  xlab("día-mes") +
  ggtitle(paste0("total ", DEFUNCIONES_STR, " por ", PROVINCIA_STR, ", ", ANO_STR)) +
  scale_y_continuous(breaks=seq(0, 700, 100), limits=c(0, 700)) +
  scale_x_continuous(
    breaks = as.numeric(df_colera.groupByProvinciaFecha[, FECHA_STR]),
    labels = format(df_colera.groupByProvinciaFecha[, FECHA_STR], "%d - %m"),
    expand = c(0,0)
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom")

# save the plot
ggsave(paste(PLOTS_DIR, "colera_total_defuncionesXprovincia.png", sep = "/"),
       width = 14,
       height = 7,
       dpi = 300,
       limitsize = TRUE)
