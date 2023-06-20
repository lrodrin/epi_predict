library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyverse)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
COLERA_PLOTS_DIR <- "colera_plots"
dir.create(COLERA_PLOTS_DIR, showWarnings = FALSE)

CODIGO_INE_STR <- "Codigo Ine"
FECHA_STR <- "Fecha"
MUNICIPIO_STR <- "Municipio"
INVASIONES_STR <- "invasiones"
DEFUNCIONES_STR <- "defunciones"
PROVINCIA_STR <- "Provincia"
ANO_STR <- "1885"
START_DATE <- "1885-06-18"
END_DATE <- "1885-11-18"


# main --------------------------------------------------------------------


# read "colera" dataset
df_colera <-
  read_excel(paste(DATA_DIR, "Base colera harmo_codigos CCAA.xlsx", sep = "/"),
             sheet = "Capitales_Pueblos")

# remove columns "observaciones_1", "observaciones_2" and "Fichero"
df_colera$observaciones_1 <- NULL
df_colera$observaciones_2 <- NULL
df_colera$Fichero <- NULL

# remove "Codigo Ine" 99999, 99998 and 9999
df_colera <-
  df_colera[(
    !(df_colera[,CODIGO_INE_STR] == "99999.0") &
      !(df_colera[,CODIGO_INE_STR] == "99998.0") &
      !(df_colera[,CODIGO_INE_STR] == "9999.0")
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

# save total "invasiones" and "defunciones" by Fecha" 
write.csv(df_colera_invasiones.groupByFecha, paste(DATA_DIR, "colera_total_invasiones.csv", sep = "/"), row.names = FALSE)
write.csv(df_colera_defunciones.groupByFecha, paste(DATA_DIR, "colera_total_defunciones.csv", sep = "/"), row.names = FALSE)

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

# save total "invasiones" and "defunciones" by "Provincia" and "Fecha" 
write.csv(df_colera_invasiones.groupByProvinciaFecha, paste(DATA_DIR, "colera_total_invasionesXprovincia.csv", sep = "/"), row.names = FALSE)
write.csv(df_colera_defunciones.groupByProvinciaFecha, paste(DATA_DIR, "colera_total_defuncionesXprovincia.csv", sep = "/"), row.names = FALSE)

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
    (Provincia == "badajoz") ~ "extrenadura",
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
  # scale_x_continuous(
  #   breaks = as.numeric(df_colera.groupByProvinciaFecha[, FECHA_STR]),
  #   labels = format(df_colera.groupByProvinciaFecha[, FECHA_STR], "%d - %m"),
  #   expand = c(0,0)
  # ) +
  # theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none") +
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
  # scale_x_continuous(
  #   breaks = as.numeric(df_colera.groupByProvinciaFecha[, FECHA_STR]),
  #   labels = format(df_colera.groupByProvinciaFecha[, FECHA_STR], "%d - %m"),
  #   expand = c(0,0)
  # ) +
  # theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom") +
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
  xlab("número invasiones") +
  theme(legend.position="none")

# save the bar plot
ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_invasionesXprovincia.png", sep = "/"), dpi = 300, limitsize = TRUE)

ggplot(df_colera.groupByProvinciaFecha, aes(x = Total_defunciones, y = Provincia, fill = Provincia)) +
  geom_bar(stat = "identity") + 
  ylab(PROVINCIA_STR) +
  xlab("número defunciones") +
  theme(legend.position="none")

# save the plot
ggsave(paste(COLERA_PLOTS_DIR, "barplot.colera_total_defuncionesXprovincia.png", sep = "/"), dpi = 300, limitsize = TRUE)

# remove column "ccaa"
df_colera.groupByProvinciaFecha$ccaa <- NULL


# # TOTALES - MUNICIPIOS ----------------------------------------------------
# 
# 
# # group "invasiones" and "defunciones" by "Provincia", "Fecha" and "Municipio" 
# df_colera_invasiones.groupByProvinciaFechaMunicipio <- df_colera_invasiones %>%
#   group_by(Provincia, Municipio, Fecha) %>%
#   summarize(Total_invasiones = sum(invasiones)) %>%
#   na.omit(df_colera_invasiones) %>%
#   complete(
#     Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = "day"),
#     fill = list(Total_invasiones = 0)
#   ) # add missing dates
# 
# df_colera_defunciones.groupByProvinciaFechaMunicipio <- df_colera_defunciones %>%
#   group_by(Provincia, Municipio, Fecha) %>%
#   summarize(Total_defunciones = sum(defunciones)) %>%
#   na.omit(df_colera_defunciones) %>%
#   complete(
#     Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = "day"),
#     fill = list(Total_defunciones = 0)
#   ) # add missing dates
# 
# # save total "invasiones" and "defunciones" by "Provincia", "Fecha" and "Municipio"  
# write.csv(df_colera_invasiones.groupByProvinciaFechaMunicipio, paste(DATA_DIR, "colera_total_invasionesXmunicipio.csv", sep = "/"), row.names = FALSE)
# write.csv(df_colera_defunciones.groupByProvinciaFechaMunicipio, paste(DATA_DIR, "colera_total_defuncionesXmunicipio.csv", sep = "/"), row.names = FALSE)
