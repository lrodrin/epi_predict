library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
PLOTS_DIR <- "plots"
CODIGO_INE_STR <- "Codigo Ine"
FECHA_STR <- "Fecha"
INVASIONES_STR <- "invasiones"
DEFUNCIONES_STR <- "defunciones"
AÑO_STR <- "1885"


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

# add column "año"
df_colera$año <- AÑO_STR

# format "Fecha" as year-month-day
df_colera$Fecha <- as.Date(with(df_colera, paste(año, mes, dia, sep = "-")), "%Y-%m-%d")
df_colera$dia <- NULL
df_colera$mes <- NULL
df_colera$año <- NULL

# divide dataset for "invasiones" and "defunciones"
rows_odd <- seq_len(nrow(df_colera)) %% 2
df_colera_invasiones <- df_colera[rows_odd == 1,]
df_colera_defunciones <- df_colera[rows_odd == 0,]

# create columns with number of "invasiones" and "defunciones" 
colnames(df_colera_invasiones)[4] = INVASIONES_STR
colnames(df_colera_defunciones)[4] = DEFUNCIONES_STR
df_colera_invasiones$`Causa (Invasion, Defuncion)` <- NULL
df_colera_defunciones$`Causa (Invasion, Defuncion)` <- NULL

# df_colera_invasiones$invasiones <- df_colera_defunciones$invasiones
# df_colera_invasiones <- df_colera_invasiones[, c(1, 2, 4, 5, 6, 7, 8, 9, 3)]

# df_colera_invasiones[order(df_colera_invasiones[,5], df_colera_invasiones[,8]), ]
# df_colera_defunciones[order(df_colera_defunciones[,5], df_colera_defunciones[,8]), ]


# TODO: aggregate by "Provincia"


# group "invasiones" and "defunciones" by "Fecha"
df_colera_invasiones.grouped <- df_colera_invasiones %>%
  group_by(Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) %>%
  na.omit(df_colera_invasiones)

df_colera_defunciones.grouped <- df_colera_defunciones %>%
  group_by(Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones)

# merge grouped "invasiones" and "defunciones" as df_colera.grouped
df_colera.grouped <- merge(df_colera_invasiones.grouped, df_colera_defunciones.grouped)

# plot "invasiones" and "defunciones"
ggplot(df_colera.grouped, aes(Fecha)) + 
  geom_line(aes(y = Total_invasiones, colour = INVASIONES_STR)) +
  geom_line(aes(y = Total_defunciones, colour = DEFUNCIONES_STR)) +
  scale_color_discrete(name = "causa") +
  ylab("número") +
  xlab("día-mes") +
  ggtitle(paste0("total ", INVASIONES_STR, "/", DEFUNCIONES_STR, ", ", AÑO_STR)) +
  scale_y_continuous(breaks=seq(0, 7000, 1000), limits=c(0, 7000)) +
  scale_x_continuous(
    breaks = as.numeric(df_colera.grouped[, FECHA_STR]),
    labels = format(df_colera.grouped[, FECHA_STR], "%d - %m"),
    expand = c(0,0)
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom")

# save the plot
ggsave(paste(PLOTS_DIR, "colera_total_invasiones&defunciones.png", sep = "/"),
       width = 14,
       height = 4.5,
       dpi = 300,
       limitsize = TRUE)
