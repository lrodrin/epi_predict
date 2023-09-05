library(ggplot2)
library(scales)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)


load("colera_data.RData")


# constants ---------------------------------------------------------------


COLERA_PLOTS_DIR <- "colera_plots"
dir.create(COLERA_PLOTS_DIR, showWarnings = FALSE)

TASA_MORTALIDAD_STR <- "Tasa_mortalidad"
MONTHS_LABELS <- c("Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre")
CAPITALES <- c(
  "vitoria-gasteiz", "albacete", "alicante", "almeria", "oviedo", "avila",
  "badajoz", "barcelona", "burgos", "caceres", "cadiz", "castellon",
  "ciudad real", "cordoba", "cuenca", "gerona", "granada", "guadalajara",
  "san sebastian", "huelva", "huesca", "palma", "jaen", "la coruna",
  "logrono", "las palmas", "leon", "lleida", "lugo", "madrid", 
  "malaga", "murcia", "pamplona", "orense", "palencia", "pontevedra",
  "salamanca", "santa cruz de tenerife", "zaragoza", "segovia", "sevilla", "soria", 
  "tarragona", "teruel", "toledo", "valencia", "valladolid", "bilbao", 
  "zamora", "sarria"
)


# functions ---------------------------------------------------------------


formatN <- function(N) {
  #' Format a numeric value with thousands and decimal separators.
  #'
  #' This function takes a numeric value and formats it with thousands separators
  #' and decimal marks for improved readability.
  #'
  #' @param N Numeric value to be formatted.
  #'
  #' @return A character string representing the formatted numeric value.
  
  return(format(N, big.mark = ".", decimal.mark = ","))
  
}


factorize <- function(df, var_col, breaks, labels) {
  #' Factorize a numeric column in a data frame.
  #'
  #' This function takes a data frame, a column name, and specified breaks and labels to
  #' factorize the numeric values in that column.
  #'
  #' @param df Data frame containing the column to be factorized.
  #' @param var_col Name of the column to be factorized.
  #' @param breaks Breaks for factorization.
  #' @param labels Labels for the factors.
  #'
  #' @return The data frame with the specified column factorized.
  
  df[[var_col]] <-
    cut(
      df[[var_col]],
      breaks = breaks,
      labels = labels
    )
  
  return(df)
  
}


# main --------------------------------------------------------------------


# raw data ----------------------------------------------------------------


df_colera.merged.month$`Codigo Ine` <- as.numeric(df_colera.merged.month$`Codigo Ine`)
df_colera.merged.day$`Codigo Ine` <- as.numeric(df_colera.merged.day$`Codigo Ine`)
rownames(df_colera.merged.month) <- 1:nrow(df_colera.merged.month)
rownames(df_colera.merged.day) <- 1:nrow(df_colera.merged.day)
head(df_colera.merged.month)
head(df_colera.merged.day)


# fallecidos por cólera en 1885 -------------------------------------------


df_cuadro1 <- df_colera.merged.month
paste0("Fallecidos por cólera en ", ANO_STR, ": ", formatN(sum(df_cuadro1$Total_defunciones)))


# fallecidos por cólera en Madrid capital, en 1885 ------------------------


df_cuadro2 <- subset(df_cuadro1, Municipio == "Madrid")
paste0("Fallecidos por cólera en Madrid capital, en ", ANO_STR, ": ", formatN(sum(df_cuadro2$Total_defunciones)))


# fallecidos por cólera en Gerindote (Toledo), en 1885 --------------------


df_cuadro3 <- subset(df_cuadro1, Municipio == "Gerindote" & Total_invasiones != 0)
paste0("Fallecidos por cólera en Gerindote (Toledo), en ", ANO_STR, ": ", sum(df_cuadro3$Total_defunciones))


# plot (1)

ggplot(data = df_cuadro3, aes(x = Fecha, y = Total_defunciones)) +
  geom_bar(stat = "identity") +
  labs(x = paste0("Año ", ANO_STR), y = "Fallecidos") +
  ggtitle(paste0("Fallecidos por cólera en Gerindote (Toledo), en ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 65, 5), limits = c(0, 65), labels = number) +
  scale_x_continuous(breaks = 6:8, labels = MONTHS_LABELS[1:3]) +
  geom_text(
    data = data.frame(
      Fecha = c(6, 7, 8),
      defunciones_suma = c(
        sum(df_cuadro3$Total_defunciones[df_cuadro3$Fecha == 6]),
        sum(df_cuadro3$Total_defunciones[df_cuadro3$Fecha == 7]),
        sum(df_cuadro3$Total_defunciones[df_cuadro3$Fecha == 8])
      )
    ),
    aes(x = Fecha, y = defunciones_suma, label = defunciones_suma),
    vjust = -0.5
  ) +
  theme_bw()

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro3.png", sep = "/"), dpi = 300, limitsize = TRUE)


# plot (2)

df_cuadro3_2 <-
  data.frame(
    category = c(
      "'Superavit' de fallecimientos",
      "Fallecidos",
      paste0("Media de fallecidos en el quinquenio anterior ", ANO_STR)
    ),
    value = c(
      sum(df_cuadro3$Total_defunciones) - 47,
      sum(df_cuadro3$Total_defunciones),
      47
    )
  )

ggplot(data = df_cuadro3_2, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity") +
  labs(x = paste0("Año ", ANO_STR), y = "Fallecidos", fill = "") +
  ggtitle(paste0("Fallecidos por cólera en Gerindote (Toledo), en ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 200, 25), limits = c(0, 200), labels = number) +
  geom_text(aes(label = value), vjust = 1, position = position_stack(vjust = 0.5)) +
  theme_bw()

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro3_2.png", sep = "/"), dpi = 300, limitsize = TRUE)


# poblaciones de algunas ciudades -----------------------------------------


df_cuadro4 <- df_colera.merged.month %>%
  group_by(`Codigo Ine`) %>%
  summarise(
    Municipio = first(Municipio),
    Total_defunciones = sum(Total_defunciones),
    Total_poblacion = first(Total_poblacion)
  )  %>%
  arrange(desc(Total_poblacion))

colnames(df_cuadro4)[3:4] <- c("Fallecidos por cólera en 1885", "Censo de 1887")
head(df_cuadro4, 11)


# plot (1)

df_cuadro4.long <- df_cuadro4 %>%
  dplyr::select(Municipio, `Fallecidos por cólera en 1885`, `Censo de 1887`) %>%
  pivot_longer(cols = c(`Fallecidos por cólera en 1885`, `Censo de 1887`), names_to = "column", values_to = "value")

ggplot(data = df_cuadro4.long[1:12, ], aes(x = Municipio, y = value, fill = column)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = MUNICIPIO_STR, y = "Fallecidos", fill = NULL) +
  ggtitle("Poblaciones de algunas ciudades") +
  scale_y_continuous(breaks = seq(0, 470300, 10000), limits = c(0, 470300), labels = number) +
  scale_fill_manual(values = c("Fallecidos por cólera en 1885" = "red", "Censo de 1887" = "blue")) +
  geom_text(aes(label = value), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro4.png", sep = "/"), height = 11, dpi = 300, limitsize = TRUE)


# algunos datos demograficos de interes -----------------------------------
# del colera de 1885 ------------------------------------------------------


df_cuadro5 <- df_colera.merged.month %>%
  group_by(`Codigo Ine`) %>%
  summarise(
    Municipio = first(Municipio),
    Total_poblacion = first(Total_poblacion),
    Total_invasiones = sum(Total_invasiones),
    Total_defunciones = sum(Total_defunciones)
  ) 

paste0("Población de España (Censo de 1887): ", formatN(sum(Pob1887$Total_poblacion)))
paste0("Población sometida a la epidemia: ", formatN(sum(df_cuadro5$Total_poblacion)), " (", round((sum(df_cuadro5$Total_poblacion) / sum(Pob1887$Total_poblacion)) * 100, 2), " %)")

paste0("Municipios: ", formatN(sum(unique(Pob1887$`Codigo Ine`))))
paste0("Municipios afectados: ", formatN(sum(unique(df_cuadro5$`Codigo Ine`))), " (", round((sum(unique(df_cuadro5$`Codigo Ine`)) / sum(unique(Pob1887$`Codigo Ine`))) * 100, 2), " %)")

paste0("Invasiones: ", formatN(sum(df_cuadro5$Total_invasiones)))
paste0("Defunciones: ", formatN(sum(df_cuadro5$Total_defunciones)))

cat(
  "Porcentaje de",
  INVASIONES_STR,
  "en relación con:\n- la población total:",
  round((
    sum(df_cuadro5$Total_invasiones) / sum(Pob1887$Total_poblacion)
  ) * 100, 2),
  "%\n- la población de los Municipios afectados:",
  round((
    sum(df_cuadro5$Total_invasiones) / sum(df_cuadro5$Total_poblacion)
  ) * 100, 2),
  "%"
)

cat(
  "Porcentaje de",
  DEFUNCIONES_STR,
  "en relación con:\n- la población total:",
  round((
    sum(df_cuadro5$Total_defunciones) / sum(Pob1887$Total_poblacion)
  ) * 100, 2),
  "%\n- la población de los Municipios afectados:",
  round((
    sum(df_cuadro5$Total_defunciones) / sum(df_cuadro5$Total_poblacion)
  ) * 100, 2),
  "%\n- las invasiones:",
  round((
    sum(df_cuadro5$Total_defunciones) / sum(df_cuadro5$Total_invasiones)
  ) * 100, 2),
  "%"
)

cat(
  paste0(FECHA_STR, " de comienzo: ", format(min(df_colera.merged.day$Fecha, na.rm = TRUE), format = "%d de %B de %Y")), "\n",
  paste0(FECHA_STR, " de finalización: ", format(max(df_colera.merged.day$Fecha, na.rm = TRUE), format = "%d de %B de %Y")), "\n",
  paste0("Duración de la epidemia: ", max(df_colera.merged.day$Fecha, na.rm = TRUE) - min(df_colera.merged.day$Fecha, na.rm = TRUE), " días"), "\n",
  paste0("Intensidad media diaria de mortalidad: ???") # TODO: calculation
) # TODO: format in English


# plot (1)

df_cuadro5_2 <- df_colera.groupByProvinciaFecha %>%
  group_by(Provincia) %>%
  summarise(
    Provincia = first(Provincia),
    Total_invasiones = sum(Total_invasiones),
    Total_defunciones = sum(Total_defunciones)
  ) 

ggplot(df_cuadro5_2, aes(x = Total_defunciones, y = Provincia, fill = Provincia)) +
  geom_bar(stat = "identity") + 
  labs(x = DEFUNCIONES_STR, y = PROVINCIA_STR) +
  scale_x_continuous(breaks = seq(0, 13000, 300), limits = c(0, 13000), labels = number) +
  geom_text(aes(label = Total_defunciones), hjust = -0.2, size = 3) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro5_2.png", sep = "/"), dpi = 300, limitsize = TRUE)


# plot (2)

df_cuadro5_3 <- df_colera.groupByProvinciaFecha
df_cuadro5_3$Fecha <- month(as.POSIXlt(df_cuadro5_3$Fecha, format = DATE_FORMAT))
df_cuadro5_3 <- df_cuadro5_3 %>%
  group_by(Fecha) %>%
  summarise(
    Fecha = first(Fecha),
    Total_invasiones = sum(Total_invasiones),
    Total_defunciones = sum(Total_defunciones)
  )

ggplot(data = df_cuadro5_3, aes(x = Fecha, y = Total_defunciones)) +
  geom_bar(stat = "identity") +
  labs(x = paste0("Año ", ANO_STR), y = "Fallecidos") +
  ggtitle(paste0("Fallecidos por cólera en España, en ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 38100, 1000), limits = c(0, 38100), labels = number) +
  scale_x_continuous(breaks = 6:11, labels = MONTHS_LABELS) +
  geom_text(
    data = data.frame(
      Fecha = c(6, 7, 8, 9, 10, 11),
      defunciones_suma = c(
        sum(df_cuadro5_3$Total_defunciones[df_cuadro5_3$Fecha == 6]),
        sum(df_cuadro5_3$Total_defunciones[df_cuadro5_3$Fecha == 7]),
        sum(df_cuadro5_3$Total_defunciones[df_cuadro5_3$Fecha == 8]),
        sum(df_cuadro5_3$Total_defunciones[df_cuadro5_3$Fecha == 9]),
        sum(df_cuadro5_3$Total_defunciones[df_cuadro5_3$Fecha == 10]),
        sum(df_cuadro5_3$Total_defunciones[df_cuadro5_3$Fecha == 11])
      )
    ),
    aes(x = Fecha, y = defunciones_suma, label = defunciones_suma),
    vjust = -0.5
  ) +
  theme_bw()

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro5_3.png", sep = "/"), dpi = 300, limitsize = TRUE)


# plot (3)

df_cuadro5_4 <- df_colera.groupByProvinciaFecha 
df_cuadro5_4$Fecha <- month(as.POSIXlt(df_cuadro5_4$Fecha, format = DATE_FORMAT))
df_cuadro5_4$Fecha <- factor(df_cuadro5_4$Fecha, levels = c(6, 7, 8, 9, 10, 11), labels = MONTHS_LABELS)
df_cuadro5_4 <- subset(df_cuadro5_4, Total_defunciones != 0) %>%
  group_by(Provincia, Fecha) %>%
  summarise(
    Provincia = first(Provincia),
    Fecha = first(Fecha),
    Total_invasiones = sum(Total_invasiones),
    Total_defunciones = sum(Total_defunciones)
  )

ggplot(df_cuadro5_4, aes(x = Provincia, y = Total_defunciones, fill = Provincia)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Fecha, scales = "free_x") +
  labs(x = PROVINCIA_STR, y = "Fallecidos") +
  ggtitle(paste0("Fallecidos por cólera en España, en ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 8100, 250), limits = c(0, 8100), labels = number) +
  geom_text(aes(label = Total_defunciones), hjust = -0.2, size = 3, angle = 90) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro5_4.png", sep = "/"), width = 17, height = 8, dpi = 300, limitsize = TRUE)


# defunciones por cólera en 1885 (por meses) ------------------------------


df_cuadro6 <- df_cuadro5_3
df_cuadro6$Fecha <- as.character(df_cuadro6$Fecha)
df_cuadro6 <- df_cuadro6 %>%
  dplyr::select(Fecha, Total_defunciones) %>%
  bind_rows(data.frame(Fecha = "Total", Total_defunciones = sum(df_cuadro6$Total_defunciones))) %>%
  mutate(Fecha = case_when(
    Fecha == 6 ~ MONTHS_LABELS[1],
    Fecha == 7 ~ MONTHS_LABELS[2],
    Fecha == 8 ~ MONTHS_LABELS[3],
    Fecha == 9 ~ MONTHS_LABELS[4],
    Fecha == 10 ~ MONTHS_LABELS[5],
    Fecha == 11 ~ MONTHS_LABELS[6],
    TRUE ~ as.character(Fecha)
  )) 


# plot (1)

ggplot(data = df_cuadro6[1:6,], aes(x = Fecha, y = Total_defunciones, fill = Fecha)) +
  geom_bar(stat = "identity") +
  labs(x = "Meses", y = "Fallecidos") +
  ggtitle(paste0("Fallecidos por cólera en ", ANO_STR, " (por meses)")) +
  scale_y_continuous(breaks = seq(0, 38010, 5000), limits = c(0, 38010), labels = number) +
  scale_fill_discrete(guide = FALSE) +
  geom_text(aes(label = Total_defunciones), hjust = 0.5, vjust = -0.5, size = 3) + 
  theme_bw() # TODO: add total ???

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro6.png", sep = "/"), dpi = 300, limitsize = TRUE)

df_cuadro6.total <- df_cuadro6 %>%
  mutate(Fecha = str_pad(Fecha, width = 11, side = "right", pad = " ")) %>%
  unite(Fecha_Total, Fecha, Total_defunciones, sep = "    ") %>%
  rename("Fallecidos por cólera en 1885 (por meses)" = Fecha_Total)

df_cuadro6.total


# resumen general por municipios de las invasiones y defunciones ----------
# ocurridas por colera en España durante el año de 1885 -------------------


df_cuadro7 <- df_colera.merged.day
df_cuadro7 <- subset(df_cuadro7, Total_invasiones != 0)
df_cuadro7 <- df_cuadro7 %>%
  group_by(`Codigo Ine`) %>%
  arrange(Fecha) %>%
  mutate(Primer_caso = min(Fecha), Ultimo_caso = max(Fecha)) %>%
  ungroup() %>%
  dplyr::select(`Codigo Ine`, Provincia, Municipio, Total_poblacion, Primer_caso, Ultimo_caso, Total_invasiones, Tasa_incidencia, Total_defunciones, Tasa_mortalidad) %>%
  group_by(`Codigo Ine`, Provincia, Municipio, Total_poblacion, Primer_caso, Ultimo_caso) %>%
  summarize(
    Total_invasiones = sum(Total_invasiones),
    Total_defunciones = sum(Total_defunciones),
    Tasa_incidencia = sum(Tasa_incidencia),
    Tasa_mortalidad = sum(Tasa_mortalidad)
  )

df_cuadro7$Total_dias <- ifelse(df_cuadro7$Ultimo_caso - df_cuadro7$Primer_caso == 0, 1, df_cuadro7$Ultimo_caso - df_cuadro7$Primer_caso)
df_cuadro7$Tasa_invasiones <- round((df_cuadro7$Total_defunciones / df_cuadro7$Total_invasiones) * 100, 4)
df_cuadro7 <- df_cuadro7[, c(1, 2, 3, 4, 5, 6, 11, 7, 8, 9, 10, 12)]
head(df_cuadro7)

# TODO: create plots


# municipios con mayor % (>2) de defunciones ------------------------------
# respecto a la población sometida a epidemia -----------------------------


df_cuadro11 <- df_cuadro7[, c(CODIGO_INE_STR, PROVINCIA_STR, MUNICIPIO_STR, TOTAL_POBLACION_STR, TOTAL_INVASIONES_STR, TOTAL_DEFUNCIONES_STR, TASA_MORTALIDAD_STR)]
df_cuadro11 <- subset(df_cuadro11, Tasa_mortalidad > 2)
df_cuadro11 <- df_cuadro11 %>%
  group_by(Municipio) %>%
  summarise(Tasa_mortalidad = sum(Tasa_mortalidad))
df_cuadro11 <- df_cuadro11[order(df_cuadro11$Tasa_mortalidad, decreasing = TRUE),]
head(df_cuadro11, 13)


# plot (1)

ggplot(df_cuadro11[1:13,], aes(x = Tasa_mortalidad, y = Municipio, fill = Municipio)) +
  geom_bar(stat = "identity") + 
  labs(x = "Fallecidos (%)", y = MUNICIPIO_STR) +
  ggtitle("Municipios con mayor % (>2) de defunciones respecto a la población sometida a epidemia") +
  scale_x_continuous(breaks = seq(0, 32, 2), limits = c(0, 32), labels = number) +
  geom_text(aes(label = Tasa_mortalidad), hjust = -0.2, size = 3) + 
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 12))

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro11.png", sep = "/"), dpi = 300, limitsize = TRUE)

# TODO: cuadro 12


# permanencia de la epidemia ----------------------------------------------


df_cuadro13 <- df_cuadro7[, c("Total_dias", MUNICIPIO_STR)]
df_cuadro13 <- factorize(
  df_cuadro13,
  "Total_dias",
  c(-1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
  c("De menos de 5 días", "De 6 a 10 días", "De 11 a 20 días", "De 21 a 30 días", 
    "De 31 a 40 días", "De 41 a 50 días", "De 51 a 60 días", "De 61 a 70 días", 
    "De 71 a 80 días", "De 81 a 90 días", "De 91 a 100 días", "De más de 100 días")
)

df_cuadro13 <- df_cuadro13 %>%
  group_by(Total_dias) %>%
  summarize(Numero_de_Municipios = n()) 

df_cuadro13 <- bind_rows(df_cuadro13, data.frame(Total_dias = "Total", Numero_de_Municipios = sum(df_cuadro13$Numero_de_Municipios))) 
df_cuadro13$`Fallecidos (%)` <- round((df_cuadro13$Numero_de_Municipios / df_cuadro13$Numero_de_Municipios[13]) * 100, 2)
df_cuadro13


# plot (1)

df_cuadro13.long <- df_cuadro13 %>%
  dplyr::select(Total_dias, Numero_de_Municipios, `Fallecidos (%)`) %>%
  pivot_longer(cols = c(Numero_de_Municipios, `Fallecidos (%)`), names_to = "column", values_to = "value")

ggplot(data = df_cuadro13.long[1:12, ], aes(x = Total_dias, y = value, fill = column)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Permanencia de la epidemia") +
  scale_y_continuous(breaks = seq(0, 360, 10), limits = c(0, 360), labels = number) +
  scale_fill_manual(values = c("Numero_de_Municipios" = "red", "Fallecidos (%)" = "blue")) +
  geom_text(aes(label = value), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro13.png", sep = "/"), height = 11, dpi = 300, limitsize = TRUE)


# porcentaje de población que fallece -------------------------------------
# por cólera en distintos municipios --------------------------------------


df_cuadro14 <- df_cuadro7[, c(TASA_MORTALIDAD_STR, MUNICIPIO_STR)]
df_cuadro14 <- factorize(
  df_cuadro14,
  TASA_MORTALIDAD_STR,
  c(-1, 0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, Inf),
  c("Hasta el 0.5 %", "Del 0.51 al 1 %", "Del 1.01 al 2 %", "Del 2.01 al 3 %", 
    "Del 3.01 al 4 %", "Del 4.01 al 5 %", "Del 5.01 al 6 %", "Del 6.01 al 7 %", 
    "Del 7.01 al 8 %", "Del 8.01 al 9 %", "Del 9.01 al 10 %", "De más del 10 %")
)

df_cuadro14 <- df_cuadro14 %>%
  group_by(Tasa_mortalidad) %>%
  summarize(Numero_de_Municipios = n()) 

df_cuadro14 <- bind_rows(df_cuadro14, data.frame(Tasa_mortalidad = "Total", Numero_de_Municipios = sum(df_cuadro14$Numero_de_Municipios))) 
df_cuadro14$`Fallecidos (%)` <- round((df_cuadro14$Numero_de_Municipios / df_cuadro14$Numero_de_Municipios[13]) * 100, 2)
df_cuadro14


# plot (1)

df_cuadro14.long <- df_cuadro14 %>%
  dplyr::select(Tasa_mortalidad, Numero_de_Municipios, `Fallecidos (%)`) %>%
  pivot_longer(cols = c(Numero_de_Municipios, `Fallecidos (%)`), names_to = "column", values_to = "value")

ggplot(data = df_cuadro14.long[1:12, ], aes(x = Tasa_mortalidad, y = value, fill = column)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Porcentaje de población que fallece por cólera en distintos municipios") +
  scale_y_continuous(breaks = seq(0, 565, 10), limits = c(0, 565), labels = number) +
  scale_fill_manual(values = c("Numero_de_Municipios" = "red", "Fallecidos (%)" = "blue")) +
  geom_text(aes(label = value), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(size = 12))

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro14.png", sep = "/"), height = 11, dpi = 300, limitsize = TRUE)


# municipios con mayor porcentaje de fallecidos ---------------------------
# con respecto a la población (>10 %) -------------------------------------


df_cuadro15 <- df_cuadro7[, c(CODIGO_INE_STR, PROVINCIA_STR, MUNICIPIO_STR, TOTAL_POBLACION_STR, TOTAL_INVASIONES_STR, TOTAL_DEFUNCIONES_STR, TASA_MORTALIDAD_STR)]
df_cuadro15 <- subset(df_cuadro15, Tasa_mortalidad >= 10)
df_cuadro15 <- df_cuadro15[order(df_cuadro15$Tasa_mortalidad, decreasing = TRUE),]
df_cuadro15


# plot (1)

ggplot(df_cuadro15, aes(x = Tasa_mortalidad, y = Municipio, fill = Municipio)) +
  geom_bar(stat = "identity") + 
  labs(x = "Fallecidos (%)", y = MUNICIPIO_STR) +
  ggtitle("Municipios con mayor porcentaje de fallecidos con respecto a la población (>10 %)") +
  scale_x_continuous(breaks = seq(0, 32, 2), limits = c(0, 32), labels = number) +
  geom_text(aes(label = Tasa_mortalidad), hjust = -0.2, size = 3) + 
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 12))

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro15.png", sep = "/"), dpi = 300, limitsize = TRUE)


# fallecidos en 1885 en Monteagudo (Soria) --------------------------------
# por meses y días --------------------------------------------------------

df_cuadro17 <- subset(df_colera.groupByProvinciaFechaMunicipo, Municipio == "monteagudo")
df_cuadro18 <- df_cuadro17
df_cuadro17$Fecha <- month(as.POSIXlt(df_cuadro17$Fecha, format = DATE_FORMAT))
df_cuadro17 <- df_cuadro17 %>%
  group_by(Fecha) %>%
  summarise(
    Fecha = first(Fecha),
    Total_defunciones = sum(Total_defunciones)
  )

df_cuadro17$Fecha <- as.character(df_cuadro17$Fecha)
df_cuadro17 <- df_cuadro17 %>%
  dplyr::select(Fecha, Total_defunciones) %>%
  bind_rows(data.frame(Fecha = "Total", Total_defunciones = sum(df_cuadro17$Total_defunciones))) %>%
  mutate(Fecha = case_when(
    Fecha == 6 ~ MONTHS_LABELS[1],
    Fecha == 7 ~ MONTHS_LABELS[2],
    Fecha == 8 ~ MONTHS_LABELS[3],
    Fecha == 9 ~ MONTHS_LABELS[4],
    Fecha == 10 ~ MONTHS_LABELS[5],
    Fecha == 11 ~ MONTHS_LABELS[6],
    TRUE ~ as.character(Fecha)
  ))

df_cuadro17


# plot (1)

ggplot(data = df_cuadro17[1:6,], aes(x = Fecha, y = Total_defunciones, fill = Fecha)) +
  geom_bar(stat = "identity") +
  labs(x = "Meses", y = "Fallecidos") +
  ggtitle(paste0("Fallecidos por cólera en Monteagudo (Soria), en ", ANO_STR, " (por meses)")) +
  # scale_y_continuous(breaks = seq(0, 38010, 5000), limits = c(0, 38010), labels = number) +
  scale_fill_discrete(guide = FALSE) +
  geom_text(aes(label = Total_defunciones), hjust = 0.5, vjust = -0.5, size = 3) + 
  theme_bw() # TODO: add total ???

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro17.png", sep = "/"), dpi = 300, limitsize = TRUE)

df_cuadro17.total <- df_cuadro17 %>%
  mutate(Fecha = str_pad(Fecha, width = 11, side = "right", pad = " ")) %>%
  unite(Fecha_Total, Fecha, Total_defunciones, sep = "    ") %>%
  rename("Fallecidos por cólera en Monteagudo (Soria), en 1885 (por meses)" = Fecha_Total)

df_cuadro17.total


# plot (2)

ggplot(data = df_cuadro18, aes(x = Fecha, y = Total_defunciones)) +
  geom_bar(stat = "identity") +
  labs(x = paste0("Año ", ANO_STR), y = "Fallecidos") +
  ggtitle(paste0("Fallecidos por cólera en Monteagudo (Soria), en ", ANO_STR, " (por días)")) +
  scale_y_continuous(breaks = seq(0, 70, 5), limits = c(0, 70), labels = number) +
  scale_x_continuous(
    breaks = seq(min(df_cuadro18$Fecha), max(df_cuadro18$Fecha), by = "2 days"),
    labels = seq(min(df_cuadro18$Fecha), max(df_cuadro18$Fecha), by = "2 days")
  ) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro18.png", sep = "/"), width = 9, dpi = 300, limitsize = TRUE)


# capitales de provincia con % fallecidos por colera ----------------------
# con respecto a su poblacion ---------------------------------------------


df_cuadro77 <- df_cuadro7[, c(CODIGO_INE_STR, PROVINCIA_STR, MUNICIPIO_STR, TOTAL_POBLACION_STR, TOTAL_INVASIONES_STR, TOTAL_DEFUNCIONES_STR, TASA_MORTALIDAD_STR)]
df_cuadro77$Municipio <- tolower(iconv(df_cuadro77$Municipio, from = "UTF-8", to = "ASCII//TRANSLIT"))
df_cuadro77$isCapital <- ifelse(df_cuadro77$Municipio %in% CAPITALES, TRUE, FALSE)
df_cuadro77.capitales <- subset(df_cuadro77, isCapital == TRUE)
df_cuadro77.capitales$isCapital <- NULL
df_cuadro77.capitales <- df_cuadro77.capitales[order(df_cuadro77.capitales$Tasa_mortalidad, decreasing = TRUE),]
head(df_cuadro77, 10)


# plot (1)

ggplot(df_cuadro77.capitales[1:10,], aes(x = Tasa_mortalidad, y = Municipio, fill = Municipio)) +
  geom_bar(stat = "identity") + 
  labs(x = "Fallecidos (%)", y = MUNICIPIO_STR) +
  ggtitle("Capitales de provincia con % fallecidos por colera con respecto a su poblacion") +
  scale_x_continuous(breaks = seq(0, 5, 1), limits = c(0, 5), labels = number) +
  geom_text(aes(label = Tasa_mortalidad), hjust = -0.2, size = 3) + 
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 12))

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro77.capitales.png", sep = "/"), dpi = 300, limitsize = TRUE)


# map ---------------------------------------------------------------------


# TODO
