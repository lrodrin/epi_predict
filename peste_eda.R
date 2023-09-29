library(sf)
library(tmap)
library(dplyr)
library(ggplot2)
library(scales)


# load("peste_data.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
PESTE_PLOTS_DIR <- "peste_plots"
dir.create(PESTE_PLOTS_DIR, showWarnings = FALSE)
PESTE_MAPS_DIR <- "peste_maps"
dir.create(PESTE_MAPS_DIR, showWarnings = FALSE)

FECHA_STR <- "Fecha"
MUNICIPIO_STR <- "Municipio"
NAMEUNIT_STR <- "NAMEUNIT"
LOCALIDADES_STR <- c("Artà", "Capdepera", "Sant Llorenç des Cardassar", "Son Servera")
CATEGORIAS_STR <- c(INFECTADOS_STR, MUERTOS_STR, CURADOS_STR, SUPERVIVIENTES_STR)
MONTHS_INT <- c(6, 7, 8, 9, 10)
MONTHS_STR <- c("Junio", "Julio", "Agosto", "Septiembre", "Octubre")


# functions ---------------------------------------------------------------


read_shapefile <- function(filename) {
  #' Read a shape file from the specified directory.
  #'
  #' This function reads a shape file from the specified directory and returns it as a
  #' spatial object.
  #'
  #' @param filename Name of the shape file file.
  #'
  #' @return A spatial object representing the shape file.
  
  shapefile <- st_read(paste(SHAPES_DATA_DIR, filename, sep = "/"), quiet = TRUE)
  print(head(shapefile))
  
  return(shapefile)
  
}


add_tasas <- function(df_peste) {
  #' Add mortality and morbidity rates to the plague dataset
  #'
  #' This function calculates and adds mortality and morbidity rates to the plague dataset. 
  #' It computes the mortality rate (Tasa_mortalidad) and the morbidity rate (Tasa_morbilidad) 
  #' for each record based on the number of cases and population. Additionally, it calculates 
  #' the percentage of cured individuals (% Curados) and the percentage of survivors 
  #' (% Supervivientes) for each date and municipality.
  #'
  #' @param df_peste DataFrame containing the plague data.
  #'
  #' @return DataFrame updated with mortality and morbidity rates, as well as percentages of 
  #'         cured individuals and survivors.
  
  df_peste$Tasa_mortalidad <- round(ifelse(df_peste$Categoria == MUERTOS_STR, (df_peste$Casos / df_peste$Poblacion) * 1000, 0), 2)
  
  df_peste$Tasa_morbilidad <- round(ifelse(df_peste$Categoria == INFECTADOS_STR, (df_peste$Casos / df_peste$Poblacion) * 1000, 0), 2)
  
  df_peste <- df_peste %>% group_by(Fecha, Municipio) %>%
    mutate(`% Curados` = round(ifelse(Categoria == CURADOS_STR, Casos / sum(Casos[Categoria == INFECTADOS_STR]) * 100, 0), 2))
  
  df_peste <- df_peste %>% group_by(Fecha, Municipio) %>%
    mutate(`% Supervivientes` = round(ifelse(Categoria == SUPERVIVIENTES_STR, Casos / sum(Casos[Categoria == INFECTADOS_STR]) * 100, 0), 2))
  
  return(df_peste)
  
}


# main --------------------------------------------------------------------


# raw data ----------------------------------------------------------------


df_peste.eda.day <- df_peste.merged.day
df_peste.eda.month <- df_peste.merged.month


# maps --------------------------------------------------------------------


mapS.municipios <- read_shapefile("Municipios_IGN.shp")
mapS.mallorca <- subset(mapS.municipios, CODNUT1 == "ES5" & !(CODNUT2 %in% c("ES51", "ES52")) & CODNUT3 == "ES532") # select Balearic Islands

head(mapS.municipios)
head(mapS.mallorca)


# Isla de Mallorca. Ubicación de la epidemia de 1820 ----------------------


map.mallorca <- tm_shape(mapS.mallorca) + tm_borders() + tm_shape(mapS.mallorca[mapS.mallorca$NAMEUNIT == "Palma", ]) + tm_text(NAMEUNIT_STR, size = 0.7) +
  tm_shape(mapS.mallorca[mapS.mallorca$NAMEUNIT %in% LOCALIDADES_STR, ]) +
  tm_fill(col = "gray", alpha = 0.5) + tm_text(NAMEUNIT_STR, size = 0.7) + 
  tm_layout(frame = FALSE, title = "Isla de Mallorca. Ubicación de la epidemia de 1820") +
  tm_compass(type = "arrow", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 1, position = c("left", "bottom"))

tmap_save(map.mallorca, filename = paste(PESTE_MAPS_DIR, "map.mallorca.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# Cronología de la epidemia -----------------------------------------------


cat(
  paste0(FECHA_STR, " de comienzo: ", format(min(df_peste.merged.day$Fecha, na.rm = TRUE), format = "%d de %B de %Y")), "\n",
  paste0(FECHA_STR, " de finalización: ", format(max(df_peste.merged.day$Fecha, na.rm = TRUE), format = "%d de %B de %Y")), "\n",
  paste0("Duración de la epidemia: ", max(df_peste.merged.day$Fecha, na.rm = TRUE) - min(df_peste.merged.day$Fecha, na.rm = TRUE), " días")
)


# plots

ggplot(subset(df_peste.eda.day, Municipio == LOCALIDADES_STR[1]), aes(x = Fecha, y = Casos, color = Categoria)) +
  geom_line() +   
  geom_vline(xintercept = as.numeric(as.Date("1820-07-15")), color = "black", linetype = "dashed") +
  geom_text(x = as.numeric(as.Date("1820-07-15")) + 1, y = max(df_peste.eda.day$Casos), label = "Se levanta el cordón sanitario", hjust = 0, vjust = 0, color = "black", size = 3.5) +
  labs(x = NULL, y = "Casos", title = paste0("Cronología de la epidemia en ", LOCALIDADES_STR[1], ", ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 310, 10), limits = c(0, 310), labels = number) +
  scale_x_continuous(
    breaks = df_peste.eda.day$Fecha,
    labels = df_peste.eda.day$Fecha
  ) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "bottom", legend.title = element_blank())

ggplot(subset(df_peste.eda.day, Municipio == LOCALIDADES_STR[2]), aes(x = Fecha, y = Casos, color = Categoria)) +
  geom_line() +   
  geom_vline(xintercept = as.numeric(as.Date("1820-07-15")), color = "black", linetype = "dashed") +
  geom_text(x = as.numeric(as.Date("1820-07-15")) + 1, y = max(df_peste.eda.day$Casos), label = "Se levanta el cordón sanitario", hjust = 0, vjust = 0, color = "black", size = 3.5) +
  labs(x = NULL, y = "Casos", title = paste0("Cronología de la epidemia en ", LOCALIDADES_STR[2], ", ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 100), labels = number) +
  scale_x_continuous(
    breaks = df_peste.eda.day$Fecha,
    labels = df_peste.eda.day$Fecha
  ) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "bottom", legend.title = element_blank())

ggplot(subset(df_peste.eda.day, Municipio == LOCALIDADES_STR[3]), aes(x = Fecha, y = Casos, color = Categoria)) +
  geom_line() +   
  geom_vline(xintercept = as.numeric(as.Date("1820-07-15")), color = "black", linetype = "dashed") +
  geom_text(x = as.numeric(as.Date("1820-07-15")) + 1, y = max(df_peste.eda.day$Casos), label = "Se levanta el cordón sanitario", hjust = 0, vjust = 0, color = "black", size = 3.5) +
  labs(x = NULL, y = "Casos", title = paste0("Cronología de la epidemia en ", LOCALIDADES_STR[3], ", ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 110, 5), limits = c(0, 110), labels = number) +
  scale_x_continuous(
    breaks = df_peste.eda.day$Fecha,
    labels = df_peste.eda.day$Fecha
  ) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "bottom", legend.title = element_blank())

ggplot(subset(df_peste.eda.day, Municipio == LOCALIDADES_STR[4]), aes(x = Fecha, y = Casos, color = Categoria)) +
  geom_line() +   
  geom_vline(xintercept = as.numeric(as.Date("1820-07-15")), color = "black", linetype = "dashed") +
  geom_text(x = as.numeric(as.Date("1820-07-15")) + 1, y = max(df_peste.eda.day$Casos), label = "Se levanta el cordón sanitario", hjust = 0, vjust = 0, color = "black", size = 3.5) +
  labs(x = NULL, y = "Casos", title = paste0("Cronología de la epidemia en ", LOCALIDADES_STR[4], ", ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 125, 5), limits = c(0, 125), labels = number) +
  scale_x_continuous(
    breaks = df_peste.eda.day$Fecha,
    labels = df_peste.eda.day$Fecha
  ) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "bottom", legend.title = element_blank())

# TODO: add missing dates ???


# Número de habitantes y personas infectadas, -----------------------------
# fallecidas y curadas por municipio, 1820 --------------------------------


# plots

ggplot(df_peste.eda.month %>% filter(Categoria == MUERTOS_STR), aes(x = Municipio, y = Casos, fill = Municipio)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Fecha, scales = "free_x") +
  labs(x = MUNICIPIO_STR, y = "Fallecidos") +
  ggtitle(paste0("Número de fallecidos por municipio, ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 230, 5), limits = c(0, 230), labels = number) +
  geom_text(aes(label = Casos), hjust = -0.2, size = 3, angle = 90) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none") 

ggsave(paste(PESTE_PLOTS_DIR, "total_defunciones.png", sep = "/"), width = 6, height = 7, dpi = 300, limitsize = TRUE)

ggplot(df_peste.eda.month %>% filter(Categoria == INFECTADOS_STR), aes(x = Municipio, y = Casos, fill = Municipio)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Fecha, scales = "free_x") +
  labs(x = MUNICIPIO_STR, y = "Contaminados") +
  ggtitle(paste0("Número de contaminados por municipio, ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 3115, 100), limits = c(0, 3115), labels = number) +
  geom_text(aes(label = Casos), hjust = -0.2, size = 3, angle = 90) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste(PESTE_PLOTS_DIR, "total_invasiones.png", sep = "/"), width = 6, height = 7, dpi = 300, limitsize = TRUE)

ggplot(df_peste.eda.month %>% filter(Categoria == CURADOS_STR), aes(x = Municipio, y = Casos, fill = Municipio)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Fecha, scales = "free_x") +
  labs(x = MUNICIPIO_STR, y = CURADOS_STR) +
  ggtitle(paste0("Número de curados por municipio, ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 145, 5), limits = c(0, 145), labels = number) +
  geom_text(aes(label = Casos), hjust = -0.2, size = 3, angle = 90) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste(PESTE_PLOTS_DIR, "total_curados.png", sep = "/"), width = 6, height = 7, dpi = 300, limitsize = TRUE)

ggplot(df_peste.eda.month %>% filter(Categoria == SUPERVIVIENTES_STR), aes(x = Municipio, y = Casos, fill = Municipio)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Fecha, scales = "free_x") +
  labs(x = MUNICIPIO_STR, y = SUPERVIVIENTES_STR) +
  ggtitle(paste0("Número de supervivientes por municipio, ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 2890, 100), limits = c(0, 2890), labels = number) +
  geom_text(aes(label = Casos), hjust = -0.2, size = 3, angle = 90) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste(PESTE_PLOTS_DIR, "total_supervivientes.png", sep = "/"), width = 6, height = 7, dpi = 300, limitsize = TRUE)


# table (2)

table_2 <- data.frame(matrix(ncol = 6, nrow = 5))
colnames(table_2) <- c("Municipio", "Número de habitantes antes de la epidemia", "Número de personas infectadas", "Número de muertes", "Número de personas curadas", "Número de supervivientes")
table_2$Municipio <- c(LOCALIDADES_STR, "TOTAL")

table_2[1,2] <- 3626
table_2[2,2] <- 1179
table_2[3,2] <- 1338
table_2[4,2] <- 1684
table_2[5,2] <- sum(table_2[1:4, 2])

for (j in 3:6) {
  for (i in 1:4) {
    
    table_2[i, j] <- df_peste.eda.month %>%
      filter(Categoria == CATEGORIAS_STR[j - 2] & Municipio == LOCALIDADES_STR[i]) %>%
      pull(Casos) %>%
      sum()
  }
  
  table_2[5, j] <- sum(table_2[1:4, j])
}


# Indicadores demográficos ------------------------------------------------


df_peste.eda.day <- add_tasas(df_peste.eda.day)
df_peste.eda.day <- df_peste.eda.day %>% mutate(`% Curados` = replace(`% Curados`, is.na(`% Curados`), 0))
df_peste.eda.day <- df_peste.eda.day %>% mutate(`% Supervivientes` = replace(`% Supervivientes`, is.na(`% Supervivientes`), 0))
df_peste.eda.month <- add_tasas(df_peste.eda.month)
df_peste.eda.month <- df_peste.eda.month %>% mutate(`% Curados` = replace(`% Curados`, is.na(`% Curados`), 0))
df_peste.eda.month <- df_peste.eda.month %>% mutate(`% Supervivientes` = replace(`% Supervivientes`, is.na(`% Supervivientes`), 0))


# plots

ggplot(df_peste.eda.month %>% filter(Categoria == MUERTOS_STR), aes(x = Municipio, y = Tasa_mortalidad, fill = Municipio)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Fecha, scales = "free_x") +
  labs(x = MUNICIPIO_STR, y = "% Mortalidad") +
  ggtitle(paste0("Tasa de mortalidad por municipio, ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 65, 5), limits = c(0, 65), labels = number) +
  geom_text(aes(label = Tasa_mortalidad), hjust = -0.2, size = 3, angle = 90) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none") 

ggsave(paste(PESTE_PLOTS_DIR, "tasa_mortalidad.png", sep = "/"), width = 6, height = 10, dpi = 300, limitsize = TRUE)

ggplot(df_peste.eda.month %>% filter(Categoria == INFECTADOS_STR), aes(x = Municipio, y = Tasa_morbilidad, fill = Municipio)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Fecha, scales = "free_x") +
  labs(x = MUNICIPIO_STR, y = "% Morbilidad") +
  ggtitle(paste0("Tasa de morbilidad por municipio, ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 860, 20), limits = c(0, 860), labels = number) +
  geom_text(aes(label = Tasa_morbilidad), hjust = -0.2, size = 3, angle = 90) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste(PESTE_PLOTS_DIR, "tasa_morbilidad.png", sep = "/"), width = 6, height = 10, dpi = 300, limitsize = TRUE)

ggplot(df_peste.eda.month %>% filter(Categoria == CURADOS_STR), aes(x = Municipio, y = `% Curados`, fill = Municipio)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Fecha, scales = "free_x") +
  labs(x = MUNICIPIO_STR, y = paste0("% ", CURADOS_STR)) +
  ggtitle(paste0("Tasa de curados por municipio, ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 155, 5), limits = c(0, 155), labels = number) +
  geom_text(aes(label = `% Curados`), hjust = -0.2, size = 3, angle = 90) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste(PESTE_PLOTS_DIR, "tasa_curados.png", sep = "/"), width = 6, height = 10, dpi = 300, limitsize = TRUE)

ggplot(df_peste.eda.month %>% filter(Categoria == SUPERVIVIENTES_STR), aes(x = Municipio, y = `% Supervivientes`, fill = Municipio)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Fecha, scales = "free_x") +
  labs(x = MUNICIPIO_STR, y = SUPERVIVIENTES_STR) +
  ggtitle(paste0("Tasa de supervivientes por municipio, ", ANO_STR)) +
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 100), labels = number) +
  geom_text(aes(label = `% Supervivientes`), hjust = -0.2, size = 3, angle = 90) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.1), legend.position = "none")

ggsave(paste(PESTE_PLOTS_DIR, "tasa_supervivientes.png", sep = "/"), width = 6, height = 10, dpi = 300, limitsize = TRUE)


# table (3)

table_3 <- data.frame(matrix(ncol = 5, nrow = 4))
colnames(table_3) <- c("Municipio", "% mortalidad", "% morbilidad", "% curados", "% supervivientes")
table_3$Municipio <- LOCALIDADES_STR

table_3[1,2] <- round((table_2[1,4] / table_2[[1,2]]) * 1000, 2)
table_3[2,2] <- round((table_2[2,4] / table_2[2,2]) * 1000, 2)
table_3[3,2] <- round((table_2[3,4] / table_2[3,2]) * 1000, 2)
table_3[4,2] <- round((table_2[4,4] / table_2[4,2]) * 1000, 2)

table_3[1,3] <- round((table_2[1,3] / table_2[1,2]) * 1000, 2)
table_3[2,3] <- round((table_2[2,3] / table_2[2,2]) * 1000, 2)
table_3[3,3] <- round((table_2[3,3] / table_2[3,2]) * 1000, 2)
table_3[4,3] <- round((table_2[4,3] / table_2[4,2]) * 1000, 2)

table_3[1,4] <- round((table_2[1,5] / table_2[1,3]) * 100, 2)
table_3[2,4] <- round((table_2[2,5] / table_2[2,3]) * 100, 2)
table_3[3,4] <- round((table_2[3,5] / table_2[3,3]) * 100, 2)
table_3[4,4] <- round((table_2[4,5] / table_2[4,3]) * 100, 2)

table_3[1,5] <- round((table_2[1,6] / table_2[1,3]) * 100, 2)
table_3[2,5] <- round((table_2[2,6] / table_2[2,3]) * 100, 2)
table_3[3,5] <- round((table_2[3,6] / table_2[2,3]) * 100, 2)
table_3[4,5] <- round((table_2[4,6] / table_2[4,3]) * 100, 2)


# clean environment -------------------------------------------------------


rm(mapS.municipios,mapS.mallorca, map.mallorca, table_2, table_3)
