library(ggplot2)
library(scales)
library(dplyr)
library(sf)
library(tmap)


# load("colera_data.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
COLERA_MAPS_DIR <- "colera_maps"
dir.create(COLERA_MAPS_DIR, showWarnings = FALSE)

TASA_MORTALIDAD_STR <- "Tasa_mortalidad"
DEFUNCIONES_FACTOR_STR <- "defunciones_factor"
CODIGOINE_STR <- "CODIGOINE"
NAMEUNIT_STR <- "NAMEUNIT"
LONG_STR <- "long"
LAT_STR <- "lat"
MONTHS_STR <- c("Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre")
CAPITALES_STR <- c(
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


factorize <- function(df, var_col, breaks, labels, isFactor_col, factor_col) {
  #' Factorize a numeric or factor column in a data frame.
  #'
  #' This function takes a data frame, a column name, specified breaks, and labels to
  #' factorize the numeric or factor values in that column.
  #'
  #' @param df Data frame containing the column to be factorized.
  #' @param var_col Name of the column to be factorized.
  #' @param breaks Breaks for factorization.
  #' @param labels Labels for the factors.
  #' @param isFactor_col Logical value indicating whether the column is already a factor.
  #' @param factor_col Name of the factor column (only used if isFactor_col is TRUE).
  #'
  #' @return The data frame with the specified column factorized.
  
  if (isFactor_col == FALSE) {
    
    df[[var_col]] <- cut(df[[var_col]], breaks = breaks, labels = labels)
    
  }  else {
    
    df[[factor_col]] <- cut(df[[var_col]], breaks = breaks, labels = labels)
  }
  
  return(df)
  
}


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


merge_coords <- function(df, df_coord) {
  #' Merge coordinates with cholera data and perform data cleaning.
  #'
  #' This function merges coordinates with cholera data, removes unnecessary columns, and performs
  #' data cleaning on latitude and longitude values.
  #'
  #' @param df_colera.merged Cholera data with rates and other variables.
  #' @param df_colera_coord Data frame containing latitude and longitude coordinates.
  #'
  #' @return Merged data frame with cleaned latitude and longitude values.
  
  df_colera_eda.merged <- merge(df, df_coord, by = CODIGO_INE_STR)
  df_colera_eda.merged <- df_colera_eda.merged[, !colnames(df_colera_eda.merged) == MUNICIPIO_STR]
  df_colera_eda.merged <- df_colera_eda.merged %>%
    mutate(
      lat = ifelse(lat < 36.0, 36.0, ifelse(lat > 43.0, 43.0, lat)),
      long = ifelse(long < -10.0, -10.0, ifelse(long > 4.0, 4.0, long))
    )
  
  return(df_colera_eda.merged)
  
}


df_quincena <- function(df, first, last) {
  #' Extract and summarize data for a specific bi-weekly period.
  #'
  #' This function extracts and summarizes data for a specific bi-weekly period
  #' defined by the "first" and "last" dates.
  #'
  #' @param df Data frame containing cholera data.
  #' @param first Start date of the bi-weekly period.
  #' @param last End date of the bi-weekly period.
  #'
  #' @return Data frame with summarized cholera data for the specified period.
  
  df_quincena <- subset(df, Fecha %in% c(first, last))
  df_quincena <- df_quincena %>%
    group_by(CODIGOINE, OBJECTID, INSPIREID, NATCODE, NAMEUNIT, CODNUT1, CODNUT2, CODNUT3, Provincia, Total_poblacion, lat, long, geometry) %>%
    summarize(
      Total_invasiones = sum(as.numeric(Total_invasiones)),
      Tasa_incidencia = sum(as.numeric(Tasa_incidencia)),
      Total_defunciones = sum(as.numeric(Total_defunciones)),
      Tasa_mortalidad = sum(as.numeric(Tasa_mortalidad))
    )
  
  return(df_quincena)
  
}

create_tmap <- function(df_mes, map, var_col, legend_title, style, coords) {
  #' Create a thematic map using tmap package.
  #'
  #' This function creates a thematic map using the tmap package based on the provided data frame,
  #' map object, variable column, legend title, style and coords.
  #'
  #' @param df_mes Data frame containing the data to be mapped.
  #' @param map Spatial object representing the map background.
  #' @param var_col Column containing the variable to be mapped.
  #' @param legend_title Title for the legend.
  #' @param style Style for mapping (e.g., "cat" for categorical).
  #' @param coords Spatial boundaries based on coordinates.
  #'
  #' @return A thematic map.
  
  return(
    tm_shape(df_mes) +
      tm_polygons(
        col = var_col,
        border.col = NULL,
        title = legend_title,
        palette = "Reds",
        style = style
      ) +
      tm_shape(map) + tm_borders() +
      tm_layout(
        legend.position =  c("right", "bottom"),
        legend.outside = TRUE,
        frame = FALSE,
        inner.margins = c(0, 0, 0, 0)
      ) +
      tm_view(bbox = coords)
  )
}


create_tmapByFecha <- function(df_mes, map, var_col, legend_title, style, coords, nrows) {
  #' Create a thematic map with facets by date using tmap package.
  #'
  #' This function creates a thematic map with facets by date using the tmap package.
  #' It is designed to display temporal data on multiple map panels.
  #'
  #' @param df_mes Data frame containing the data to be mapped.
  #' @param map Spatial object representing the map background.
  #' @param var_col Column containing the variable to be mapped.
  #' @param legend_title Title for the legend.
  #' @param style Style for mapping (e.g., "cat" for categorical).
  #' @param coords Spatial boundaries based on coordinates.
  #' @param nrows Number of rows for arranging facets.
  #'
  #' @return A thematic map with facets.
  
  return(
    tm_shape(df_mes) +
      tm_polygons(
        col = var_col,
        border.col = NULL,
        title = legend_title,
        palette = "Reds",
        style = style
      ) +
      tm_facets(
        by = FECHA_STR,
        nrow = nrows,
        free.coords = FALSE
      ) +
      tm_shape(map) + tm_borders() +
      tm_layout(
        legend.position =  c("right", "bottom"),
        legend.outside = TRUE,
        frame = FALSE,
        inner.margins = c(0, 0, 0, 0)
      ) +
      tm_view(bbox = coords)
  )
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
  scale_x_continuous(breaks = 6:8, labels = MONTHS_STR[1:3]) +
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


df_cuadro5 <- df_colera.merged.day %>%
  group_by(`Codigo Ine`, Municipio, Total_poblacion) %>%
  summarise(
    Total_invasiones = sum(Total_invasiones),
    Total_defunciones = sum(Total_defunciones)
  ) 

paste0("Población de España (Censo de 1887): ", formatN(sum(Pob1887$Total_poblacion)))
paste0("Población sometida a la epidemia: ", formatN(sum(df_cuadro5$Total_poblacion)), " (", round((sum(unique(df_cuadro5$Total_poblacion)) / sum(Pob1887$Total_poblacion)) * 100, 2), " %)")

paste0("Municipios: ", formatN(sum(unique(Pob1887$`Codigo Ine`))))
paste0("Municipios afectados: ", formatN(sum(unique(df_cuadro5$`Codigo Ine`))), " (", round((sum(unique(df_cuadro5$`Codigo Ine`)) / sum(unique(Pob1887$`Codigo Ine`))) * 100, 2), " %)")

paste0("Invasiones: ", formatN(sum(df_cuadro5$Total_invasiones)))
paste0("Defunciones: ", formatN(sum(df_cuadro5$Total_defunciones)))

cat(
  "Porcentaje de", INVASIONES_STR, "en relación con:\n- la población total:",
  round((sum(df_cuadro5$Total_invasiones) / sum(Pob1887$Total_poblacion)) * 100, 2),
  "%\n- la población de los Municipios afectados:",
  round((sum(df_cuadro5$Total_invasiones) / sum(df_cuadro5$Total_poblacion)) * 100, 2), "%"
)

cat(
  "Porcentaje de", DEFUNCIONES_STR, "en relación con:\n- la población total:",
  round((sum(df_cuadro5$Total_defunciones) / sum(Pob1887$Total_poblacion)) * 100, 2),
  "%\n- la población de los Municipios afectados:",
  round((sum(df_cuadro5$Total_defunciones) / sum(df_cuadro5$Total_poblacion)) * 100, 2),
  "%\n- las invasiones:",
  round((sum(df_cuadro5$Total_defunciones) / sum(df_cuadro5$Total_invasiones)) * 100, 2), "%"
)

cat(
  paste0(FECHA_STR, " de comienzo: ", format(min(df_colera.merged.day$Fecha, na.rm = TRUE), format = "%d de %B de %Y")), "\n",
  paste0(FECHA_STR, " de finalización: ", format(max(df_colera.merged.day$Fecha, na.rm = TRUE), format = "%d de %B de %Y")), "\n",
  paste0("Duración de la epidemia: ", max(df_colera.merged.day$Fecha, na.rm = TRUE) - min(df_colera.merged.day$Fecha, na.rm = TRUE), " días"), "\n",
  paste0("Intensidad media diaria de mortalidad: ", round((sum(df_cuadro5$Total_defunciones) / 153), 2), " fallecidos") 
)


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
  scale_x_continuous(breaks = 6:11, labels = MONTHS_STR) +
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
df_cuadro5_4$Fecha <- factor(df_cuadro5_4$Fecha, levels = c(6, 7, 8, 9, 10, 11), labels = MONTHS_STR)
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
    Fecha == 6 ~ MONTHS_STR[1], Fecha == 7 ~ MONTHS_STR[2],
    Fecha == 8 ~ MONTHS_STR[3], Fecha == 9 ~ MONTHS_STR[4],
    Fecha == 10 ~ MONTHS_STR[5], Fecha == 11 ~ MONTHS_STR[6],
    TRUE ~ as.character(Fecha)
  )) 


# plot (1)

ggplot(data = df_cuadro6[1:6,], aes(x = Fecha, y = Total_defunciones, fill = Fecha)) +
  geom_bar(stat = "identity") +
  labs(x = "Meses", y = "Fallecidos") +
  ggtitle(paste0("Fallecidos por cólera en ", ANO_STR, " (por meses)")) +
  scale_y_continuous(breaks = seq(0, 38010, 5000), limits = c(0, 38010), labels = number) +
  scale_fill_discrete(guide = "none") +
  geom_text(aes(label = Total_defunciones), hjust = 0.5, vjust = -0.5, size = 3) + 
  theme_bw() +
  scale_x_discrete(limits = MONTHS_STR)

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro6.png", sep = "/"), dpi = 300, limitsize = TRUE)

df_cuadro6.total <- df_cuadro6 %>%
  mutate(Fecha = str_pad(Fecha, width = 11, side = "right", pad = " ")) %>%
  unite(Fecha_Total, Fecha, Total_defunciones, sep = "    ") %>%
  rename("Fallecidos por cólera en 1885 (por meses)" = Fecha_Total)

df_cuadro6.total


# resumen general por municipios de las invasiones y defunciones ----------
# ocurridas por colera en España durante el año de 1885 -------------------


df_cuadro7 <- df_colera.merged.day %>%
  group_by(`Codigo Ine`, Municipio, Total_poblacion) %>%
  summarize(
    Primer_caso = min(Fecha),
    Ultimo_caso = max(Fecha),
    Total_invasiones = sum(Total_invasiones),
    Total_defunciones = sum(Total_defunciones)
  )

df_cuadro7$Total_dias <- ifelse(df_cuadro7$Ultimo_caso - df_cuadro7$Primer_caso == 0, 1, df_cuadro7$Ultimo_caso - df_cuadro7$Primer_caso)
df_cuadro7$Intensidad_diaria <- round((df_cuadro7$Total_defunciones / df_cuadro7$Total_dias), 2)
df_cuadro7$Tasa_mortalidad <- round((df_cuadro7$Total_defunciones / df_cuadro7$Total_poblacion) * 100, 2)
df_cuadro7$Tasa_invasiones <- ifelse(df_cuadro7$Total_invasiones == 0, 0, round((df_cuadro7$Total_defunciones / df_cuadro7$Total_invasiones) * 100, 2))
df_cuadro7 <- df_cuadro7[, c(1, 2, 3, 4, 5, 8, 6, 7, 9, 10, 11)]

df_cuadro7.total <- tibble(
  `Codigo Ine` = 00000, Municipio = "Total",
  Total_poblacion = sum(df_cuadro7$Total_poblacion),
  Primer_caso = min(df_cuadro7$Primer_caso),
  Ultimo_caso = max(df_cuadro7$Ultimo_caso),
  Total_dias = unique(df_cuadro7$Total_dias),
  Total_invasiones = sum(df_cuadro7$Total_invasiones),
  Total_defunciones = sum(df_cuadro7$Total_defunciones),
  Intensidad_diaria = mean(df_cuadro7$Intensidad_diaria),
  Tasa_mortalidad = mean(df_cuadro7$Tasa_mortalidad),
  Tasa_invasiones = mean(df_cuadro7$Tasa_invasiones)
)

df_cuadro7 <- bind_rows(df_cuadro7, df_cuadro7.total)
head(df_cuadro7)


# municipios con mayor % (>2) de defunciones ------------------------------
# respecto a la población sometida a epidemia -----------------------------


df_cuadro11 <- df_cuadro7[, c(CODIGO_INE_STR, MUNICIPIO_STR, TOTAL_POBLACION_STR, TOTAL_INVASIONES_STR, TOTAL_DEFUNCIONES_STR, TASA_MORTALIDAD_STR)]
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


# intensidad diaria fallecidos por día ------------------------------------
# (>3 por día de invasión) ------------------------------------------------


df_cuadro12 <- df_cuadro7[, c(CODIGO_INE_STR, MUNICIPIO_STR, TOTAL_POBLACION_STR, TOTAL_INVASIONES_STR, TOTAL_DEFUNCIONES_STR, "Intensidad_diaria")]
df_cuadro12 <- subset(df_cuadro12, Intensidad_diaria > 3)
df_cuadro12 <- df_cuadro12 %>%
  group_by(Municipio) %>%
  summarise(Intensidad_diaria = sum(Intensidad_diaria))
df_cuadro12 <- df_cuadro12[order(df_cuadro12$Intensidad_diaria, decreasing = TRUE),]
head(df_cuadro12, 13)


# plot (1)

ggplot(df_cuadro12[1:15,], aes(x = Intensidad_diaria, y = Municipio, fill = Municipio)) +
  geom_bar(stat = "identity") + 
  labs(x = "Intensidad diaria (%)", y = MUNICIPIO_STR) +
  ggtitle("Intensidad diaria fallecidos por día (>3 por día de invasión)") +
  scale_x_continuous(breaks = seq(0, 25, 1), limits = c(0, 25), labels = number) +
  geom_text(aes(label = Intensidad_diaria), hjust = -0.2, size = 3) + 
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 12))

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro12.png", sep = "/"), dpi = 300, limitsize = TRUE)


# permanencia de la epidemia ----------------------------------------------


day_categories <- c("De menos de 5 días", "De 6 a 10 días", "De 11 a 20 días", "De 21 a 30 días", 
                    "De 31 a 40 días", "De 41 a 50 días", "De 51 a 60 días", "De 61 a 70 días", 
                    "De 71 a 80 días", "De 81 a 90 días", "De 91 a 100 días", "De más de 100 días")


df_cuadro13.subset <- subset(df_colera.merged.day, Total_invasiones != 0)
df_cuadro13.subset <- df_cuadro13.subset %>%
  group_by(`Codigo Ine`, Municipio, Total_poblacion) %>%
  summarize(
    Primer_caso = min(Fecha),
    Ultimo_caso = max(Fecha),
    Total_invasiones = sum(Total_invasiones),
    Total_defunciones = sum(Total_defunciones),
    Tasa_mortalidad = sum(Tasa_mortalidad)
  )

df_cuadro13.subset$Total_dias <- ifelse(df_cuadro13.subset$Ultimo_caso - df_cuadro13.subset$Primer_caso == 0, 1, df_cuadro13.subset$Ultimo_caso - df_cuadro13.subset$Primer_caso)
df_cuadro13 <- df_cuadro13.subset[, c("Total_dias", MUNICIPIO_STR)]
df_cuadro13 <- factorize(
  df_cuadro13,
  "Total_dias",
  c(-1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, Inf),
  day_categories,
  FALSE
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

ggplot(data = df_cuadro13.long[1:24, ], aes(x = Total_dias, y = value, fill = column)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = NULL, y = NULL, fill = NULL) +
  ggtitle("Permanencia de la epidemia") +
  scale_y_continuous(breaks = seq(0, 360, 10), limits = c(0, 360), labels = number) +
  scale_fill_manual(values = c("Numero_de_Municipios" = "red", "Fallecidos (%)" = "blue")) +
  geom_text(aes(label = value), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom") +
  scale_x_discrete(limits = day_categories)

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro13.png", sep = "/"), height = 11, dpi = 300, limitsize = TRUE)


# porcentaje de población que fallece -------------------------------------
# por cólera en distintos municipios --------------------------------------


mortalityy_categories <- c("Hasta el 0.5 %", "Del 0.51 al 1 %", "Del 1.01 al 2 %", "Del 2.01 al 3 %", 
                           "Del 3.01 al 4 %", "Del 4.01 al 5 %", "Del 5.01 al 6 %", "Del 6.01 al 7 %", 
                           "Del 7.01 al 8 %", "Del 8.01 al 9 %", "Del 9.01 al 10 %", "De más del 10 %")
  
df_cuadro14 <- df_cuadro13.subset[, c(TASA_MORTALIDAD_STR, MUNICIPIO_STR)]
df_cuadro14 <- factorize(
  df_cuadro14,
  TASA_MORTALIDAD_STR,
  c(-1, 0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, Inf),
  mortalityy_categories,
  FALSE
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
  scale_y_continuous(breaks = seq(0, 490, 20), limits = c(0, 490), labels = number) +
  scale_fill_manual(values = c("Numero_de_Municipios" = "red", "Fallecidos (%)" = "blue")) +
  geom_text(aes(label = value), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "bottom", plot.title = element_text(size = 12)) +
  scale_x_discrete(limits = mortalityy_categories)

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro14.png", sep = "/"), height = 11, dpi = 300, limitsize = TRUE)


# municipios con mayor porcentaje de fallecidos ---------------------------
# con respecto a la población (>10 %) -------------------------------------


df_cuadro15 <- df_cuadro7[, c(CODIGO_INE_STR, MUNICIPIO_STR, TOTAL_POBLACION_STR, TOTAL_INVASIONES_STR, TOTAL_DEFUNCIONES_STR, TASA_MORTALIDAD_STR)]
df_cuadro15 <- subset(df_cuadro15, Tasa_mortalidad > 10)
df_cuadro15 <- df_cuadro15 %>%
  group_by(Municipio) %>%
  summarise(Tasa_mortalidad = sum(Tasa_mortalidad))
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


df_cuadro17 <- subset(df_colera.merged.day, Municipio == "Monteagudo")
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
    Fecha == 6 ~ MONTHS_STR[1], Fecha == 7 ~ MONTHS_STR[2],
    Fecha == 8 ~ MONTHS_STR[3], Fecha == 9 ~ MONTHS_STR[4],
    Fecha == 10 ~ MONTHS_STR[5], Fecha == 11 ~ MONTHS_STR[6],
    TRUE ~ as.character(Fecha)
  ))

df_cuadro17


# plot (1)

ggplot(data = df_cuadro17[1:6,], aes(x = Fecha, y = Total_defunciones, fill = Fecha)) +
  geom_bar(stat = "identity") +
  labs(x = "Meses", y = "Fallecidos") +
  ggtitle(paste0("Fallecidos por cólera en Monteagudo (Soria), en ", ANO_STR, " (por meses)")) +
  scale_y_continuous(breaks = seq(0, 270, 10), limits = c(0, 270), labels = number) +
  scale_fill_discrete(guide = FALSE) +
  geom_text(aes(label = Total_defunciones), hjust = 0.5, vjust = -0.5, size = 3) + 
  theme_bw() +
  scale_x_discrete(limits = MONTHS_STR)

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro17.png", sep = "/"), dpi = 300, limitsize = TRUE)


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


# capitales de provincia con % fallecidos por cólera ----------------------
# con respecto a su población ---------------------------------------------


df_cuadro77 <- df_cuadro7[, c(CODIGO_INE_STR, MUNICIPIO_STR, TASA_MORTALIDAD_STR)]
df_cuadro77$Municipio <- tolower(iconv(df_cuadro77$Municipio, from = "UTF-8", to = "ASCII//TRANSLIT"))
df_cuadro77$isCapital <- ifelse(df_cuadro77$Municipio %in% CAPITALES_STR, TRUE, FALSE)

df_cuadro77.capitales <- subset(df_cuadro77, isCapital == TRUE)
df_cuadro77.capitales$isCapital <- NULL
df_cuadro77.capitales <- df_cuadro77.capitales[order(df_cuadro77.capitales$Tasa_mortalidad, decreasing = TRUE),]
head(df_cuadro77, 10)


# plot (1)

ggplot(df_cuadro77.capitales[1:10,], aes(x = Tasa_mortalidad, y = Municipio, fill = Municipio)) +
  geom_bar(stat = "identity") + 
  labs(x = "Fallecidos (%)", y = MUNICIPIO_STR) +
  ggtitle("Capitales de provincia con % fallecidos por cólera con respecto a su población") +
  scale_x_continuous(breaks = seq(0, 5, 1), limits = c(0, 5), labels = number) +
  geom_text(aes(label = Tasa_mortalidad), hjust = -0.2, size = 3) + 
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 12))

ggsave(paste(COLERA_PLOTS_DIR, "df_cuadro77.capitales.png", sep = "/"), dpi = 300, limitsize = TRUE)


# clean environment -------------------------------------------------------


rm(
  df_cuadro1,
  df_cuadro11,
  df_cuadro12,
  df_cuadro13,
  df_cuadro13.long,
  df_cuadro13.subset,
  df_cuadro14,
  df_cuadro14.long,
  df_cuadro15,
  df_cuadro17,
  df_cuadro18,
  df_cuadro2,
  df_cuadro3,
  df_cuadro3_2,
  df_cuadro4,
  df_cuadro4.long,
  df_cuadro5,
  df_cuadro5_2,
  df_cuadro5_3,
  df_cuadro5_4,
  df_cuadro6,
  df_cuadro6.total,
  df_cuadro7,
  df_cuadro7.total,
  df_cuadro77,
  df_cuadro77.capitales
)


# maps --------------------------------------------------------------------


mapS.municipios <- read_shapefile("Municipios_IGN.shp")
mapS.provincias <- read_shapefile("Provincias_ETRS89_30N.shp")
mapRailwayLines <- read_shapefile("RailwayLines_int.shp")
mapRivers <- read_shapefile("Rius.shp")


# format "Texto" of mapS.provincias and "NAMEUNIT" of mapS.municipios, and remove from maps the Canary Islands, "Ceuta" and "Melilla"

mapS.provincias <- subset(mapS.provincias, !(Cod_CCAA %in% c("05", "18", "19")))
mapS.provincias$Texto <- tolower(iconv(mapS.provincias$Texto, from = "UTF-8", to = "ASCII//TRANSLIT"))
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7") 
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001)))
mapS.municipios$NAMEUNIT <- sapply(strsplit(mapS.municipios$NAMEUNIT, "/"), function(x) x[1])

head(mapS.provincias)
head(mapS.municipios)


# raw data ----------------------------------------------------------------


# add coordinates from df_colera and merge with df_colera.merged.month, df_colera.merged.day and df_colera.merged.week

df_colera_coord <- df_colera[, c(CODIGO_INE_STR, "LAT_POB_new_num", "LNG_POB_new_num")]
df_colera_coord <- distinct(df_colera_coord, .keep_all = TRUE)
df_colera_coord <- na.omit(df_colera_coord)
colnames(df_colera_coord)[2:3] <- c(LAT_STR, LONG_STR)
head(df_colera_coord)

df_colera_eda.month <- merge_coords(df_colera.merged.month, df_colera_coord)
df_colera_eda.day <- merge_coords(df_colera.merged.day, df_colera_coord)
df_colera_eda.week <- merge_coords(df_colera.merged.week, df_colera_coord)

head(df_colera_eda.month)
head(df_colera_eda.day)
head(df_colera_eda.week)


# maps with raw data ------------------------------------------------------


# format and merge mapS.provincias with df_colera_eda.month

df_colera_eda.month.merged.provincias <- merge(mapS.provincias, df_colera_eda.month, by.x = "Texto", by.y = PROVINCIA_STR)
df_colera_eda.month.merged.provincias$Texto_Alt <- NULL
colnames(df_colera_eda.month.merged.provincias)[1:2] <- c(PROVINCIA_STR, paste0("Cod_", PROVINCIA_STR))
head(df_colera_eda.month.merged.provincias)


# merge mapS.municipios and df_colera_eda.month, df_colera_eda.day and df_colera_eda.week

df_colera_eda.month.merged.municipios <- merge(mapS.municipios, df_colera_eda.month, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
df_colera_eda.day.merged <- merge(mapS.municipios, df_colera_eda.day, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
df_colera_eda.week.merged <- merge(mapS.municipios, df_colera_eda.week, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(df_colera_eda.day.merged)
head(df_colera_eda.day.merged)
head(df_colera_eda.week.merged)


# clean environment -------------------------------------------------------


rm(df_colera_coord, df_colera_eda.month, df_colera_eda.day, df_colera_eda.week)


# plot maps ---------------------------------------------------------------


# intensidad gradual de la epidemia en las provincias ---------------------


df_colera_eda.month.provincias <- df_colera_eda.month.merged.provincias %>%
  group_by(Provincia, Cod_Provincia, Cod_CCAA, CCAA, `Codigo Ine`, Fecha, lat, long, geometry) %>%
    summarize(
      Total_invasiones = sum(Total_invasiones),
      Tasa_incidencia = sum(Tasa_incidencia),
      Total_defunciones = sum(Total_defunciones),
      Tasa_mortalidad = sum(Tasa_mortalidad),
      Total_poblacion = sum(Total_poblacion)
    )

df_colera_eda.month.provincias <- factorize(
  df_colera_eda.month.provincias,
  TOTAL_DEFUNCIONES_STR,
  c(-1, 800, 1600, 2400, Inf), 
  c("De menos de 800", "De 800 a 1600", "De 1600 a 2400", "De 2400 a 3200"),
  TRUE,
  DEFUNCIONES_FACTOR_STR
)


# plot (1)

m6 <-
  create_tmap(
    df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 6,],
    mapS.provincias,
    DEFUNCIONES_FACTOR_STR,
    "Número de fallecidos por provincia",
    "cat",
    c(min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 6,]$long), min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 6,]$lat), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 6,]$long), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 6,]$lat))
  ) + 
  tm_shape(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 6,]) + tm_text(PROVINCIA_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m6, filename = paste(COLERA_MAPS_DIR, "m6.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")

m7 <-
  create_tmap(
    df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 7,],
    mapS.provincias,
    DEFUNCIONES_FACTOR_STR,
    "Número de fallecidos por provincia",
    "cat",
    c(min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 7,]$long), min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 7,]$lat), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 7,]$long), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 7,]$lat))
  ) + 
  tm_shape(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 7,]) + tm_text(PROVINCIA_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m7, filename = paste(COLERA_MAPS_DIR, "m7.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")

m8 <-
  create_tmap(
    df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 8,],
    mapS.provincias,
    DEFUNCIONES_FACTOR_STR,
    "Número de fallecidos por provincia",
    "cat",
    c(min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 8,]$long), min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 8,]$lat), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 8,]$long), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 8,]$lat))
  ) + 
  tm_shape(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 8,]) + tm_text(PROVINCIA_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m8, filename = paste(COLERA_MAPS_DIR, "m8.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")

m9 <-
  create_tmap(
    df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 9,],
    mapS.provincias,
    DEFUNCIONES_FACTOR_STR,
    "Número de fallecidos por provincia",
    "cat",
    c(min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 9,]$long), min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 9,]$lat), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 9,]$long), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 9,]$lat))
  ) + 
  tm_shape(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 9,]) + tm_text(PROVINCIA_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m9, filename = paste(COLERA_MAPS_DIR, "m9.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")

m10 <-
  create_tmap(
    df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 10,],
    mapS.provincias,
    DEFUNCIONES_FACTOR_STR,
    "Número de fallecidos por provincia",
    "cat",
    c(min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 10,]$long), min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 10,]$lat), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 10,]$long), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 10,]$lat))
  ) + 
  tm_shape(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 10,]) + tm_text(PROVINCIA_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m10, filename = paste(COLERA_MAPS_DIR, "m10.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")

m11 <-
  create_tmap(
    df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 11,],
    mapS.provincias,
    DEFUNCIONES_FACTOR_STR,
    "Número de fallecidos por provincia",
    "cat",
    c(min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 11,]$long), min(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 11,]$lat), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 11,]$long), max(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 11,]$lat))
  ) + 
  tm_shape(df_colera_eda.month.provincias[df_colera_eda.month.provincias$Fecha == 11,]) + tm_text(PROVINCIA_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m11, filename = paste(COLERA_MAPS_DIR, "m11.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# plot (2)

m611 <-
  create_tmapByFecha(
    df_colera_eda.month.provincias,
    mapS.provincias,
    DEFUNCIONES_FACTOR_STR,
    "Número de fallecidos por provincia",
    "cat",
    c(min(df_colera_eda.month.provincias$long), min(df_colera_eda.month.provincias$lat), max(df_colera_eda.month.provincias$long), max(df_colera_eda.month.provincias$lat)),
    2
  ) + 
  tm_shape(df_colera_eda.month.provincias) + tm_text(PROVINCIA_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m611, filename = paste(COLERA_MAPS_DIR, "m611.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión progresiva durante la 1a quincena de Junio ---------------------


df_colera_eda.week.1aquin.junio <- df_quincena(df_colera_eda.week.merged, 25, 26)
head(df_colera_eda.week.1aquin.junio)


# plot (1)

m2526 <-
  create_tmap(
    df_colera_eda.week.1aquin.junio,
    mapS.municipios,
    TOTAL_INVASIONES_STR,
    "Número de invadidos por municipio",
    "jenks",
    c(min(df_colera_eda.week.1aquin.junio$long), min(df_colera_eda.week.1aquin.junio$lat), max(df_colera_eda.week.1aquin.junio$long), max(df_colera_eda.week.1aquin.junio$lat))
  ) + 
  tm_shape(df_colera_eda.week.1aquin.junio[df_colera_eda.week.1aquin.junio$Total_invasiones > 131, ]) + tm_text(NAMEUNIT_STR, size = 0.6) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m2526, filename = paste(COLERA_MAPS_DIR, "m2526.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión progresiva durante la 2a quincena de Junio ---------------------


df_colera_eda.week.2aquin.junio <- df_quincena(df_colera_eda.week.merged, 27, 28)
head(df_colera_eda.week.2aquin.junio)


# plot (1)

m2728 <-
  create_tmap(
    df_colera_eda.week.2aquin.junio,
    mapS.municipios,
    TOTAL_INVASIONES_STR,
    "Número de invadidos por municipio",
    "jenks",
    c(min(df_colera_eda.week.2aquin.junio$long), min(df_colera_eda.week.2aquin.junio$lat), max(df_colera_eda.week.2aquin.junio$long), max(df_colera_eda.week.2aquin.junio$lat))
  ) + 
  tm_shape(df_colera_eda.week.2aquin.junio[df_colera_eda.week.2aquin.junio$Total_invasiones > 81, ]) + tm_text(NAMEUNIT_STR, size = 0.6) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m2728, filename = paste(COLERA_MAPS_DIR, "m2728.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión progresiva durante la 1a quincena de Julio ---------------------


df_colera_eda.week.1aquin.julio <- df_quincena(df_colera_eda.week.merged, 29, 30)
head(df_colera_eda.week.1aquin.julio)


# plot (1)

m2930 <-
  create_tmap(
    df_colera_eda.week.1aquin.julio,
    mapS.municipios,
    TOTAL_INVASIONES_STR,
    "Número de invadidos por municipio",
    "jenks",
    c(min(df_colera_eda.week.1aquin.julio$long), min(df_colera_eda.week.1aquin.julio$lat), max(df_colera_eda.week.1aquin.julio$long), max(df_colera_eda.week.1aquin.julio$lat))
  ) + 
  tm_shape(df_colera_eda.week.1aquin.julio[df_colera_eda.week.1aquin.julio$Total_invasiones > 202, ]) + tm_text(NAMEUNIT_STR, size = 0.6) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m2930, filename = paste(COLERA_MAPS_DIR, "m2930.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión progresiva durante la 2a quincena de Julio ---------------------


df_colera_eda.week.2aquin.julio <- df_quincena(df_colera_eda.week.merged, 31, 32)
head(df_colera_eda.week.2aquin.julio)


# plot (1)

m3132 <-
  create_tmap(
    df_colera_eda.week.2aquin.julio,
    mapS.municipios,
    TOTAL_INVASIONES_STR,
    "Número de invadidos por municipio",
    "jenks",
    c(min(df_colera_eda.week.2aquin.julio$long), min(df_colera_eda.week.2aquin.julio$lat), max(df_colera_eda.week.2aquin.julio$long), max(df_colera_eda.week.2aquin.julio$lat))
  ) + 
  tm_shape(df_colera_eda.week.2aquin.julio[df_colera_eda.week.2aquin.julio$Total_invasiones > 274, ]) + tm_text(NAMEUNIT_STR, size = 0.6) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m3132, filename = paste(COLERA_MAPS_DIR, "m3132.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión progresiva durante Agosto y Septiembre -------------------------


df_colera_eda.month.89 <- subset(df_colera_eda.month.merged.municipios, Fecha %in% c(8, 9))
df_colera_eda.month.89 <- df_colera_eda.month.89 %>%
  group_by(CODIGOINE, OBJECTID, INSPIREID, NATCODE, NAMEUNIT, CODNUT1, CODNUT2, CODNUT3, Provincia, Total_poblacion, lat, long, geometry) %>%
  summarize(
    Total_invasiones = sum(as.numeric(Total_invasiones)),
    Tasa_incidencia = sum(as.numeric(Tasa_incidencia)),
    Total_defunciones = sum(as.numeric(Total_defunciones)),
    Tasa_mortalidad = sum(as.numeric(Tasa_mortalidad))
  )
head(df_colera_eda.month.89)


# plot (1)

m89 <-
  create_tmap(
    df_colera_eda.month.89,
    mapS.municipios,
    TOTAL_INVASIONES_STR,
    "Número de invadidos por municipio",
    "jenks",
    c(min(df_colera_eda.month.89$long), min(df_colera_eda.month.89$lat), max(df_colera_eda.month.89$long), max(df_colera_eda.month.89$lat))
  ) + 
  tm_shape(df_colera_eda.month.89[df_colera_eda.month.89$Total_invasiones > 615, ]) + tm_text(NAMEUNIT_STR, size = 0.6) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m89, filename = paste(COLERA_MAPS_DIR, "m89.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión colérica en la provincia de Valencia ---------------------------
# durante el mes de Junio -------------------------------------------------


df_colera_eda.month.valencia.6 <- subset(df_colera_eda.month.merged.municipios, Provincia == "valencia" & Fecha == 6)
mapS.valencia <- subset(mapS.municipios, CODNUT3 %in% c("ES522", "ES423", "ES616", "ES220", "ES417", "ES514", "ES242", "ES523")) 


# plot (1)

m6.valencia <-
  create_tmap(
    df_colera_eda.month.valencia.6,
    mapS.valencia,
    TOTAL_INVASIONES_STR,
    "Número de invasiones en Valencia",
    "jenks",
    c(min(df_colera_eda.month.valencia.6$long), min(df_colera_eda.month.valencia.6$lat), max(df_colera_eda.month.valencia.6$long), max(df_colera_eda.month.valencia.6$lat))
  ) + 
  tm_shape(df_colera_eda.month.valencia.6[df_colera_eda.month.valencia.6$Total_invasiones > 64, ]) + tm_text(NAMEUNIT_STR, size = 0.6) + # 0.7 for interactive map
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  ) # for non interactive map

tmap_save(m6.valencia, filename = paste(COLERA_MAPS_DIR, "m6.valencia.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# plot (2)

# tmap_leaflet(m6.valencia) %>% addLegend("topright", colors = "#FF3333", labels = "Líneas de tren", title = "Transporte multimodal")
# TODO: add two legends

tmap_mode("view")
m6.valencia
tmap_mode("plot")


# durante los meses de Julio y Agosto -------------------------------------


df_colera_eda.month.valencia.78 <- subset(df_colera_eda.month.merged.municipios, Provincia == "valencia" & Fecha %in% c(7, 8))


# plot (1)

m78.valencia <-
  create_tmap(
    df_colera_eda.month.valencia.78,
    mapS.valencia,
    TOTAL_INVASIONES_STR,
    "Número de invasiones en Valencia",
    "jenks",
    c(min(df_colera_eda.month.valencia.78$long), min(df_colera_eda.month.valencia.78$lat), max(df_colera_eda.month.valencia.78$long), max(df_colera_eda.month.valencia.78$lat))
  ) + 
  tm_shape(df_colera_eda.month.valencia.78[df_colera_eda.month.valencia.78$Total_invasiones > 164, ]) + tm_text(NAMEUNIT_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  ) 

tmap_save(m78.valencia, filename = paste(COLERA_MAPS_DIR, "m78.valencia.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión colérica en la provincia de Zaragoza ---------------------------


df_colera_eda.month.zaragoza <- subset(df_colera_eda.month.merged.municipios, Provincia == "zaragoza")
df_colera_eda.month.zaragoza <- df_colera_eda.month.zaragoza %>%
  group_by(CODIGOINE, OBJECTID, INSPIREID, NATCODE, NAMEUNIT, CODNUT1, CODNUT2, CODNUT3, Provincia, Total_poblacion, lat, long, geometry) %>%
  summarize(
    Total_invasiones = sum(as.numeric(Total_invasiones)),
    Tasa_incidencia = sum(as.numeric(Tasa_incidencia)),
    Total_defunciones = sum(as.numeric(Total_defunciones)),
    Tasa_mortalidad = sum(as.numeric(Tasa_mortalidad))
  )

mapS.zaragoza <- subset(mapS.municipios, CODNUT3 %in% c("ES522", "ES423", "ES512", "ES614", "ES424", "ES241", "ES513", "ES300", "ES620", "ES415", "ES416", "ES514", "ES242", "ES425", "ES523", "ES243")) 


# plot (1)

m611.zaragoza <-
  create_tmap(
    df_colera_eda.month.zaragoza,
    mapS.zaragoza,
    TOTAL_INVASIONES_STR,
    "Número de invasiones en Zaragoza",
    "jenks",
    c(min(df_colera_eda.month.zaragoza$long), min(df_colera_eda.month.zaragoza$lat), max(df_colera_eda.month.zaragoza$long), max(df_colera_eda.month.zaragoza$lat))
  ) + 
  tm_shape(df_colera_eda.month.zaragoza[df_colera_eda.month.zaragoza$Total_invasiones > 261, ]) + tm_text(NAMEUNIT_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m611.zaragoza, filename = paste(COLERA_MAPS_DIR, "m611.zaragoza.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión colérica en las provincias de Granada y Málaga -----------------


df_colera_eda.month.granada_malaga <- subset(df_colera_eda.month.merged.municipios, Provincia %in% c("granada", "malaga"))
df_colera_eda.month.granada_malaga <- df_colera_eda.month.granada_malaga %>%
  group_by(CODIGOINE, OBJECTID, INSPIREID, NATCODE, NAMEUNIT, CODNUT1, CODNUT2, CODNUT3, Provincia, Total_poblacion, lat, long, geometry) %>%
  summarize(
    Total_invasiones = sum(as.numeric(Total_invasiones)),
    Tasa_incidencia = sum(as.numeric(Tasa_incidencia)),
    Total_defunciones = sum(as.numeric(Total_defunciones)),
    Tasa_mortalidad = sum(as.numeric(Tasa_mortalidad))
  )

mapS.granada_malaga <- subset(mapS.municipios, CODNUT3 %in% c("ES522", "ES423", "ES614", "ES616", "ES617", "ES414")) 


# plot (1)

m611.granada_malaga <-
  create_tmap(
    df_colera_eda.month.granada_malaga,
    mapS.granada_malaga,
    TOTAL_INVASIONES_STR,
    "Número de invasiones en Granada y Málaga",
    "jenks",
    c(min(df_colera_eda.month.granada_malaga$long), min(df_colera_eda.month.granada_malaga$lat), max(df_colera_eda.month.granada_malaga$long), max(df_colera_eda.month.granada_malaga$lat))
  ) + 
  tm_shape(df_colera_eda.month.granada_malaga[df_colera_eda.month.granada_malaga$Total_invasiones > 270, ]) + tm_text(NAMEUNIT_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  ) 

tmap_save(m611.granada_malaga, filename = paste(COLERA_MAPS_DIR, "m611.granada_malaga.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión colérica en las provincias de Murcia, Castellón y Alicante -----


df_colera_eda.month.murcia_castellon_alicante <- subset(df_colera_eda.month.merged.municipios, Provincia %in% c("murcia", "castellon", "alicante"))
df_colera_eda.month.murcia_castellon_alicante <- df_colera_eda.month.murcia_castellon_alicante %>%
  group_by(CODIGOINE, OBJECTID, INSPIREID, NATCODE, NAMEUNIT, CODNUT1, CODNUT2, CODNUT3, Provincia, Total_poblacion, lat, long, geometry) %>%
  summarize(
    Total_invasiones = sum(as.numeric(Total_invasiones)),
    Tasa_incidencia = sum(as.numeric(Tasa_incidencia)),
    Total_defunciones = sum(as.numeric(Total_defunciones)),
    Tasa_mortalidad = sum(as.numeric(Tasa_mortalidad))
  )

mapS.murcia_castellon_alicante <- subset(mapS.municipios, CODNUT3 %in% c("ES522", "ES614", "ES424", "ES513", "ES620", "ES220", "ES416")) 


# plot (1)

m611.murcia_castellon_alicante <-
  create_tmap(
    df_colera_eda.month.murcia_castellon_alicante,
    mapS.murcia_castellon_alicante,
    TOTAL_INVASIONES_STR,
    "Número de invasiones en Murcia, Castellon y Alicante",
    "jenks",
    c(min(df_colera_eda.month.murcia_castellon_alicante$long), min(df_colera_eda.month.murcia_castellon_alicante$lat), max(df_colera_eda.month.murcia_castellon_alicante$long), max(df_colera_eda.month.murcia_castellon_alicante$lat))
  ) + 
  tm_shape(df_colera_eda.month.murcia_castellon_alicante[df_colera_eda.month.murcia_castellon_alicante$Total_invasiones > 574, ]) + tm_text(NAMEUNIT_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren"),
    col = c("red"),
    title = "Transporte multimodal"
  )

tmap_save(m611.murcia_castellon_alicante, filename = paste(COLERA_MAPS_DIR, "m611.murcia_castellon_alicante.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión colérica en las cuencas del Guadalaviar y Jucar ----------------
# Guadalaviar, se convierte en Río Blanco y desemboca como Turia

df_colera_eda.month.guadalaviar_jucar <- subset(df_colera_eda.month.merged.municipios, Provincia %in% c("teruel", "cuenca", "albacete", "valencia"))
df_colera_eda.month.guadalaviar_jucar <- df_colera_eda.month.guadalaviar_jucar %>%
  group_by(CODIGOINE, OBJECTID, INSPIREID, NATCODE, NAMEUNIT, CODNUT1, CODNUT2, CODNUT3, Provincia, Total_poblacion, lat, long, geometry) %>%
  summarize(
    Total_invasiones = sum(as.numeric(Total_invasiones)),
    Tasa_incidencia = sum(as.numeric(Tasa_incidencia)),
    Total_defunciones = sum(as.numeric(Total_defunciones)),
    Tasa_mortalidad = sum(as.numeric(Tasa_mortalidad))
  )

mapS.guadalaviar_jucar <- subset(mapS.municipios, CODNUT3 %in% c("ES522", "ES613", "ES423", "ES614", "ES241", "ES616", "ES513", "ES230", "ES617", "ES220", "ES417", "ES514", "ES242", "ES523", "ES419"))


# plot (1)

m611.guadalaviar_jucar <-
  create_tmap(
    df_colera_eda.month.guadalaviar_jucar,
    mapS.guadalaviar_jucar,
    TOTAL_INVASIONES_STR,
    "Número de invasiones en Teruel, Cuenca, Albacete y Valencia",
    "jenks",
    c(min(df_colera_eda.month.guadalaviar_jucar$long), min(df_colera_eda.month.guadalaviar_jucar$lat), max(df_colera_eda.month.guadalaviar_jucar$long), max(df_colera_eda.month.guadalaviar_jucar$lat))
  ) + 
  tm_shape(df_colera_eda.month.guadalaviar_jucar[df_colera_eda.month.guadalaviar_jucar$Total_invasiones > 253, ]) + tm_text(NAMEUNIT_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_shape(mapRivers) + tm_lines(lwd = 1, col = "blue") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren", "Ríos"),
    col = c("red", "blue"),
    title = "Transporte multimodal"
  ) 

tmap_save(m611.guadalaviar_jucar, filename = paste(COLERA_MAPS_DIR, "m611.guadalaviar_jucar.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión colérica en las cuencas del Guadiana y Guadalquivir ------------


df_colera_eda.month.guadiana_guadalquivir <- subset(df_colera_eda.month.merged.municipios, Provincia %in% c("ciudad real", "badajoz", "jaen", "cordoba", "sevilla", "cadiz"))
df_colera_eda.month.guadiana_guadalquivir <- df_colera_eda.month.guadiana_guadalquivir %>%
  group_by(CODIGOINE, OBJECTID, INSPIREID, NATCODE, NAMEUNIT, CODNUT1, CODNUT2, CODNUT3, Provincia, Total_poblacion, lat, long, geometry) %>%
  summarize(
    Total_invasiones = sum(as.numeric(Total_invasiones)),
    Tasa_incidencia = sum(as.numeric(Tasa_incidencia)),
    Total_defunciones = sum(as.numeric(Total_defunciones)),
    Tasa_mortalidad = sum(as.numeric(Tasa_mortalidad))
  )

mapS.guadiana_guadalquivir <- subset(mapS.municipios, CODNUT3 %in% c("ES612", "ES522", "ES422", "ES613", "ES423", "ES614", "ES616"))
# TODO: tinc poca dada de cadiz

# plot (1)

m611.guadiana_guadalquivir <-
  create_tmap(
    df_colera_eda.month.guadiana_guadalquivir,
    mapS.guadiana_guadalquivir,
    TOTAL_INVASIONES_STR,
    "Número de invasiones en Ciudad Real, Badajoz, Jaén, Córdoba, Sevilla y Cádiz",
    "jenks",
    c(min(df_colera_eda.month.guadiana_guadalquivir$long), min(df_colera_eda.month.guadiana_guadalquivir$lat), max(df_colera_eda.month.guadiana_guadalquivir$long), max(df_colera_eda.month.guadiana_guadalquivir$lat))
  ) + 
  tm_shape(df_colera_eda.month.guadiana_guadalquivir[df_colera_eda.month.guadiana_guadalquivir$Total_invasiones > 263, ]) + tm_text(NAMEUNIT_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_shape(mapRivers) + tm_lines(lwd = 1, col = "blue") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren", "Ríos"),
    col = c("red", "blue"),
    title = "Transporte multimodal"
  ) 

tmap_save(m611.guadiana_guadalquivir, filename = paste(COLERA_MAPS_DIR, "m611.guadiana_guadalquivir.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión colérica en las cuencas del Tajo -------------------------------


df_colera_eda.month.tajo <- subset(df_colera_eda.month.merged.municipios, Provincia %in% c("guadalajara", "cuenca", "toledo"))
df_colera_eda.month.tajo <- df_colera_eda.month.tajo %>%
  group_by(CODIGOINE, OBJECTID, INSPIREID, NATCODE, NAMEUNIT, CODNUT1, CODNUT2, CODNUT3, Provincia, Total_poblacion, lat, long, geometry) %>%
  summarize(
    Total_invasiones = sum(as.numeric(Total_invasiones)),
    Tasa_incidencia = sum(as.numeric(Tasa_incidencia)),
    Total_defunciones = sum(as.numeric(Total_defunciones)),
    Tasa_mortalidad = sum(as.numeric(Tasa_mortalidad))
  )

mapS.tajo <- subset(mapS.municipios, CODNUT3 %in% c("ES422", "ES423", "ES614", "ES424", "ES241", "ES513", "ES230", "ES300", "ES620", "ES415", "ES242", "ES425", "ES419"))


# plot (1)

m611.tajo <-
  create_tmap(
    df_colera_eda.month.tajo,
    mapS.tajo,
    TOTAL_INVASIONES_STR,
    "Número de invasiones en Guadalajara, Cuenca y Toledo",
    "jenks",
    c(min(df_colera_eda.month.tajo$long), min(df_colera_eda.month.tajo$lat), max(df_colera_eda.month.tajo$long), max(df_colera_eda.month.tajo$lat))
  ) + 
  tm_shape(df_colera_eda.month.tajo[df_colera_eda.month.tajo$Total_invasiones > 156, ]) + tm_text(NAMEUNIT_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_shape(mapRivers) + tm_lines(lwd = 1, col = "blue") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren", "Ríos"),
    col = c("red", "blue"),
    title = "Transporte multimodal"
  ) 

tmap_save(m611.tajo, filename = paste(COLERA_MAPS_DIR, "m611.tajo.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# invasión colérica en las cuencas del Ebro -------------------------------


df_colera_eda.month.ebro <- subset(df_colera_eda.month.merged.municipios, Provincia %in% c("la rioja", "navarra", "zaragoza", "tarragona"))
df_colera_eda.month.ebro <- df_colera_eda.month.ebro %>%
  group_by(CODIGOINE, OBJECTID, INSPIREID, NATCODE, NAMEUNIT, CODNUT1, CODNUT2, CODNUT3, Provincia, Total_poblacion, lat, long, geometry) %>%
  summarize(
    Total_invasiones = sum(as.numeric(Total_invasiones)),
    Tasa_incidencia = sum(as.numeric(Tasa_incidencia)),
    Total_defunciones = sum(as.numeric(Total_defunciones)),
    Tasa_mortalidad = sum(as.numeric(Tasa_mortalidad))
  )

mapS.ebro <- subset(mapS.municipios, CODNUT3 %in% c("ES522", "ES423", "ES512", "ES614", "ES424", "ES212", "ES241", "ES616", "ES513", "ES300", "ES620", "ES220", "ES415", "ES416", "ES514", "ES242", "ES425", "ES523", "ES243"))


# plot (1)

m611.ebro <-
  create_tmap(
    df_colera_eda.month.ebro,
    mapS.ebro,
    TOTAL_INVASIONES_STR,
    "Número de invasiones en Guadalajara, Cuenca y Toledo",
    "jenks",
    c(min(df_colera_eda.month.ebro$long), min(df_colera_eda.month.ebro$lat), max(df_colera_eda.month.ebro$long), max(df_colera_eda.month.ebro$lat))
  ) + 
  tm_shape(df_colera_eda.month.ebro[df_colera_eda.month.ebro$Total_invasiones > 316, ]) + tm_text(NAMEUNIT_STR, size = 0.7) +
  tm_shape(mapRailwayLines) + tm_lines(lwd = 1, col = "red") +
  tm_shape(mapRivers) + tm_lines(lwd = 1, col = "blue") +
  tm_add_legend(
    type = "line",
    labels = c("Líneas de tren", "Ríos"),
    col = c("red", "blue"),
    title = "Transporte multimodal"
  ) 

tmap_save(m611.ebro, filename = paste(COLERA_MAPS_DIR, "m611.ebro.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# clean environment -------------------------------------------------------


rm(
  m6,
  m7,
  m8,
  m9,
  m10,
  m11,
  m611,
  m2526,
  m2728,
  m2930,
  m3132,
  m89,
  m6.valencia,
  m78.valencia,
  m611.zaragoza,
  m611.barcelona,
  m611.granada_malaga,
  m611.murcia_castellon_alicante,
  m611.guadalaviar_jucar,
  m611.guadiana_guadalquivir,
  m611.tajo,
  m611.ebro
)
