library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
PESTE_DATA_DIR <- "peste_data"
dir.create(PESTE_DATA_DIR, showWarnings = FALSE)

ANO_STR <- "1820"
DATE_FORMAT <- "%Y-%m-%d"
INFECTADOS_STR <- "Infectados"
MUERTOS_STR <- "Muertos"
CURADOS_STR <- "Curados"
SUPERVIVIENTES_STR <- "Supervivientes"
LOCALIDADES_STR <- c("Artà", "Capdepera", "Sant Llorenç des Cardassar", "Son Servera")


# functions ---------------------------------------------------------------


add_poblacion <- function(df_peste) {
  #' Add population information to a data frame based on municipality names.
  #'
  #' This function adds population information to a data frame based on municipality names.
  #' It assigns population values from predefined lists based on municipality names.
  #'
  #' @param df_peste Data frame to which population information will be added.
  #'
  #' @return A modified data frame with population information.
  
  return(df_peste %>%
           mutate(
             Poblacion = case_when(
               Municipio == LOCALIDADES_STR[1] ~ 3626,
               Municipio == LOCALIDADES_STR[2] ~ 1179,
               Municipio == LOCALIDADES_STR[3] ~ 1338,
               Municipio == LOCALIDADES_STR[4] ~ 1684
             )
           ))
}


reduce_categories <- function(df_peste) {
  #' Reduce and recategorize disease categories in a data frame.
  #'
  #' This function reduces and recategorizes disease categories in a data frame.
  #' It combines and renames certain categories to create a simplified set of categories.
  #'
  #' @param df_peste Data frame containing disease categories to be recategorized.
  #'
  #' @return A modified data frame with simplified disease categories.
  
  return(df_peste %>% mutate(Categoria = case_when(
    Categoria == CURADOS_STR ~ CURADOS_STR,
    Categoria == MUERTOS_STR ~ MUERTOS_STR,
    Categoria == "Enfermos existentes segun parte anterior" ~ INFECTADOS_STR,
    Categoria == "Nuevamente acometidos" ~ INFECTADOS_STR, # Reinfections
    Categoria == "Pasados a convalecientes" ~ CURADOS_STR,
    Categoria == "Total de enfermos que restan" ~ INFECTADOS_STR))
  )
}


add_supervivientes <- function(df_peste, isSexo) {
  #' Add the "Supervivientes" category to the plague dataset
  #'
  #' This function calculates and adds the "Supervivientes" category to the plague dataset by 
  #' subtracting the infected cases from the deceased cases. The new "Supervivientes" category 
  #' reflects the cases of individuals who have survived the disease.
  #'
  #' @param df_peste Data Frame containing the plague data.
  #' @param isSexo Boolean variable indicating the gender of individuals.
  #' 
  #' @return Data Frame updated with the new "Supervivientes" cat
  
  df_infectados <- df_peste %>% filter(Categoria == INFECTADOS_STR)
  
  df_muertos <- df_peste %>% filter(Categoria == MUERTOS_STR)
  
  # perform the subtraction and create a new row for "Supervivientes"
  df_supervivientes <- df_infectados %>%
    left_join(
      if (isSexo) {
        df_muertos %>% select(Fecha, Municipio, Categoria, Sexo, Casos, Poblacion)
      } else {
        df_muertos %>% select(Fecha, Municipio, Categoria, Casos, Poblacion)
      },
      by = c("Fecha", "Municipio", "Poblacion", if (isSexo) "Sexo" else NULL)
    ) %>%
    mutate(
      Categoria = ifelse(!is.na(Casos.y), "Supervivientes", Categoria),
      Casos = ifelse(!is.na(Casos.y), Casos.x - Casos.y, Casos.x)
    ) %>%
    select(-Casos.x, -Casos.y)
  
  # merge the original rows with the new row of "Supervivientes"
  df_peste <- bind_rows(df_peste, df_supervivientes)
  df_peste <- df_peste %>%
    arrange(Fecha, Municipio, if (isSexo) Sexo else NULL) %>%
    select(-Categoria.x, -Categoria.y)
  
  return(df_peste)
  
}


# main --------------------------------------------------------------------


# read "peste" dataset
df_peste <- read_excel(paste(DATA_DIR, "Municipis Pesta Mallorca.xlsx", sep = "/"), sheet = "Hoja1")
colnames(df_peste)[5:7] <- c("Categoria", "Sexo", "Casos")

# remove column "Fichero" and add column "Año"
df_peste$Fichero <- NULL
df_peste$Ano <- ANO_STR

# format columns "Casos" and "Año" as numeric
df_peste$Casos <- as.numeric(df_peste$Casos)
df_peste$Ano <- as.numeric(df_peste$Ano)

# format "Fecha" as year-month-day
df_peste$Fecha <- as.Date(with(df_peste, paste(Ano, mes, dia, sep = "-")), DATE_FORMAT)
df_peste$dia <- NULL
df_peste$mes <- NULL
df_peste$Ano <- NULL

# rename municipios "arta", "San Lorenzo" and "San Lorenzo campamento" to "Artà" and "Sant Llorenç des Cardassar"
df_peste <- df_peste %>%
  mutate(
    Municipio = case_when(
      Municipio == "arta" ~ LOCALIDADES_STR[1],
      Municipio == "San Lorenzo" ~ LOCALIDADES_STR[3],
      Municipio == "San Lorenzo campamento" ~ LOCALIDADES_STR[3],
      TRUE ~ Municipio
    )
  )

# change "no consta" cases to 0 
df_peste$Casos <- ifelse(is.na(df_peste$Casos), 0, df_peste$Casos)


# TOTALES - MUNICIPIOS ----------------------------------------------------


# divide "Casos" by "Sexo" 
df_peste.sexo <- subset(df_peste, Sexo != "total")
df_peste.total <- subset(df_peste, Sexo == "total")

# reduce categories to "Infectados", "Curados", and "Muertos"
df_peste.sexo <- reduce_categories(df_peste.sexo)
df_peste.total <- reduce_categories(df_peste.total)
df_peste.total$Sexo <- NULL


# by day

df_peste.sexo.agg.day <- df_peste.sexo %>%
  group_by(Fecha, Municipio, Categoria, Sexo) %>%
  summarise(Casos = sum(Casos))

df_peste.agg.day <- df_peste.sexo %>%
  group_by(Fecha, Municipio, Categoria) %>%
  summarise(Casos = sum(Casos))

df_peste.total.agg.day <- df_peste.total %>%
  group_by(Fecha, Municipio, Categoria) %>%
  summarise(Casos = sum(Casos))

df_peste.merged.day <- rbind(df_peste.agg.day, df_peste.total.agg.day)


# by month

df_peste.sexo.agg.month <- df_peste.sexo %>%
  mutate(Fecha = month(as.POSIXlt(Fecha, format = DATE_FORMAT))) %>% 
  group_by(Fecha, Municipio, Categoria, Sexo) %>%
  summarise(Casos = sum(Casos))

df_peste.agg.month <- df_peste.sexo %>%
  mutate(Fecha = month(as.POSIXlt(Fecha, format = DATE_FORMAT))) %>% 
  group_by(Fecha, Municipio, Categoria) %>%
  summarise(Casos = sum(Casos))

df_peste.total.agg.month <- df_peste.total %>%
  mutate(Fecha = month(as.POSIXlt(Fecha, format = DATE_FORMAT))) %>% 
  group_by(Fecha, Municipio, Categoria) %>%
  summarise(Casos = sum(Casos))

df_peste.merged.month <- rbind(df_peste.agg.month, df_peste.total.agg.month)

# add "Poblacion"
df_peste.sexo.agg.day <- add_poblacion(df_peste.sexo.agg.day)
df_peste.sexo.agg.month <- add_poblacion(df_peste.sexo.agg.month)
df_peste.merged.day <- add_poblacion(df_peste.merged.day)
df_peste.merged.month <- add_poblacion(df_peste.merged.month) 

# add "Supervivientes" ("Infectados" - "Muertos")
df_peste.sexo.agg.day <- add_supervivientes(df_peste.sexo.agg.day, TRUE)
df_peste.sexo.agg.month <- add_supervivientes(df_peste.sexo.agg.month, TRUE)
df_peste.merged.day <- add_supervivientes(df_peste.merged.day, FALSE)
df_peste.merged.month <- add_supervivientes(df_peste.merged.month, FALSE)


# save as CSV -------------------------------------------------------------


write.csv(df_peste.sexo.agg.day, paste(PESTE_DATA_DIR, "peste_total_casosXmunicipiosXsexo.day.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_peste.sexo.agg.month, paste(PESTE_DATA_DIR, "peste_total_casosXmunicipiosXsexo.month.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_peste.merged.day, paste(PESTE_DATA_DIR, "peste_total_casosXmunicipios.day.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_peste.merged.month, paste(PESTE_DATA_DIR, "peste_total_casosXmunicipios.month.csv", sep = "/"), row.names = FALSE, fileEncoding = "UTF-8")


# clean environment -------------------------------------------------------


rm(df_peste.agg.day, df_peste.total.agg.day, df_peste.agg.month, df_peste.total.agg.month)
