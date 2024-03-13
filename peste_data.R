library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)

# municipios of Mallorca with black plague cases
LOCALIDADES_STR <- c("Artà", "Capdepera", "Sant Llorenç des Cardassar", "Son Servera")


# functions ---------------------------------------------------------------


add_poblacion <- function(df_peste) {
  #' Add Poblacion Column
  #'
  #' This function adds a "Poblacion" column to the input data frame.
  #'
  #' @param df_peste A data frame containing peste data.
  #'
  #' @return A data frame containing peste data with a "Poblacion" column.

  return(df_peste %>%
           mutate(
             Poblacion = case_when(
               Municipio == LOCALIDADES_STR[1] ~ 3626, # Artà
               Municipio == LOCALIDADES_STR[2] ~ 1179, # Capdepera
               Municipio == LOCALIDADES_STR[3] ~ 1338, # Sant Llorenç des Cardassar
               Municipio == LOCALIDADES_STR[4] ~ 1684  # Son Servera
             )
           ))
}


# main --------------------------------------------------------------------


# read peste data and rename columns to "Categoria", "Sexo" and "Casos"
df_peste <- read_excel(paste(DATA_DIR, "Municipis Pesta Mallorca.xlsx", sep = "/"), sheet = "Hoja1")
colnames(df_peste)[5:7] <- c("Categoria", "Sexo", "Casos")

# remove "Fichero" and "Sexo" columns, and format "Casos" as numeric
df_peste$Fichero <- NULL
df_peste$Sexo <- NULL
df_peste$Casos <- as.numeric(df_peste$Casos)

# rename "arta" to "Artà" and "San Lorenzo" or "San Lorenzo campamento" to "Sant Llorenç des Cardassar"
df_peste <- df_peste %>%
  mutate(
    Municipio = case_when(
      Municipio == "arta" ~ LOCALIDADES_STR[1],
      Municipio == "San Lorenzo" ~ LOCALIDADES_STR[3],
      Municipio == "San Lorenzo campamento" ~ LOCALIDADES_STR[3],
      TRUE ~ Municipio
    )
  )

# rename "no consta", "no hay novedad" and "se goza de perfectisima salud" to 0
df_peste <- df_peste %>%
  mutate(
    Casos = case_when(
      Casos == "no consta" ~ 0,
      Casos == "no hay novedad" ~ 0,
      Casos == "se goza de perfectisima salud" ~ 0,
      TRUE ~ Casos
    )
  )

# divide cases and deaths categories as separate data frames
muertos_str <- "Muertos"
df_peste.defunciones <- subset(df_peste, Categoria == muertos_str) # create "defunciones" subset with "Categoria" == "Muertos"
df_peste <- subset(df_peste, !(Categoria %in% c("Curados", muertos_str))) # remove "Curados" and "Muertos" from "Categoria"

# add population information
df_peste.defunciones <- add_poblacion(df_peste.defunciones)
df_peste <- add_poblacion(df_peste)

# remove "Categoria" column grouping by "Municipio", "mes", "dia" and "Poblacion"
df_peste.defunciones <- df_peste.defunciones %>% group_by(Municipio, mes, dia, Poblacion) %>% summarize(Casos = sum(Casos))
df_peste <- df_peste %>% group_by(Municipio, mes, dia, Poblacion) %>% summarize(Casos = sum(Casos))

save.image("peste_data.RData")
