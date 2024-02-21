library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)

ANO_STR <- "1820"
DATE_FORMAT <- "%Y-%m-%d"
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


# main --------------------------------------------------------------------


# read "peste" dataset
df_peste <- read_excel(paste(DATA_DIR, "Municipis Pesta Mallorca.xlsx", sep = "/"), sheet = "Hoja1")
colnames(df_peste)[5:7] <- c("Categoria", "Sexo", "Casos")

# remove "Fichero" and "Sexo", and add "Ano"
df_peste$Fichero <- NULL
df_peste$Sexo <- NULL
df_peste$Ano <- ANO_STR

# format "Casos" and "Año" as numeric
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

# remove "Muertos" and add population for each "Localidad"
df_peste <- subset(df_peste, Categoria != "Muertos")
df_peste <- add_poblacion(df_peste)

# remove NA values
df_peste <- na.omit(df_peste)

# change "no consta" cases to 0 
df_peste$Casos <- ifelse(is.na(df_peste$Casos), 0, df_peste$Casos)
