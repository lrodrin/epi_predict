# load("colera_data.RData")


# constants ---------------------------------------------------------------


colera_provincias <- unique(df_colera.groupByProvinciaFecha$Provincia)
colera_municipios <- as.factor(unique(df_colera.merged.week$Municipio))


# functions ---------------------------------------------------------------


colera_epicentres <- function(df_colera, cause, county=NULL, city=NULL, limit_period, limit_epicentre, isRate) {
  #' Identify cholera epicentres based on specified criteria.
  #'
  #' This function identifies cholera epicentres based on the specified criteria, including the type of cases,
  #' geographical level of aggregation (county or city), time period, and limit for considering an epicentre.
  #'
  #' @param df_colera Data frame containing cholera data.
  #' @param cause Type of cholera cases to analyse ("invasiones" or "defunciones").
  #' @param county Name of the county for aggregation (if city is NULL).
  #' @param city Name of the city for aggregation (if provided).
  #' @param limit_period Time period for considering an epicentre (30 or 3).
  #' @param limit_epicentre Limit for considering an epicentre.
  #' @param isRate Boolean indicating whether to use rates instead of counts (if TRUE).
  
  if (is.null(city)) {
    df_colera.tmp <- subset(df_colera, Provincia == county)
    agg_level <- county
    first_case <- which(df_colera.tmp$Total_invasiones!=0)[1]
    last_case <- first_case + limit_period
    
  } else if (is.null(county)) {
    df_colera.tmp <- subset(df_colera, Municipio == city)
    agg_level <- city
    first_case <- which(df_colera.tmp$Total_invasiones!=0)[1]
    last_case <- first_case + limit_period
  }
  
  if (!is.na(first_case)) {
    
    if (cause == INVASIONES_STR) {
      if (limit_period == 30) { # 30 days after the first case
        if (!isRate) {
          if (!is.na(sum(df_colera.tmp[first_case:last_case,]$Total_invasiones)) &&
              sum(df_colera.tmp[first_case:last_case,]$Total_invasiones) >= limit_epicentre) {
            
            print(paste(INVASIONES_STR, "en", agg_level, ":", sum(df_colera.tmp[first_case:last_case,]$Total_invasiones), sep = " "))
          }
        }
      }
      else if (limit_period == 3) { # 3 weeks after the first case 
        if (!isRate) {
          if (!is.na(sum(df_colera.tmp[first_case:last_case,]$Total_invasiones)) &&
              sum(df_colera.tmp[first_case:last_case,]$Total_invasiones) >= limit_epicentre) { 
            
            print(paste(INVASIONES_STR, "en", agg_level, ":", sum(df_colera.tmp[first_case:last_case,]$Total_invasiones), sep = " "))
          }
        }
        else if (isRate) {
          if (!is.na(sum(df_colera.tmp[first_case:last_case,]$Tasa_incidencia)) &&
              sum(df_colera.tmp[first_case:last_case,]$Tasa_incidencia) >= limit_epicentre) { 
            
            print(paste(INVASIONES_STR, "en", agg_level, ":", sum(df_colera.tmp[first_case:last_case,]$Tasa_incidencia), sep = " "))
          }
        }
      }
      
    } else if (cause == DEFUNCIONES_STR) {
      if (limit_period == 30) { # 30 days after the first case
        if (!isRate) {
          if (!is.na(sum(df_colera.tmp[first_case:last_case,]$Total_defunciones)) &&
              sum(df_colera.tmp[first_case:last_case,]$Total_defunciones) >= limit_epicentre) { 
            
            print(paste(DEFUNCIONES_STR, "en", agg_level, ":", sum(df_colera.tmp[first_case:last_case,]$Total_defunciones), sep = " "))
          }
        }
      }
      else if (limit_period == 3) { # 3 weeks after the first case 
        if (!isRate) {
          if (!is.na(sum(df_colera.tmp[first_case:last_case,]$Total_defunciones)) &&
              sum(df_colera.tmp[first_case:last_case,]$Total_defunciones) >= limit_epicentre) { 
            
            print(paste(DEFUNCIONES_STR, "en", agg_level, ":", sum(df_colera.tmp[first_case:last_case,]$Total_defunciones), sep = " "))
          }
        }
        else if (isRate) {
          if (!is.na(sum(df_colera.tmp[first_case:last_case,]$Tasa_mortalidad)) &&
              sum(df_colera.tmp[first_case:last_case,]$Tasa_mortalidad) >= limit_epicentre) { 
            
            print(paste(DEFUNCIONES_STR, "en", agg_level, ":", sum(df_colera.tmp[first_case:last_case,]$Tasa_mortalidad), sep = " "))
          }
        }
      }
    }
  }
}


# main --------------------------------------------------------------------


# epicentres --------------------------------------------------------------


for (provincia in colera_provincias) { # for each "Provincia"
  
  # n invasiones and defunciones by county
  colera_epicentres(
    df_colera.groupByProvinciaFecha,
    INVASIONES_STR,
    provincia,
    NULL,
    limit_period = 30, # 30 days after the first case
    limit_epicentre = 1000, # 1000 or more invasiones
    isRate = FALSE
  )
  colera_epicentres(
    df_colera.groupByProvinciaFecha,
    DEFUNCIONES_STR,
    provincia,
    NULL,
    limit_period = 30,
    limit_epicentre = 1000, # 1000 or more defunciones
    isRate = FALSE
  )
  
}


for (municipio in colera_municipios) { # for each "Municipio"
  
  # n invasiones and defunciones by city
  colera_epicentres(df_colera.merged.week,
                    INVASIONES_STR,
                    NULL,
                    municipio,
                    limit_period = 3, # 3 weeks after the first case
                    limit_epicentre = 500, # 500 or more invasiones
                    isRate = FALSE)
  
  colera_epicentres(df_colera.merged.week,
                    DEFUNCIONES_STR,
                    NULL,
                    municipio,
                    limit_period = 3,
                    limit_epicentre = 300, # 300 or more invasiones
                    isRate = FALSE)
  
  # % invasiones and defunciones by city
  colera_epicentres(df_colera.merged.week,
                    INVASIONES_STR,
                    NULL,
                    municipio,
                    limit_period = 3,
                    limit_epicentre = 10,
                    isRate = TRUE)
  
  colera_epicentres(df_colera.merged.week,
                    DEFUNCIONES_STR,
                    NULL,
                    municipio,
                    limit_period = 3,
                    limit_epicentre = 5,
                    isRate = TRUE)
  
}
