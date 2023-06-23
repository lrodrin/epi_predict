source("colera_data.R")


# constants ---------------------------------------------------------------


colera_provincias <- unique(df_colera.groupByProvinciaFecha$Provincia)
colera_municipios <- as.factor(unique(df_colera.groupByProvinciaFechaMunicipo$Municipio))

EPICENTRE_MAX <- 100 # more than 100 invasiones/defunciones
EPICENTRE_LIMIT <- 13 # number of rows corresponding to the first month


# functions ---------------------------------------------------------------


colera_epicentres <- function(df_colera, cause, county=NULL, city=NULL) {
  
  if (is.null(city)) {
    df_colera.tmp <- subset(df_colera, Provincia == county)
    agg_level <- county
    
  } else if (is.null(county)) {
    df_colera.tmp <- subset(df_colera, Municipio == city)
    agg_level <- city
  }
  
  if (cause == INVASIONES_STR) {
    if (!is.na(sum(df_colera.tmp$Total_invasiones[1:EPICENTRE_LIMIT])) &&
        sum(df_colera.tmp$Total_invasiones[1:EPICENTRE_LIMIT]) >= EPICENTRE_MAX) { # is epicentre (more than 100 invasiones)
      
      print(paste(INVASIONES_STR, "en", agg_level, ":", sum(df_colera.tmp$Total_invasiones[1:EPICENTRE_LIMIT]), sep = " "))
    }
    
  } else if (cause == DEFUNCIONES_STR) {
    if (!is.na(sum(df_colera.tmp$Total_defunciones[1:EPICENTRE_LIMIT])) &&
        sum(df_colera.tmp$Total_defunciones[1:EPICENTRE_LIMIT]) >= EPICENTRE_MAX) { # is epicentre (more than 100 defunciones)
      
      print(paste(DEFUNCIONES_STR, "en", agg_level, ":", sum(df_colera.tmp$Total_defunciones[1:EPICENTRE_LIMIT]), sep = " "))
    }
  }
}


colera_ts <- function(df_colera, cause, county=NULL, city=NULL) {
  
  if (is.null(city)) {
    df_colera.tmp <- subset(df_colera, Provincia == county)
    agg_level <- county
    plotnamefile <- paste0(COLERA_PLOTS_DIR, "/ts.colera_total_", cause, "Xprovincia_", agg_level, ".png")
    
  } else if (is.null(county)) {
    df_colera.tmp <- subset(df_colera, Municipio == city)
    agg_level <- city
    plotnamefile <- paste0(COLERA_PLOTS_DIR, "/ts.colera_total_", cause, "Xmunicipio_", agg_level, ".png")
  }
  
  if (cause == INVASIONES_STR) {
    aes <- aes(x = Fecha, y = Total_invasiones)
    
  } else if (cause == DEFUNCIONES_STR) {
    aes <- aes(x = Fecha, y = Total_defunciones)
  }
  
  # print(
  ggplot(df_colera.tmp, aes) + 
    geom_line() +
    xlab("día-mes") +
    ylab("número") +
    ggtitle(paste0("colera ", cause, " en ", agg_level, ", ", ANO_STR)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_continuous(
      breaks = as.numeric(df_colera.tmp$Fecha),
      labels = format(df_colera.tmp$Fecha, "%d - %m")
    )
  # )
  
  ggsave(plotnamefile, width = 15.7, height = 4.5, dpi = 300, limitsize = TRUE)
  
  return(df_colera.tmp)
  
}


# main --------------------------------------------------------------------


# epicentres & plots ------------------------------------------------------


for (provincia in colera_provincias) { # for each "Provincia"
  
  # invasiones by county
  colera_epicentres(df_colera.groupByProvinciaFecha, INVASIONES_STR, provincia, NULL)
  colera_ts(df_colera.groupByProvinciaFecha, INVASIONES_STR, provincia, NULL)
  
  # defunciones by county
  colera_epicentres(df_colera.groupByProvinciaFecha, DEFUNCIONES_STR, provincia, NULL)
  colera_ts(df_colera.groupByProvinciaFecha, DEFUNCIONES_STR, provincia, NULL)
  
}


for (municipio in colera_municipios) { # for each "Municipio"
  
  # invasiones by city
  colera_epicentres(df_colera.groupByProvinciaFechaMunicipo, INVASIONES_STR, NULL, municipio)
  # colera_ts(df_colera.groupByProvinciaFechaMunicipo, INVASIONES_STR, NULL, municipio)
  
  # defunciones by city
  colera_epicentres(df_colera.groupByProvinciaFechaMunicipo, DEFUNCIONES_STR, NULL, municipio)
  # colera_ts(df_colera.groupByProvinciaFechaMunicipo, DEFUNCIONES_STR, NULL, municipio)
  
}
