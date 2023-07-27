source("colera_data.R")


# constants ---------------------------------------------------------------


colera_provincias <- unique(df_colera.groupByProvinciaFecha$Provincia)
colera_municipios <- as.factor(unique(df_colera.groupByProvinciaFechaMunicipo$Municipio))


# functions ---------------------------------------------------------------


colera_ts <- function(df_colera, cause, county, city=NULL) {
  
  if (is.null(city)) { # provincias
    df_colera.tmp <- subset(df_colera, Provincia == county)
    agg_level <- county
    outputfilename <-  paste0(COLERA_DATA_DIR, "/ts.colera_total_", cause, "Xprovincia_", agg_level, ".csv")
    plot_outputfilename <- paste0(COLERA_PLOTS_DIR, "/ts.colera_total_", cause, "Xprovincia_", agg_level, ".png")
    
  } else { # municipios
    df_colera.tmp <- subset(df_colera, Provincia == county | Municipio == city)
    agg_level <- city
    outputfilename <-  paste0(COLERA_DATA_DIR, "/ts.colera_total_", cause, "Xmunicipio_", agg_level, ".csv")
    plot_outputfilename <- paste0(COLERA_PLOTS_DIR, "/ts.colera_total_", cause, "Xmunicipio_", agg_level, ".png")
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
  
  ggsave(plot_outputfilename, width = 15.7, height = 4.5, dpi = 300, limitsize = TRUE)
  
  write.csv(df_colera.tmp, outputfilename, row.names = FALSE)
  
}


# main --------------------------------------------------------------------


# plots -------------------------------------------------------------------


for (provincia in colera_provincias) { # for each "Provincia"
  
  # invasiones by county
  colera_ts(df_colera.groupByProvinciaFecha, INVASIONES_STR, provincia, NULL)
  
  # defunciones by county
  colera_ts(df_colera.groupByProvinciaFecha, DEFUNCIONES_STR, provincia, NULL)
  
}


# for "Municipio" in the specified "Provincia"
  

# invasiones by city
colera_ts(df_colera.groupByProvinciaFechaMunicipo, INVASIONES_STR, "valencia", "valencia")

# defunciones by city
colera_ts(df_colera.groupByProvinciaFechaMunicipo, DEFUNCIONES_STR, "valencia", "valencia")
