library(sf)
library(rgdal)
library(viridis)
library(tidyr)
library(gghighlight)
library(plotly)
library(zoo)
library(gganimate)
library(transformr)
library(gifski)

# remove.packages("gganimate")
# remove.packages("transformr")
# install.packages("https://cran.r-project.org/src/contrib/Archive/gganimate/gganimate_1.0.7.tar.gz", repos = NULL, type = "source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/transformr/transformr_0.1.3.tar.gz", repos = NULL, type = "source")

source("colera_data.R")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)

TOTAL_POBLACION_STR <- "Total_poblacion"
TOTAL_INVASIONES_STR <- paste("Total", INVASIONES_STR, sep = "_")
TOTAL_DEFUNCIONES_STR <- paste("Total", DEFUNCIONES_STR, sep = "_")
VALENCIA_STR <- "valencia"
ARAGON_STR <- "aragon"
MURCIA_STR <- "murcia"
MINPOS_SUBSTR <- 18
MAXPOS_SUBSTR <- 19


# functions ---------------------------------------------------------------


split_dfByCCAA <- function(df, provincias) {
  
  if (is.vector(provincias)) { # more than one county
    df_ByCCAA <- subset(df, Provincia %in% provincias)
  }
  else {
    df_ByCCAA <- subset(df, Provincia == provincias) # one county
  }
  
  return(df_ByCCAA)
  
}


agg_invasionesByCCAA <- function(df, nameCCAA) {
  
  # aggregate df
  df_colera_invasiones.agg <- aggregate(
    x = df$Total_invasiones,
    by = list(
      `Codigo Ine` = df$`Codigo Ine`, 
      Total_poblacion = df$Total_poblacion,
      Fecha = df$Fecha),
    FUN = sum
  )
  names(df_colera_invasiones.agg) <- c(CODIGO_INE_STR, TOTAL_POBLACION_STR, FECHA_STR, TOTAL_INVASIONES_STR)
  
  # order by "Codigo Ine" and "Fecha"  
  df_colera_invasiones.agg <- df_colera_invasiones.agg[order(df_colera_invasiones.agg$`Codigo Ine`, df_colera_invasiones.agg$Fecha),]
  # change date format ("1885-06-18") to month (6)
  df_colera_invasiones.agg$Fecha <- month(as.POSIXlt(df_colera_invasiones.agg$Fecha, format = DATE_FORMAT))
  # change the index numbers
  rownames(df_colera_invasiones.agg) <- 1:nrow(df_colera_invasiones.agg)
  
  # save aggregated df for Shiny SpatialEpiApp
  write.csv(
    df_colera_invasiones.agg,
    paste0(COLERA_DATA_DIR, "/webapp.colera_total_", INVASIONES_STR, "XCCAA.", nameCCAA, ".csv"),
    row.names = FALSE
  )
  
  return(df_colera_invasiones.agg)
  
}


agg_invasionesToMap <- function(df, map) {
  
  # reshape aggregated df as wide direction
  df_colera_invasiones.aggw <-
    reshape(
      df,
      timevar = FECHA_STR,
      idvar = CODIGO_INE_STR,
      direction = "wide"
    )
  
  print(df_colera_invasiones.aggw[1:2, ])
  print(map[1:2, ])
  
  # merge map with df_colera_invasiones.aggw
  map.merged <- merge(map, df_colera_invasiones.aggw, by.x = "CODIGOINE", by.y = CODIGO_INE_STR)
  print(map.merged[1:2, ])
  
  # convert map of type SpatialPolygonsDataFrame to of type sf
  mapsf <- st_as_sf(map.merged)
  
  return(mapsf)
  
}


plot_invasionesByCCAAxMonth <- function(mapsf, nameCCAA, invasiones_midpoint) {
  
  ggplot(mapsf) + geom_sf(aes(fill = Total_invasiones)) +
    facet_wrap(~Fecha, ncol = 6) +
    ggtitle(paste0(TOTAL_INVASIONES_STR, " ", nameCCAA, ", ", ANO_STR)) + theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_fill_gradient2(
      midpoint = invasiones_midpoint, low = "blue", mid = "white", high = "red"
    )
  
  # TODO: save plot as paste0(COLERA_PLOTS_DIR, "/colera_total_invasionesXCCAA_", nameCCAA, ".png")
  
}


gif_invasionesByCCAAxMonth <- function(mapsf, nameCCAA, invasiones_midpoint) {
  
  p <- ggplot(mapsf) + 
    geom_sf(aes(fill = Total_invasiones)) +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_fill_gradient2(
      midpoint = invasiones_midpoint,
      low = "blue",
      mid = "white",
      high = "red"
    ) +
    transition_time(Fecha) +
    labs(title = "Mes: {round(frame_time, 0)}")
  
  animate(p, render = gifski_renderer())
  anim_save(paste0(COLERA_PLOTS_DIR, "/colera_total_invasionesXCCAA_", nameCCAA, ".gif"))
  
}


timeplot_invasionesByCCAAxMonth <- function(df) {
  
  g <- ggplot(
    df,
    aes(
      x = Fecha,
      y = Total_invasiones,
      group = `Codigo Ine`,
      color = `Codigo Ine`
    )
  ) + geom_line() + geom_point(size = 2) + theme_bw()
  
  # g
  # g + theme(legend.position = "none")
  ggplotly(g)
  
  # return(g)
  
}


# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


Pob_Arago_PaisValencia_Murcia <- read_excel(paste(DATA_DIR, "Pob_Arago_PaisValencia_Murcia.xlsx", sep = "/"))
Pob_Arago_PaisValencia_Murcia1887 <- Pob_Arago_PaisValencia_Murcia[, c("Codi INE", "Municipi", "1887")]
Pob_Arago_PaisValencia_Murcia1887 <- na.omit(Pob_Arago_PaisValencia_Murcia1887) # remove NA
names(Pob_Arago_PaisValencia_Murcia1887) <- c(CODIGO_INE_STR, MUNICIPIO_STR, TOTAL_POBLACION_STR) # change column names

# group "invasiones" and "defunciones" by "Provincia", "Fecha" and "Codigo Ine"
df_colera_invasiones.groupByProvinciaFechaCodigoINE <- df_colera_invasiones %>%
  group_by(Provincia, `Codigo Ine`, Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) %>%
  na.omit(df_colera_invasiones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = DAY_STR),
    fill = list(Total_invasiones = 0)
  ) # add missing dates

df_colera_defunciones.groupByProvinciaFechaCodigoINE <- df_colera_defunciones %>%
  group_by(Provincia, `Codigo Ine`, Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = DAY_STR),
    fill = list(Total_defunciones = 0)
  ) # add missing dates

# merge grouped "invasiones" and "defunciones" as df_colera.groupByProvinciaFechaCodigoINE
df_colera.groupByProvinciaFechaCodigoINE <- merge(df_colera_invasiones.groupByProvinciaFechaCodigoINE, df_colera_defunciones.groupByProvinciaFechaCodigoINE)

# merge df_colera.groupByProvinciaFechaCodigoINE with Pob_Arago_PaisValencia_Murcia1887
df_colera.merged <- merge(df_colera.groupByProvinciaFechaCodigoINE, Pob_Arago_PaisValencia_Murcia1887, by = CODIGO_INE_STR)
df_colera.merged <-
  df_colera.merged[, c(
    CODIGO_INE_STR,
    MUNICIPIO_STR,
    PROVINCIA_STR,
    TOTAL_INVASIONES_STR,
    TOTAL_DEFUNCIONES_STR,
    TOTAL_POBLACION_STR,
    FECHA_STR
  )]

# unique(df_colera.merged$Provincia)

# split df_colera.merged for CCAA 
df_colera.split.CCAAvalencia <- split_dfByCCAA(df_colera.merged, c("castellon", VALENCIA_STR))
df_colera.split.CCAAaragon <- split_dfByCCAA(df_colera.merged, c("huesca", "teruel", "zaragoza"))
df_colera.split.CCAAmurcia <- split_dfByCCAA(df_colera.merged, MURCIA_STR)

head(df_colera.split.CCAAvalencia)
head(df_colera.split.CCAAaragon)
head(df_colera.split.CCAAmurcia)

codigosINE.CCAAvalencia <- unique(df_colera.split.CCAAvalencia$`Codigo Ine`)
codigosINE.CCAAaragon <- unique(df_colera.split.CCAAaragon$`Codigo Ine`)
codigosINE.CCAAmurcia <- unique(df_colera.split.CCAAmurcia$`Codigo Ine`)


# map ---------------------------------------------------------------------


map_municipios <- st_read(paste(SHAPES_DATA_DIR, "MUNICIPIOS_IGN.shp", sep = "/"), quiet = TRUE)
summary(map_municipios)

# split map_municipios of Spain for CCAA (only "codigosINE" presents in data) 
map_valencia <- map_municipios[map_municipios$CODIGOINE %in% codigosINE.CCAAvalencia,]
map_aragon <- map_municipios[map_municipios$CODIGOINE %in% codigosINE.CCAAaragon,]
map_murcia <- map_municipios[map_municipios$CODIGOINE %in% codigosINE.CCAAmurcia,]

map_valencia$CODIGOINE <- as.numeric(map_valencia$CODIGOINE)
map_aragon$CODIGOINE <- as.numeric(map_aragon$CODIGOINE)
map_murcia$CODIGOINE <- as.numeric(map_murcia$CODIGOINE)

plot(map_valencia)
plot(map_aragon)
plot(map_murcia)


# invasiones agg by CCAA --------------------------------------------------


df_colera_invasiones.CCAAvalencia.agg <- agg_invasionesByCCAA(df_colera.split.CCAAvalencia, VALENCIA_STR)
df_colera_invasiones.CCAAaragon.agg <- agg_invasionesByCCAA(df_colera.split.CCAAaragon, ARAGON_STR)
df_colera_invasiones.CCAAmurcia.agg <- agg_invasionesByCCAA(df_colera.split.CCAAmurcia, MURCIA_STR)

head(df_colera_invasiones.CCAAvalencia.agg)
head(df_colera_invasiones.CCAAaragon.agg)
head(df_colera_invasiones.CCAAmurcia.agg)


# adding invasiones agg to map --------------------------------------------


mapsf_valencia <- agg_invasionesToMap(df_colera_invasiones.CCAAvalencia.agg, map_valencia)
mapsf_aragon <- agg_invasionesToMap(df_colera_invasiones.CCAAaragon.agg, map_aragon)
mapsf_murcia <- agg_invasionesToMap(df_colera_invasiones.CCAAmurcia.agg, map_murcia)

mapsf_valencia <- gather(mapsf_valencia, Fecha, Total_invasiones, paste0(TOTAL_INVASIONES_STR, ".", 6:11))
mapsf_aragon <- gather(mapsf_aragon, Fecha, Total_invasiones, paste0(TOTAL_INVASIONES_STR, ".", 6:11))
mapsf_murcia <- gather(mapsf_murcia, Fecha, Total_invasiones, paste0(TOTAL_INVASIONES_STR, ".", 6:11))
# mapsf_murcia <-
#   gather(mapsf_murcia,
#          Fecha,
#          Total_invasiones,
#          paste0(TOTAL_INVASIONES_STR, ".", seq(as.Date("1885-06-18"), as.Date("1885-11-18"), by = 1)))

# TODO: try gather by yyyy-mm-dd


# invasiones agg plots ----------------------------------------------------


# keep integer month in each column like "Total_invasiones_6" in Fecha column
mapsf_valencia$Fecha <- as.integer(substring(mapsf_valencia$Fecha, MINPOS_SUBSTR, MAXPOS_SUBSTR))
mapsf_aragon$Fecha <- as.integer(substring(mapsf_aragon$Fecha, MINPOS_SUBSTR, MAXPOS_SUBSTR))
mapsf_murcia$Fecha <- as.integer(substring(mapsf_murcia$Fecha, MINPOS_SUBSTR, MAXPOS_SUBSTR))
# mapsf_murcia$Fecha <- as.Date(substring(mapsf_murcia$Fecha, 18, 30), format = DATE_FORMAT)

mapsf_valencia[1:2, ]
mapsf_aragon[1:2, ]
mapsf_murcia[1:2, ]

# plots by month
plot_invasionesByCCAAxMonth(mapsf_valencia, VALENCIA_STR, 100)
plot_invasionesByCCAAxMonth(mapsf_aragon, ARAGON_STR, 180)
plot_invasionesByCCAAxMonth(mapsf_murcia, MURCIA_STR, 100)
gif_invasionesByCCAAxMonth(mapsf_valencia, VALENCIA_STR, 100)
gif_invasionesByCCAAxMonth(mapsf_aragon, ARAGON_STR, 180)
gif_invasionesByCCAAxMonth(mapsf_murcia, MURCIA_STR, 100)

# timeplots by month
timeplot_invasionesByCCAAxMonth(df_colera_invasiones.CCAAvalencia.agg)
timeplot_invasionesByCCAAxMonth(df_colera_invasiones.CCAAaragon.agg)
timeplot_invasionesByCCAAxMonth(df_colera_invasiones.CCAAmurcia.agg)

# TODO: g + gghighlight(`Codigo Ine` == "30030")
