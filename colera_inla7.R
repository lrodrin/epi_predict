# https://www.paulamoraga.com/book-geospatial/index.html
# https://inla.r-inla-download.org/R/stable/bin/windows/contrib/4.0/

# macOS install.packages("INLA_21.02.23.tar", repos = NULL, type = "source")


library(lwgeom)
library(sf)
library(viridis)
library(foreach)
library(INLA)
library(splancs)
library(raster)
library(reshape2)
library(doParallel)
library(SpatialEpi)
library(plotly)
library(gghighlight)


# load("~/epi_predict/colera_data.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)

TOTAL_POBLACION_STR <- "Total_poblacion"
TOTAL_INVASIONES_STR <- paste("Total", INVASIONES_STR, sep = "_")
TOTAL_DEFUNCIONES_STR <- paste("Total", DEFUNCIONES_STR, sep = "_")
VALENCIA_STR <- "valencia"
ARAGON_STR <- "aragon"
MURCIA_STR <- "murcia"
SIR_STR <- "SIR"
MINPOS_SUBSTR <- 5
MAXPOS_SUBSTR <- 6


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


invasiones_expectedCasesAndSIRs <- function(df) {
  
  Total_invasionesE <- expected(
    population = df$Total_poblacion,
    cases = df$Total_invasiones,
    n.strata = 1
  )
  
  # select unique "Codigo Ine"
  vecid <- unique(df$`Codigo Ine`)
  
  # calculate all consecutive dates between min and max
  vectime <- seq(as.Date(min(df$Fecha)), as.Date(max(df$Fecha)), by = 1)
  length(vectime)
  
  # create new data frame dfE with the expected counts for each "Codigo Ine" and "Fecha"
  dfE <- expand.grid(`Codigo Ine` = vecid, Fecha = vectime)
  dfE <- dfE[ with(dfE, order(`Codigo Ine`, Fecha)), ] # order by "Codigo Ine" and "Fecha"
  dfE$Total_poblacion <- df$Total_poblacion
  dfE$Total_invasiones <- df$Total_invasiones
  dfE$Total_invasionesE <- Total_invasionesE
  dfE$SIR <- dfE$Total_invasiones/dfE$Total_invasionesE
  head(dfE)
  
  return(dfE)
  
}


adding_dfToMap <- function(df, map) {
  
  # format column "Fecha" as POSIXlt
  df$Fecha <- month(as.POSIXlt(df$Fecha, format = DATA_FORMAT))
  
  # reshape df as wide direction
  dfw <-
    reshape(
      df,
      timevar = FECHA_STR,
      idvar = CODIGO_INE_STR,
      direction = "wide"
    )
  
  print(dfw[1:2, ])
  print(map[1:2, ])
  
  # merge map with dfw
  map.merged <- merge(map, dfw, by.x = "CODIGOINE", by.y = CODIGO_INE_STR)
  print(map.merged[1:2, ])
  
  # convert map of type SpatialPolygonsDataFrame to of type sf
  mapsf <- st_as_sf(map.merged)
  
  return(mapsf)
  
}


plot_SIRByCCAAxMonth <- function(mapsf, nameCCAA, invasiones_midpoint) {
  
  ggplot(mapsf) + geom_sf(aes(fill = SIR)) +
    facet_wrap(~Fecha, ncol = 6) +
    ggtitle(paste0(SIR_STR, " ", nameCCAA, ", ", ANO_STR)) + theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_fill_gradient2(
      midpoint = invasiones_midpoint, low = "blue", mid = "white", high = "red"
    )
  
  # TODO: save plot as paste0(COLERA_PLOTS_DIR, "/colera_total_SIRXCCAA_", nameCCAA, ".png")
  
}


timeplot_SIRByCCAAxMonth <- function(df) {
  
  g <- ggplot(
    df,
    aes(
      x = Fecha,
      y = SIR,
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
  )] # select columns "Codigo Ine", "Municipio", "Provincia", "Total_invasiones", "Total_defunciones", "Total_poblacion" and "Fecha" 

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


# calculation of expected cases and SIRs by CCAA --------------------------


df_colera_invasiones.CCAAvalencia <- invasiones_expectedCasesAndSIRs(df_colera_invasiones.CCAAvalencia.agg)
df_colera_invasiones.CCAAaragon <- invasiones_expectedCasesAndSIRs(df_colera_invasiones.CCAAaragon.agg)
df_colera_invasiones.CCAAmurcia <- invasiones_expectedCasesAndSIRs(df_colera_invasiones.CCAAmurcia.agg)


# adding to map -----------------------------------------------------------


mapsf_valencia <- adding_dfToMap(df_colera_invasiones.CCAAvalencia, map_valencia)
mapsf_aragon <- adding_dfToMap(df_colera_invasiones.CCAAaragon, map_aragon)
mapsf_murcia <- adding_dfToMap(df_colera_invasiones.CCAAmurcia, map_murcia)


# SIR plots ---------------------------------------------------------------


mapsf_valencia <- gather(mapsf_valencia, Fecha, SIR, paste0(SIR_STR, ".", 6:11))
mapsf_aragon <- gather(mapsf_aragon, Fecha, SIR, paste0(SIR_STR, ".", 6:11))
mapsf_murcia <- gather(mapsf_murcia, Fecha, SIR, paste0(SIR_STR, ".", 6:11))
# mapsf_murcia <-
#   gather(mapsf_murcia,
#          Fecha,
#          SIR,
#          paste0(SIR_STR, ".", seq(as.Date("1885-06-18"), as.Date("1885-11-18"), by = 1)))

# TODO: try gather by yyyy-mm-dd

# keep integer month in each column like "Total_SIR_6" in Fecha column
mapsf_valencia$Fecha <- as.integer(substring(mapsf_valencia$Fecha, MINPOS_SUBSTR, MAXPOS_SUBSTR))
mapsf_aragon$Fecha <- as.integer(substring(mapsf_aragon$Fecha, MINPOS_SUBSTR, MAXPOS_SUBSTR))
mapsf_murcia$Fecha <- as.integer(substring(mapsf_murcia$Fecha, MINPOS_SUBSTR, MAXPOS_SUBSTR))

# plots by month
plot_SIRByCCAAxMonth(mapsf_valencia, VALENCIA_STR, 30)
plot_SIRByCCAAxMonth(mapsf_aragon, ARAGON_STR, 51)
plot_SIRByCCAAxMonth(mapsf_murcia, MURCIA_STR, 17)

# timeplots by month
timeplot_SIRByCCAAxMonth(df_colera_invasiones.CCAAvalencia)
timeplot_SIRByCCAAxMonth(df_colera_invasiones.CCAAaragon)
timeplot_SIRByCCAAxMonth(df_colera_invasiones.CCAAmurcia)

# TODO: g + gghighlight(`Codigo Ine` == "30030")
