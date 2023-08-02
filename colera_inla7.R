# https://www.paulamoraga.com/book-geospatial/index.html
# https://inla.r-inla-download.org/R/stable/bin/

# install.packages("INLA_21.02.23.tar", repos = NULL, type = "source")
# install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)


library(lwgeom)
library(sf)
library(viridis)
library(foreach)
library(INLA)
library(spdep)
library(splancs)
library(raster)
library(reshape2)
library(doParallel)
library(SpatialEpi)
library(plotly)
library(gghighlight)
library(gganimate)
library(transformr)
library(gifski)

# remove.packages("gganimate")
# remove.packages("transformr")
# install.packages("https://cran.r-project.org/src/contrib/Archive/gganimate/gganimate_1.0.7.tar.gz", repos = NULL, type = "source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/transformr/transformr_0.1.3.tar.gz", repos = NULL, type = "source")


# load("~/epi_predict/colera_data.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)

TOTAL_POBLACION_STR <- "Total_poblacion"
TOTAL_INVASIONES_STR <- paste("Total", INVASIONES_STR, sep = "_")
TOTAL_DEFUNCIONES_STR <- paste("Total", DEFUNCIONES_STR, sep = "_")
VALENCIA_STR <- "valencia"
# ARAGON_STR <- "aragon"
ZARAGOZA_STR <- "zaragoza"
MURCIA_STR <- "murcia"
SIR_STR <- "SIR"
MINPOS_SUBSTR <- 5
MAXPOS_SUBSTR <- 6
MINPOS_GATHER <- 6
MAXPOS_GATHER <- 11


# functions ---------------------------------------------------------------


split_df <- function(df, provincias) {
  
  if (is.vector(provincias)) { # more than one county
    df <- subset(df, Provincia %in% provincias)
  }
  else {
    df <- subset(df, Provincia == provincias) # one county
  }
  
  return(df)
  
}


agg_invasiones <- function(df, name) {
  
  # aggregate df by "Codigo Ine", "Total_poblacion" and "Fecha"
  df_colera_invasiones.agg <- aggregate(
    x = df$Total_invasiones,
    by = list(
      `Codigo Ine` = df$`Codigo Ine`, 
      Total_poblacion = df$Total_poblacion,
      Fecha = df$Fecha),
    FUN = "sum"
  )
  names(df_colera_invasiones.agg) <- c(CODIGO_INE_STR, TOTAL_POBLACION_STR, FECHA_STR, TOTAL_INVASIONES_STR)
  
  # order by "Codigo Ine" and "Fecha"  
  df_colera_invasiones.agg <- df_colera_invasiones.agg[order(df_colera_invasiones.agg$`Codigo Ine`, df_colera_invasiones.agg$Fecha),]
  # change the index numbers
  rownames(df_colera_invasiones.agg) <- 1:nrow(df_colera_invasiones.agg)
  
  # save aggregated df for Shiny SpatialEpiApp
  write.csv(
    df_colera_invasiones.agg,
    paste0(COLERA_DATA_DIR, "/webapp.colera_total_", INVASIONES_STR, "X", name, ".csv"),
    row.names = FALSE
  )
  
  return(df_colera_invasiones.agg)
  
}


invasiones_expectedCasesAndSIRs <- function(df) {
  
  # calculate observed cases
  observados <-
    aggregate(
      x = df$Total_invasiones,
      by = list(time = df$Fecha, id = df$`Codigo Ine`),
      FUN = "sum"
    )[, "x"]
  
  # calculate population
  poblacion <-
    aggregate(
      x = df$Total_poblacion,
      by = list(time = df$Fecha, id = df$`Codigo Ine`),
      FUN = "sum"
    )[, "x"]
  
  # calculate expected cases
  esperados <- expected(
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
  rownames(dfE) <- 1:nrow(dfE) # change the index numbers
  dfE$Total_poblacion <- poblacion
  dfE$Total_invasiones <- observados
  dfE$Total_invasionesE <- esperados
  dfE$SIR <- dfE$Total_invasiones/dfE$Total_invasionesE
  print(head(dfE))
  
  return(dfE)
  
}


adding_dfToMap <- function(df, map) {
  
  # format column "Fecha" as POSIXlt
  df$Fecha <- month(as.POSIXlt(df$Fecha, format = DATE_FORMAT))
  
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


plot_SIRByMonth <- function(mapsf, name, invasiones_midpoint) {
  
  ggplot(mapsf) + geom_sf(aes(fill = SIR)) +
    facet_wrap(~Fecha, ncol = 6) +
    ggtitle(paste0(SIR_STR, " ", name, ", ", ANO_STR)) + theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_fill_gradient2(
      midpoint = invasiones_midpoint, low = "blue", mid = "orange", high = "red",
      breaks = c(0,30,60), labels = c("low", "mid", "high")
    )
  
  # TODO: save plot as paste0(COLERA_PLOTS_DIR, "/colera_total_SIRX_", name, ".png")
  
}


timeplot_SIRByMonth <- function(df) {
  
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


neighbourhood_matrix <- function(mapsf, county) {
  
  nb <- poly2nb(mapsf)
  print(head(nb))
  
  outputfilename <- paste0("map_", county, ".adj")
  nb2INLA(outputfilename, nb)
  g <- inla.read.graph(filename = outputfilename)
  
  return(g)
  
}


inference_inla <- function(df, g) {
  
  # create the index vectors for the municipalities and days
  df$idarea <- as.numeric(as.factor(df$`Codigo Ine`)) # vector with the indices of municipalities 
  df$idarea1 <- df$idarea # second index vector for municipalities (idarea1) by replicating idarea
  df$idtime <- 1 + df$Fecha - min(df$Fecha) # vector with the indices of days 
  
  # formula of the Bernardinelli model
  formula <- Total_invasiones ~ f(idarea, model = "bym", graph = g) +
    f(idarea1, idtime, model = "iid") + idtime
  
  cl <- makePSOCKcluster(4, setup_strategy = "sequential")
  registerDoParallel(cl)
  
  # inla() call
  res <- inla(formula,
              family = "poisson", data = df, E = Total_invasionesE,
              control.predictor = list(compute = TRUE), verbose = TRUE
  )
  
  stopCluster(cl)
  
  return(res)
  
}


map_RR <- function(df, inference, mapsf) {
  
  # add the relative risk estimates and the lower and upper limits of the 95% credible intervals to df
  df$RR <- inference$summary.fitted.values[, "mean"]
  df$LL <- inference$summary.fitted.values[, "0.025quant"]
  df$UL <- inference$summary.fitted.values[, "0.975quant"]
  print(head(df))
  
  # format column "Fecha" as POSIXlt
  df$Fecha <- month(as.POSIXlt(df$Fecha, format = DATE_FORMAT))
  print(head(df))
  
  # maps showing the relative risks and lower and upper limits of 95% credible intervals for each month
  mapsf.RR <- merge(
    mapsf, df,
    by.x = c("CODIGOINE", FECHA_STR),
    by.y = c(CODIGO_INE_STR, FECHA_STR)
  )
  
  return(mapsf.RR)
  
  
}


plot_RRByMonth <- function(mapsf, name, RR_midpoint) {
  
  ggplot(mapsf) + geom_sf(aes(fill = RR)) +
    facet_wrap( ~ Fecha, dir = "h", ncol = 6) +
    ggtitle(paste0("RR ", name, ", ", ANO_STR)) + theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_fill_gradient2(
      midpoint = RR_midpoint, low = "blue", mid = "orange", high = "red",
      breaks = c(0, 40, 80), labels = c("low", "mid", "high"), limits = c(0, 90)
    )
  
  # TODO: save plot as paste0(COLERA_PLOTS_DIR, "/colera_total_RRX_", name, ".png")
  
  
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
# df_colera.split.valencia <- split_df(df_colera.merged, c("castellon", VALENCIA_STR))
# df_colera.split.aragon <- split_df(df_colera.merged, c("huesca", "teruel", "zaragoza"))
# df_colera.split.murcia <- split_df(df_colera.merged, MURCIA_STR)

# split df_colera.merged for "Provincia" 
df_colera.split.valencia <- split_df(df_colera.merged, VALENCIA_STR)
df_colera.split.zaragoza <- split_df(df_colera.merged, ZARAGOZA_STR)
df_colera.split.murcia <- split_df(df_colera.merged, MURCIA_STR)

head(df_colera.split.valencia)
# head(df_colera.split.aragon)
head(df_colera.split.zaragoza)
head(df_colera.split.murcia)

codigosINE.valencia <- unique(df_colera.split.valencia$`Codigo Ine`)
# codigosINE.aragon <- unique(df_colera.split.aragon$`Codigo Ine`)
codigosINE.zaragoza <- unique(df_colera.split.zaragoza$`Codigo Ine`)
codigosINE.murcia <- unique(df_colera.split.murcia$`Codigo Ine`)

# TODO: move code to colera_data.R


# map ---------------------------------------------------------------------


map_municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
summary(map_municipios)

# split map_municipios of Spain for "Provincia" (only "codigosINE" presents in data) 
map_valencia <- map_municipios[map_municipios$CODIGOINE %in% codigosINE.valencia,]
# map_aragon <- map_municipios[map_municipios$CODIGOINE %in% codigosINE.aragon,]
map_zaragoza <- map_municipios[map_municipios$CODIGOINE %in% codigosINE.zaragoza,]
map_murcia <- map_municipios[map_municipios$CODIGOINE %in% codigosINE.murcia,]

map_valencia$CODIGOINE <- as.numeric(map_valencia$CODIGOINE)
# map_aragon$CODIGOINE <- as.numeric(map_aragon$CODIGOINE)
map_zaragoza$CODIGOINE <- as.numeric(map_zaragoza$CODIGOINE)
map_murcia$CODIGOINE <- as.numeric(map_murcia$CODIGOINE)

plot(map_valencia)
# plot(map_aragon)
plot(map_zaragoza)
plot(map_murcia)


# invasiones agg by "Provincia" -------------------------------------------


df_colera_invasiones.valencia.agg <- agg_invasiones(df_colera.split.valencia, VALENCIA_STR)
# df_colera_invasiones.aragon.agg <- agg_invasiones(df_colera.split.aragon, ARAGON_STR)
df_colera_invasiones.zaragoza.agg <- agg_invasiones(df_colera.split.zaragoza, ZARAGOZA_STR)
df_colera_invasiones.murcia.agg <- agg_invasiones(df_colera.split.murcia, MURCIA_STR)

head(df_colera_invasiones.valencia.agg)
# head(df_colera_invasiones.aragon.agg)
head(df_colera_invasiones.zaragoza.agg)
head(df_colera_invasiones.murcia.agg)


# calculation of expected cases and SIRs by "Provincia" -------------------


df_colera_invasiones.valencia <- invasiones_expectedCasesAndSIRs(df_colera_invasiones.valencia.agg)
# df_colera_invasiones.aragon <- invasiones_expectedCasesAndSIRs(df_colera_invasiones.aragon.agg)
df_colera_invasiones.zaragoza <- invasiones_expectedCasesAndSIRs(df_colera_invasiones.zaragoza.agg)
df_colera_invasiones.murcia <- invasiones_expectedCasesAndSIRs(df_colera_invasiones.murcia.agg)

head(df_colera_invasiones.valencia)
# head(df_colera_invasiones.aragon)
head(df_colera_invasiones.zaragoza)
head(df_colera_invasiones.murcia)


# adding to map -----------------------------------------------------------


mapsf_valencia <- adding_dfToMap(df_colera_invasiones.valencia, map_valencia)
# mapsf_aragon <- adding_dfToMap(df_colera_invasiones.aragon, map_aragon)
mapsf_zaragoza <- adding_dfToMap(df_colera_invasiones.zaragoza, map_zaragoza)
mapsf_murcia <- adding_dfToMap(df_colera_invasiones.murcia, map_murcia)


# SIR plots ---------------------------------------------------------------


mapsf_valencia <- gather(mapsf_valencia, Fecha, SIR, paste0(SIR_STR, ".", MINPOS_GATHER:MAXPOS_GATHER))
# mapsf_aragon <- gather(mapsf_aragon, Fecha, SIR, paste0(SIR_STR, ".", 6:11))
mapsf_zaragoza <- gather(mapsf_zaragoza, Fecha, SIR, paste0(SIR_STR, ".", MINPOS_GATHER:MAXPOS_GATHER))
mapsf_murcia <- gather(mapsf_murcia, Fecha, SIR, paste0(SIR_STR, ".", MINPOS_GATHER:MAXPOS_GATHER))
# mapsf_murcia <-
#   gather(mapsf_murcia,
#          Fecha,
#          SIR,
#          paste0(SIR_STR, ".", seq(as.Date("1885-06-18"), as.Date("1885-11-18"), by = 1)))

# TODO: try gather by yyyy-mm-dd

# keep integer month in each column like "Total_SIR_6" in Fecha column
mapsf_valencia$Fecha <- as.integer(substring(mapsf_valencia$Fecha, MINPOS_SUBSTR, MAXPOS_SUBSTR))
# mapsf_aragon$Fecha <- as.integer(substring(mapsf_aragon$Fecha, MINPOS_SUBSTR, MAXPOS_SUBSTR))
mapsf_zaragoza$Fecha <- as.integer(substring(mapsf_zaragoza$Fecha, MINPOS_SUBSTR, MAXPOS_SUBSTR))
mapsf_murcia$Fecha <- as.integer(substring(mapsf_murcia$Fecha, MINPOS_SUBSTR, MAXPOS_SUBSTR))

# plots by month
plot_SIRByMonth(mapsf_valencia, VALENCIA_STR, 30)
# plot_SIRByMonth(mapsf_aragon, ARAGON_STR, 51)
plot_SIRByMonth(mapsf_zaragoza, ZARAGOZA_STR, 48)
plot_SIRByMonth(mapsf_murcia, MURCIA_STR, 17)

# timeplots by month
timeplot_SIRByMonth(df_colera_invasiones.valencia)
# timeplot_SIRByMonth(df_colera_invasiones.aragon)
timeplot_SIRByMonth(df_colera_invasiones.zaragoza)
timeplot_SIRByMonth(df_colera_invasiones.murcia)

# TODO: g + gghighlight(`Codigo Ine` == "30030")


# model -------------------------------------------------------------------


# neighbourhood matrix
graph_valencia <- neighbourhood_matrix(mapsf_valencia, VALENCIA_STR)
#° graph_aragon <- neighbourhood_matrix(mapsf_aragon, ARAGON_STR)
graph_zaragoza <- neighbourhood_matrix(mapsf_zaragoza, ZARAGOZA_STR)
graph_murcia <- neighbourhood_matrix(mapsf_murcia, MURCIA_STR)

# inference using INLA
inference_valencia <- inference_inla(df_colera_invasiones.valencia, graph_valencia)
# inference_aragon <- inference_inla(df_colera_invasiones.aragon, graph_aragon)
inference_zaragoza <- inference_inla(df_colera_invasiones.zaragoza, graph_zaragoza)
inference_murcia <- inference_inla(df_colera_invasiones.murcia, graph_murcia)

# TODO: inference for CCAA

options(scipen=999)

# mapping relative risk
mapsf_valencia.RR <- map_RR(df_colera_invasiones.valencia, inference_valencia, mapsf_valencia)
mapsf_zaragoza.RR <- map_RR(df_colera_invasiones.zaragoza, inference_zaragoza, mapsf_zaragoza)
mapsf_murcia.RR <- map_RR(df_colera_invasiones.murcia, inference_murcia, mapsf_murcia)

# plot by month
plot_RRByMonth(mapsf_valencia.RR, VALENCIA_STR, 44)
plot_RRByMonth(mapsf_zaragoza.RR, ZARAGOZA_STR, 7)
plot_RRByMonth(mapsf_murcia.RR, MURCIA_STR, 23)
# gif_RRByMonth(mapsf_valencia.RR, VALENCIA_STR, 44)
# gif_RRByMonth(mapsf_zaragoza.RR, ZARAGOZA_STR, 7)
# gif_RRByMonth(mapsf_murcia.RR, MURCIA_STR, 23)

# gif_RRByMonth <- function(mapsf, name, RR_midpoint) {
#   
#   p <- ggplot(mapsf) +
#     geom_sf(aes(fill = RR)) +
#     theme_bw() +
#     theme(
#       axis.text.x = element_blank(),
#       axis.text.y = element_blank(),
#       axis.ticks = element_blank()
#     ) +
#     scale_fill_gradient2(
#       midpoint = RR_midpoint,
#       low = "blue",
#       mid = "white",
#       high = "red"
#     ) +
#     transition_time(Fecha) +
#     labs(title = "Mes: {round(frame_time, 0)}")
#   
#   animate(p, render = gifski_renderer())
#   anim_save(paste0(COLERA_PLOTS_DIR, "/colera_total_invasionesX_", name, ".RR.gif"))
#   
# }

options(scipen=0)

