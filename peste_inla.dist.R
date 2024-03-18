library(dplyr)
library(INLA)
library(tmap)
library(sf)
library(spdep)
library(sf)
library(terra)
library(reshape2)
library(ggplot2)

load("peste_data.RData") # load peste data


# constants ---------------------------------------------------------------


MUNICIPIOS_SHAPEFILE <- "Municipios_IGN.shp"
# RAILWAYS_SHAPEFILE <- "Railways_1887.shp"
# RIVERS_SHAPEFILE <- "A3RIOS_proj.shp"

SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
PESTE_MAPS_DIR <- "peste_maps"
dir.create(PESTE_MAPS_DIR, showWarnings = FALSE)
PESTE_INLA_DIR <- "colera_inla"
dir.create(PESTE_INLA_DIR, showWarnings = FALSE)

COVPROV_STR <- "covdist_caprov"
# COVSTATION_STR <- "covdist_station"
# COVRAIL_STR <- "covdist_rail"
# COVRIVER_STR <- "covdist_river"
# COVWATER_STR <- "covdist_water"
# COVROAD_STR <- "covdist_road"
# COVCOAST_STR <- "covdist_coast"
# COVPORT_STR <- "covdist_port"
# COVALL_STR <- c(COVPROV_STR, COVSTATION_STR, COVRAIL_STR, COVRIVER_STR, COVWATER_STR, COVROAD_STR, COVCOAST_STR, COVPORT_STR)
LOCALIDADES_STR <- c("Artà", "Capdepera", "Sant Llorenç des Cardassar", "Son Servera")
CASOS_STR <- "Casos"
MONTHS_INT <- c(6, 7, 8, 9, 10)
MONTHS_STR <- c("June", "July", "August", "September", "October")
COEFFICIENTS <- c("mean", "0.025quant", "0.975quant")


# functions ---------------------------------------------------------------


add_distanceToCapital <- function (df_peste) {
    #' Add Distance to Capital Columns
    #'
    #' This function adds a "covdist_caprov" column to the input data frame.
    #'
    #' @param df_peste A data frame containing peste data.
    #'
    #' @return A data frame containing peste data with a "covdist_caprov" column.

    return(df_peste %>%
           mutate(
             covdist_caprov = case_when(
               Municipio == LOCALIDADES_STR[1] ~ 61.59, # Artà
               Municipio == LOCALIDADES_STR[2] ~ 68.58, # Capdepera
               Municipio == LOCALIDADES_STR[3] ~ 54.64, # Sant Llorenç des Cardassar
               Municipio == LOCALIDADES_STR[4] ~ 61.16  # Son Servera
             )
           ))
}


run_inla <- function(mapS, covariate, isPred = FALSE) {
  #' Run INLA
  #'
  #' This function runs INLA.
  #'
  #' @param mapS A data frame containing peste data.
  #' @param covariate A string containing the name of the covariate.
  #' @param isPred A boolean indicating whether the model is a prediction.
  #'
  #' @return A list containing the INLA model.

  # create adjacency matrix
  nb <- poly2nb(mapS)
  head(nb)

  # create adjacency graph
  nb2INLA("map.adj", nb)
  g <- inla.read.graph(filename = "map.adj")

  # create model formula
  mapS$idarea <- as.numeric(as.factor(mapS$NAMEUNIT))
  mapS$idarea1 <- mapS$idarea
  mapS$idtime <- 1 + mapS$mes - min(mapS$mes)
  covariates <- c(covariate, "f(idarea, model = 'bym', graph = g)", "f(idarea1, idtime, model = 'iid')", "idtime")
  formula <- as.formula(paste(paste(CASOS_STR, "~"), paste(covariates, collapse = " + ")))
  print(formula)

  # run INLA
  if (isPred) {
    res <- inla(formula, family = "poisson", data = mapS, control.predictor = list(link = 1), verbose = TRUE) # prediction
  }
  else {
    res <- inla(formula, family = "poisson", data = mapS, offset = log(Poblacion), control.predictor = list(compute = TRUE), control.compute = list(return.marginals = TRUE), verbose = TRUE) # relative risk
  }
  # res <- inla(formula, family = "poisson", data = mapS, offset = log(Poblacion), control.predictor = list(compute = TRUE), control.compute = list(return.marginals = TRUE), verbose = TRUE)
  print(res)
  return(res)
}


generate_maps <- function (mapS.peste, columnvar, outputvar) {
  #' Generate Maps
  #'
  #' This function generates maps.
  #'
  #' @param mapS.peste A data frame containing peste data.
  #' @param columnvar A string containing the name of the column variable.
  #' @param outputvar A string containing the name of the output variable.
  #'
  #' @return A list containing the maps.

  maps_list <- list() # list to store maps

  for (month in MONTHS_INT) {

    # create a map of observed cases by each month

    map_casos <- tm_shape(mapS.peste[mapS.peste$mes == month,], bbox = mapS.municipios.mallorca) +
        tm_polygons(
          col = columnvar,
          border.col = NULL,
          title = "",
          palette = "Reds",
          breaks = c(0, 10, 68, 450, 652, 3178),
          style = "fixed"
        ) +
        tm_shape(mapS.municipios.mallorca) + tm_borders() +
        tm_layout(
          legend.position =  c("right", "bottom"), legend.text.size = 1.5,
          inner.margins = c(0, 0, 0, 0),
          panel.labels = c(MONTHS_STR[month-5]),
          panel.label.size = 1.5, panel.label.height = 1.1, panel.label.color = "black", panel.label.bg.color = "gray"
        ) # +
        # tm_text("NAMEUNIT", size = 0.5, root = 2) # add municipality names

    tmap_save(map_casos, filename = paste(PESTE_MAPS_DIR, paste0("tmap.municipios.", CASOS_STR, ".", month, outputvar, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in") # save map
    maps_list[[MONTHS_STR[month-5]]] <- map_casos # add map to list
  }
  return(maps_list)
}


# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


# add "covdist_caprov" column
df_peste <- add_distanceToCapital(df_peste)
df_peste.defunciones <- add_distanceToCapital(df_peste.defunciones)

# replace NA values with 0
df_peste <- df_peste %>% mutate(Casos = replace(Casos, is.na(Casos), 0))
df_peste.defunciones <- df_peste.defunciones %>% mutate(Casos = replace(Casos, is.na(Casos), 0))

# group by month
df_peste <- df_peste %>% group_by(Municipio, mes, Poblacion, covdist_caprov) %>% summarise(Casos = sum(Casos))
df_peste.defunciones <- df_peste.defunciones %>% group_by(Municipio, mes, Poblacion, covdist_caprov) %>% summarise(Casos = sum(Casos))


# maps --------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, MUNICIPIOS_SHAPEFILE, sep = "/"), quiet = TRUE)
mapS.municipios.mallorca <- subset(mapS.municipios, CODNUT3 == "ES532") # only Mallorca municipalities
mapS.municipios.mallorca.localidades <- subset(mapS.municipios.mallorca, NAMEUNIT %in% LOCALIDADES_STR) # only Mallorca municipalities with black plague cases
mapS.municipios.mallorca.notlocalidades <- subset(mapS.municipios.mallorca, !(NAMEUNIT %in% LOCALIDADES_STR)) # only Mallorca municipalities without black plague cases

# mapS.railways <- st_read(paste(SHAPES_DATA_DIR, RAILWAYS_SHAPEFILE, sep = "/"), quiet = TRUE)
# mapS.railways <- na.omit(mapS.railways)
# mapS.rivers <- st_read(paste(SHAPES_DATA_DIR, RIVERS_SHAPEFILE, sep = "/"), quiet = TRUE)

# merge peste data with map data
mapS.peste_inla7 <- merge(mapS.municipios.mallorca.localidades, df_peste, by.x = "NAMEUNIT", by.y = "Municipio")
head(mapS.peste_inla7)


# observed cases ----------------------------------------------------------


# create a map of observed cases by each month
maps_list <- generate_maps(mapS.peste_inla7, CASOS_STR, "")
map_casos.multi <- tmap_arrange(maps_list, ncol = 2) # arrange maps
tmap_save(map_casos.multi, filename = paste(PESTE_MAPS_DIR, paste0("tmap.municipios.", CASOS_STR, ".multi.png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in") # save map


# modelling ---------------------------------------------------------------


model <- run_inla(mapS.peste_inla7, COVPROV_STR, FALSE) # fit the model
model$summary.fixed

# TODO: calculate relative risk by each covariate
# exp(model$summary.fixed[2, COEFFICIENTS[1]])
# exp(model$summary.fixed[2, COEFFICIENTS[2]])
# exp(model$summary.fixed[2, COEFFICIENTS[3]])

# marginal effects
model$marginals.fixed
model.fixed <- reshape2:::melt(model$marginals.fixed) %>% reshape2:::dcast(L1+Var1~Var2, value='value')
ggplot(model.fixed, aes(y=y, x=x)) + geom_line() + facet_wrap(~L1, scales='free', nrow=1) + theme_classic()

# save relative risk to map data
mapS.peste_inla7$RR <- model$summary.fitted.values[, COEFFICIENTS[1]]
mapS.peste_inla7$LL <- model$summary.fitted.values[, COEFFICIENTS[2]]
mapS.peste_inla7$UL <- model$summary.fitted.values[, COEFFICIENTS[3]]

# create a map of relative risk by each month
maps_list.RR <- generate_maps(mapS.peste_inla7, "RR", ".RR")
map_casos.multi.RR <- tmap_arrange(maps_list.RR, ncol = 2) # arrange maps
tmap_save(map_casos.multi.RR, filename = paste(PESTE_MAPS_DIR, paste0("tmap.municipios.", CASOS_STR, ".multi.RR.png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in") # save map


# prediction --------------------------------------------------------------


# create new data
newdata <- expand.grid(NAMEUNIT = unique(mapS.municipios.mallorca.notlocalidades$NAMEUNIT), mes = unique(df_peste$mes), Casos = NA)
colnames(newdata)[1] <- "Municipio"
newdata.pred <- rbind(df_peste, newdata)
newdata.pred <- newdata.pred[, c(1,2,5)]

# merge new peste data with map data
mapS.peste_inla7.pred <- merge(mapS.municipios.mallorca, newdata.pred, by.x = "NAMEUNIT", by.y = "Municipio")
head(mapS.peste_inla7.pred)

# fit the model to the new data
model.pred <- run_inla(mapS.peste_inla7.pred, "1", TRUE)

# examine the regular summary - not there are no changes to the first fit
summary(model.pred)

# save predicted values to map data
mapS.peste_inla7.pred$mean <- model.pred$summary.fitted.values[, COEFFICIENTS[1]]
mapS.peste_inla7.pred$lower <- model.pred$summary.fitted.values[, COEFFICIENTS[2]]
mapS.peste_inla7.pred$upper <- model.pred$summary.fitted.values[, COEFFICIENTS[3]]

# create a map of predicted cases by each month
maps_list.pred <- generate_maps(mapS.peste_inla7.pred, COEFFICIENTS[1], ".pred")
map_casos.multi.pred <- tmap_arrange(maps_list.pred, ncol = 2) # arrange maps
tmap_save(map_casos.multi.pred, filename = paste(PESTE_MAPS_DIR, paste0("tmap.municipios.", CASOS_STR, ".multi.pred.png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in") # save map

# plot(mapS.peste_inla7.pred$Casos, mapS.peste_inla7.pred$mean, 
#      xlab = "Casos", ylab = "mean")
# abline(0, 1, col = "red")
