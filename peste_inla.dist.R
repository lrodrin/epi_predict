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


# test --------------------------------------------------------------------


# mapS.municipios.mallorca.points <- st_centroid(mapS.municipios.mallorca)
mapS.peste_inla7.points <- st_centroid(mapS.peste_inla7)
mapS.peste_inla7.pred.points <- st_centroid(mapS.peste_inla7.pred)
  
# mapS.municipios.mallorca.points[, c("x", "y")] <- st_coordinates(mapS.municipios.mallorca.points)
mapS.peste_inla7.points[, c("x", "y")] <- st_coordinates(mapS.peste_inla7.points)  
mapS.peste_inla7.pred.points[, c("x", "y")] <- st_coordinates(mapS.peste_inla7.pred.points)  

d <- mapS.peste_inla7.points
dp <- mapS.peste_inla7.pred.points
map <- mapS.municipios.mallorca
ggplot(map) + geom_sf() + theme_bw()

map <- map %>% st_transform(25830)
d <- d %>% st_transform(25830)
dp <- dp %>% st_transform(25830)
ggplot(map) + geom_sf() + theme_bw() + coord_sf(datum = st_crs(map))

library(viridis)
ggplot() + geom_sf(data = map) +
  geom_sf(data = d, aes(col = Casos)) +
  scale_color_viridis()
ggplot() + geom_sf(data = map) +
  geom_sf(data = dp, aes(col = Casos)) +
  scale_color_viridis()
ggplot(d) +
  geom_histogram(mapping = aes(x = Casos)) +
  facet_wrap(~mes, ncol = 1) +
  theme_bw()
ggplot() + geom_sf(data = map) +
  geom_sf(data = d, aes(col = Casos)) +
  labs(x = "", y = "") +
  scale_color_viridis() +
  facet_wrap(~mes) +
  theme_bw()
ggplot(d, aes(x = mes, y = Casos, group = NAMEUNIT, color = NAMEUNIT)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(6, 7, 8, 9, 10)) +
  theme_bw() + theme(legend.position = "none")
  
# prediction data
grid <- terra::rast(map, nrows = 100, ncols = 100)
xy <- terra::xyFromCell(grid, 1:ncell(grid))
indicespointswithin <- which(st_intersects(dp, map, sparse = FALSE))
ggplot() + geom_sf(data = map) + geom_sf(data = dp)

# model
coo <- st_coordinates(dp)
bnd <- inla.nonconvex.hull(st_coordinates(map)[, 1:2])
mesh <- inla.mesh.2d(
  loc = coo, boundary = bnd,
  max.edge = c(100000, 200000), cutoff = 1000
)
mesh$n
plot(mesh)
points(coo, col = "red")

spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)
timesn <- length(unique(dp$mes))
indexs <- inla.spde.make.index("s",
  n.spde = spde$n.spde,
  n.group = timesn
)
lengths(indexs)
group <- dp$mes - min(dp$mes) + 1
A <- inla.spde.make.A(mesh = mesh, loc = coo, group = group)

plot(coo, asp = 1)
dpp <- coo
dpp <- rbind(cbind(dpp, 1), cbind(dpp, 2), cbind(dpp, 3), cbind(dpp, 4), cbind(dpp, 5))
head(dpp)

coop <- dpp[, 1:2]
groupp <- dpp[, 3]
Ap <- inla.spde.make.A(mesh = mesh, loc = coop, group = groupp)

# stack for estimation stk.e
stk.e <- inla.stack(
  tag = "est",
  data = list(y = dp$Casos),
  A = list(1, A),
  effects = list(data.frame(b0 = rep(1, nrow(A))), s = indexs)
)

# stack for prediction stk.p
stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA), 
  A = list(1, Ap),
  effects = list(data.frame(b0 = rep(1, nrow(Ap))), s = indexs)
)

# stk.full has stk.e and stk.p
stk.full <- inla.stack(stk.e, stk.p)

formula <- y ~ 0 + b0 + f(s, model = spde)
res <- inla(formula, family = "poisson",
            data = inla.stack.data(stk.full),
            control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full)),
            control.compute = list(return.marginals = TRUE), verbose = TRUE)

summary(res)

index <- inla.stack.index(stack = stk.full, tag = "pred")$data
dpp <- data.frame(dpp)
names(dpp) <- c("x", "y", "time")

dpp$pred_mean <- res$summary.fitted.values[index, "mean"]
dpp$pred_ll <- res$summary.fitted.values[index, "0.025quant"]
dpp$pred_ul <- res$summary.fitted.values[index, "0.975quant"]

library(reshape2)
dpm <- melt(dpp,
            id.vars = c("x", "y", "time"),
            measure.vars = c("pred_mean", "pred_ll", "pred_ul")
)
head(dpm)

ggplot(map) + geom_sf() + coord_sf(datum = NA) +
  geom_tile(data = dpm, aes(x = x, y = y, fill = value)) +
  labs(x = "", y = "") +
  facet_wrap(variable ~ time) +
  scale_fill_viridis("Casos") +
  theme_bw()
