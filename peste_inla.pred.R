library(dplyr)
library(readxl)
library(INLA)
library(tmap)
library(sf)
library(reshape2)
library(spdep)
library(ggplot2)
library(terra)

load("peste_data.RData") # load peste data


# constants ---------------------------------------------------------------


DISTANCES_FILE <- "_Results.xlsx"
MUNICIPIOS_SHAPEFILE <- "Municipios_IGN.shp"

DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
PESTE_MAPS_DIR <- "peste_maps"
dir.create(PESTE_MAPS_DIR, showWarnings = FALSE)

INVASIONES_STR <- "invasiones"
DEFUNCIONES_STR <- "defunciones"
MUNICIPIO_STR <- "Municipio"
NAMEUNIT_STR <- "NAMEUNIT"
LOCALIDADES_STR <- c("Artà", "Capdepera", "Sant Llorenç des Cardassar", "Son Servera")
CASOS_STR <- "Casos"
ESPG_CODE <- 25830 # EPSG code for UTM zone 30N (Spain)
MONTHS_INT <- c(6, 7, 8, 9, 10)
COEFFICIENTS <- c("mean", "0.025quant", "0.975quant")


# functions ---------------------------------------------------------------


generate_maps <- function (map, data, columnvar, outputvar, month=NULL) {
  #' Generate Maps
  #'
  #' This function generates maps.
  #'
  #' @param map A spatial object containing the municipalities data.
  #' @param data A data frame containing the peste data.
  #' @param columnvar A string containing the name of the column variable to map.
  #' @param outputvar A string containing the name of the output variable save the map.
  #' @param month An integer containing the month to map.

  map_tmp <- tm_shape(map) + tm_borders() +
    tm_shape(data) + tm_dots(size = 0.5, col = columnvar, palette = "Reds", title = "", style = "jenks") +
    tm_layout(
      legend.position = c("right", "bottom"), legend.text.size = 1.3,
      inner.margins = c(0, 0, 0, 0),
      panel.label.size = 1.3, panel.label.height = 1, panel.label.color = "black", panel.label.bg.color = "gray"
    )

  if (!is.null(month)) { # add month to map
    map_tmp <- map_tmp + tm_layout(panel.labels = c(c("June", "July", "August", "September", "October")[month-5]))
    month_name <- month
  } else { # add all months to map
    month_name <- ""
  }
  tmap_save(map_tmp, filename = paste(PESTE_MAPS_DIR, paste0("map.municipios.pred.", month_name, ".", outputvar, ".png"), sep = "/"), dpi = 300) # save map
}


# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


# replace NA values with 0
df_peste <- df_peste %>% mutate(Casos = replace(Casos, is.na(Casos), 0))
df_peste.defunciones <- df_peste.defunciones %>% mutate(Casos = replace(Casos, is.na(Casos), 0))

# group by month
df_peste <- df_peste %>% group_by(Municipio, mes, Poblacion) %>% summarise(Casos = sum(Casos))
df_peste.defunciones <- df_peste.defunciones %>% group_by(Municipio, mes, Poblacion) %>% summarise(Casos = sum(Casos))

# read distances data and replace "Sant Llorenç des Cardass" with "Sant Llorenç des Cardassar"
df_distances <- read_excel(paste(DATA_DIR, DISTANCES_FILE, sep = "/"), sheet = "Sheet5")
df_distances <- df_distances %>%
  mutate(
    MUNICIPIO = case_when(
      MUNICIPIO == "Sant Llorenç des Cardass" ~ LOCALIDADES_STR[3],
      TRUE ~ MUNICIPIO
    )
  )

df_distances <- df_distances[, !(names(df_distances) %in% c("OD_Id", "CD_INE5", "ALTURA", "POP_1877"))] # remove unnecessary columns
colnames(df_distances)[3:5] <- c(MUNICIPIO_STR, "covdist_road", "covdist_port") # rename columns to match peste data

# merge peste data with distances data
df_peste <- merge(df_peste, df_distances, by = MUNICIPIO_STR)
df_peste.defunciones <- merge(df_peste.defunciones, df_distances, by = MUNICIPIO_STR)


# maps --------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, MUNICIPIOS_SHAPEFILE, sep = "/"), quiet = TRUE)
mapS.municipios.mallorca <- subset(mapS.municipios, CODNUT3 == "ES532") # only Mallorca municipalities
mapS.municipios.mallorca.localidades <- subset(mapS.municipios.mallorca, NAMEUNIT %in% LOCALIDADES_STR) # only Mallorca municipalities with black plague cases
mapS.municipios.mallorca.notlocalidades <- subset(mapS.municipios.mallorca, !(NAMEUNIT %in% LOCALIDADES_STR)) # only Mallorca municipalities without black plague cases

# merge peste data with map data of Mallorca municipalities with black plague cases
mapS.peste_inla7 <- merge(mapS.municipios.mallorca.localidades, df_peste, by.x = NAMEUNIT_STR, by.y = MUNICIPIO_STR)
mapS.peste_inla7.defunciones <- merge(mapS.municipios.mallorca.localidades, df_peste.defunciones, by.x = NAMEUNIT_STR, by.y = MUNICIPIO_STR)
head(mapS.peste_inla7)
head(mapS.peste_inla7.defunciones)

rm(mapS.municipios)


# create prediction data --------------------------------------------------


# create new data for prediction with municipalities without black plague cases or deaths as NA
newdata <- expand.grid(Municipio = unique(mapS.municipios.mallorca.notlocalidades$NAMEUNIT), mes = unique(df_peste$mes), Casos = NA)
newdata.invasiones <- rbind(df_peste[, c(1:2,4)], newdata)
newdata.defunciones <- rbind(df_peste.defunciones[, c(1:2,4)], newdata)

# merge new data with distances data (covdist_road and covdist_port)
newdata.invasiones <- merge(newdata.invasiones, df_distances[, c(3:5)], by = MUNICIPIO_STR)
newdata.defunciones <- merge(newdata.defunciones, df_distances[, c(3:5)], by = MUNICIPIO_STR)

# merge new data of cases and deaths to map data of Mallorca
mapS.peste_inla7.invasiones.pred <- merge(mapS.municipios.mallorca, newdata.invasiones, by.x = NAMEUNIT_STR, by.y = MUNICIPIO_STR)
mapS.peste_inla7.defunciones.pred <- merge(mapS.municipios.mallorca, newdata.defunciones, by.x = NAMEUNIT_STR, by.y = MUNICIPIO_STR)
head(mapS.peste_inla7.invasiones.pred)
head(mapS.peste_inla7.defunciones.pred)

rm(newdata, newdata.invasiones, newdata.defunciones, df_distances)


# observed and predicted data ---------------------------------------------


# convert multipolygon to point the observed and preddicted data for cases and deaths
mapS.peste_inla7.invasiones.points <- st_centroid(mapS.peste_inla7)
mapS.peste_inla7.defunciones.points <- st_centroid(mapS.peste_inla7.defunciones)
mapS.peste_inla7.invasiones.pred.points <- st_centroid(mapS.peste_inla7.invasiones.pred)
mapS.peste_inla7.defunciones.pred.points <- st_centroid(mapS.peste_inla7.defunciones.pred)

# add "Long" and "Lat" coordinates to predicted data for cases and deaths
mapS.peste_inla7.invasiones.pred.points[, c("Long", "Lat")] <- st_coordinates(mapS.peste_inla7.invasiones.pred.points)
mapS.peste_inla7.defunciones.pred.points[, c("Long", "Lat")] <- st_coordinates(mapS.peste_inla7.defunciones.pred.points)

# assign the observed and predicted data for cases and deaths as variables for the model
d <- mapS.peste_inla7.invasiones.points
d.defunciones <- mapS.peste_inla7.defunciones.points
dp <- mapS.peste_inla7.invasiones.pred.points
dp.defunciones <- mapS.peste_inla7.defunciones.pred.points
map <- mapS.municipios.mallorca

# transform the observed and predicted data for cases and deaths to the same coordinate system
d <- d %>% st_transform(ESPG_CODE)
d.defunciones <- d.defunciones %>% st_transform(ESPG_CODE)
dp <- dp %>% st_transform(ESPG_CODE)
dp.defunciones <- dp.defunciones %>% st_transform(ESPG_CODE)
map <- map %>% st_transform(ESPG_CODE)

# generate maps of observed cases and deaths by each month for each municipality
for (month in MONTHS_INT) {
  generate_maps(map, d[d$mes == month,], CASOS_STR, INVASIONES_STR, month)
  generate_maps(map, d.defunciones[d.defunciones$mes == month,], CASOS_STR, DEFUNCIONES_STR, month)
}

# group by municipality
d.grouped <- d %>% group_by(NAMEUNIT) %>% summarise(Casos = sum(Casos))
d.defunciones.grouped <- d.defunciones %>% group_by(NAMEUNIT) %>% summarise(Casos = sum(Casos))

# generate maps of observed cases and deaths by all months for each municipality
generate_maps(map, d.grouped, CASOS_STR, INVASIONES_STR)
generate_maps(map, d.defunciones.grouped, CASOS_STR, DEFUNCIONES_STR)

rm(d.grouped, d.defunciones.grouped)


# modelling ---------------------------------------------------------------


grid <- terra::rast(map, nrows = 100, ncols = 100) # raster grid covering the map
xy <- terra::xyFromCell(grid, 1:ncell(grid)) # coordinates of all cells

# indices points within the map for cases and deaths
indicespointswithin.invasiones <- which(st_intersects(dp, map, sparse = FALSE))
indicespointswithin.defunciones <- which(st_intersects(dp.defunciones, map, sparse = FALSE))

# coordinates of prediction locations 
coo.invasiones <- st_coordinates(dp)
coo.defunciones <- st_coordinates(dp.defunciones)

# mesh construction
bnd <- inla.nonconvex.hull(st_coordinates(map)[, 1:2])
mesh.invasiones <- inla.mesh.2d(
  loc = coo.invasiones, boundary = bnd,
  max.edge = c(100000, 200000), cutoff = 1000
)
mesh.defunciones <- inla.mesh.2d(
  loc = coo.defunciones, boundary = bnd,
  max.edge = c(100000, 200000), cutoff = 1000
)
mesh.invasiones$n
mesh.defunciones$n

# plot mesh (constrained refined Delaunay triangulation) 
plot(mesh.invasiones)
points(coo.invasiones, col = "red")
dev.copy(png, paste(PESTE_MAPS_DIR, paste0("mesh_", INVASIONES_STR, ".png"), sep = "/"))
dev.off()

plot(mesh.defunciones)
points(coo.defunciones, col = "red")
dev.copy(png, paste(PESTE_MAPS_DIR, paste0("mesh_", DEFUNCIONES_STR, ".png"),  sep = "/"))
dev.off()

# building the SPDE model on the mesh for cases and deaths
spde.invasiones <- inla.spde2.matern(mesh = mesh.invasiones, alpha = 2, constr = TRUE)
spde.defunciones <- inla.spde2.matern(mesh = mesh.defunciones, alpha = 2, constr = TRUE)

# index set
timesn.invasiones <- length(unique(dp$mes))
timesn.defunciones <- length(unique(dp.defunciones$mes))
indexs.invasiones <- inla.spde.make.index("s", n.spde = spde.invasiones$n.spde, n.group = timesn.invasiones)
indexs.defunciones <- inla.spde.make.index("s", n.spde = spde.defunciones$n.spde, n.group = timesn.defunciones)
lengths(indexs.invasiones)
lengths(indexs.defunciones)

# projection matrix
group.invasiones <- dp$mes - min(dp$mes) + 1
group.defunciones <- dp.defunciones$mes - min(dp.defunciones$mes) + 1
A.invasiones <- inla.spde.make.A(mesh = mesh.invasiones, loc = coo.invasiones, group = group.invasiones)
A.defunciones <- inla.spde.make.A(mesh = mesh.defunciones, loc = coo.defunciones, group = group.defunciones)

# construct data locations and times to make predictions (5 months to predict)
dpp.invasiones <- coo.invasiones
dpp.defunciones <- coo.defunciones
dpp.invasiones <- rbind(cbind(dpp.invasiones, 1), cbind(dpp.invasiones, 2), cbind(dpp.invasiones, 3), cbind(dpp.invasiones, 4), cbind(dpp.invasiones, 5))
dpp.defunciones <- rbind(cbind(dpp.defunciones, 1), cbind(dpp.defunciones, 2), cbind(dpp.defunciones, 3), cbind(dpp.defunciones, 4), cbind(dpp.defunciones, 5))
head(dpp.invasiones)
head(dpp.defunciones)

# construct the matrix Ap that projects the spatially indexed random effects to the prediction locations
coop.invasiones <- dpp.invasiones[, 1:2]
coop.defunciones <- dpp.defunciones[, 1:2]
groupp.invasiones <- dpp.invasiones[, 3]
groupp.defunciones <- dpp.defunciones[, 3]
Ap.invasiones <- inla.spde.make.A(mesh = mesh.invasiones, loc = coop.invasiones, group = groupp.invasiones)
Ap.defunciones <- inla.spde.make.A(mesh = mesh.defunciones, loc = coop.defunciones, group = groupp.defunciones)

# stack with data for estimation and prediction (with covdist_road and covdist_port)
stk.e.invasiones <- inla.stack(
  tag = "est",
  data = list(y = dp$Casos),
  A = list(1, A.invasiones),
  effects = list(data.frame(b0 = rep(1, nrow(A.invasiones)), covdist_road = dp$covdist_road, covdist_port = dp$covdist_port), s = indexs.invasiones)
)

stk.e.defunciones <- inla.stack(
  tag = "est",
  data = list(y = dp.defunciones$Casos),
  A = list(1, A.defunciones),
  effects = list(data.frame(b0 = rep(1, nrow(A.defunciones)), covdist_road = dp.defunciones$covdist_road, covdist_port = dp.defunciones$covdist_port), s = indexs.defunciones)
)

stk.p.invasiones <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap.invasiones),
  effects = list(data.frame(b0 = rep(1, nrow(Ap.invasiones)), covdist_road = dp$covdist_road, covdist_port = dp$covdist_port), s = indexs.invasiones)
)

stk.p.defunciones <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap.defunciones),
  effects = list(data.frame(b0 = rep(1, nrow(Ap.defunciones)), covdist_road = dp.defunciones$covdist_road, covdist_port = dp.defunciones$covdist_port), s = indexs.defunciones)
)

stk.full.invasiones <- inla.stack(stk.e.invasiones, stk.p.invasiones)
stk.full.defunciones <- inla.stack(stk.e.defunciones, stk.p.defunciones)

# model formula for cases and deaths
formula.invasiones <- y ~ 0 + b0 + covdist_road + covdist_port + f(s, model = spde.invasiones)
formula.defunciones <- y ~ 0 + b0 + covdist_road + covdist_port + f(s, model = spde.defunciones)

# inla() call
res.invasiones <- inla(formula.invasiones, family = "poisson", data = inla.stack.data(stk.full.invasiones), control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.invasiones)),
            control.compute = list(return.marginals = TRUE), verbose = TRUE)
res.defunciones <- inla(formula.defunciones, family = "poisson", data = inla.stack.data(stk.full.defunciones), control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full.defunciones)),
            control.compute = list(return.marginals = TRUE), verbose = TRUE)

summary(res.invasiones)
summary(res.defunciones)


# results ---------------------------------------------------------------


# mapping predicted data for cases and deaths
index.invasiones <- inla.stack.index(stack = stk.full.invasiones, tag = "pred")$data
index.defunciones <- inla.stack.index(stack = stk.full.defunciones, tag = "pred")$data
dpp.invasiones <- data.frame(dpp.invasiones)
dpp.defunciones <- data.frame(dpp.defunciones)
columnames <- c("x", "y", "time")
names(dpp.invasiones) <- columnames
names(dpp.defunciones) <- columnames

dpp.invasiones$pred_mean <- res.invasiones$summary.fitted.values[index.invasiones, COEFFICIENTS[1]]
dpp.invasiones$pred_ll <- res.invasiones$summary.fitted.values[index.invasiones, COEFFICIENTS[2]]
dpp.invasiones$pred_ul <- res.invasiones$summary.fitted.values[index.invasiones, COEFFICIENTS[3]]
dpp.defunciones$pred_mean <- res.defunciones$summary.fitted.values[index.defunciones, COEFFICIENTS[1]]
dpp.defunciones$pred_ll <- res.defunciones$summary.fitted.values[index.defunciones, COEFFICIENTS[2]]
dpp.defunciones$pred_ul <- res.defunciones$summary.fitted.values[index.defunciones, COEFFICIENTS[3]]

# make the maps for cases and deaths by month
columnames_coefs <- c("pred_mean", "pred_ll", "pred_ul")
dpm.invasiones <- melt(dpp.invasiones, id.vars = columnames, measure.vars = columnames_coefs)
dpm.defunciones <- melt(dpp.defunciones, id.vars = columnames, measure.vars = columnames_coefs)
head(dpm.invasiones)
head(dpm.defunciones)

# convert data to sf and assign time values from 1 to 5 to 6 to 10
dpm.invasiones <- st_as_sf(dpm.invasiones, coords = c("x", "y"), crs = st_crs(map))
dpm.defunciones <- st_as_sf(dpm.defunciones, coords = c("x", "y"), crs = st_crs(map))
dpm.invasiones$time <- c(6, 7, 8, 9, 10)[match(dpm.invasiones$time, c(1, 2, 3, 4, 5))]
dpm.defunciones$time <- c(6, 7, 8, 9, 10)[match(dpm.defunciones$time, c(1, 2, 3, 4, 5))]

# generate maps of predicted cases and deaths by each month for each municipality
for (month in MONTHS_INT) {
  generate_maps(map, subset(dpm.invasiones, variable == columnames_coefs[1] & time == month), "value", paste0(INVASIONES_STR, ".results"), month)
  generate_maps(map, subset(dpm.defunciones, variable == columnames_coefs[1] & time == month), "value", paste0(DEFUNCIONES_STR, ".results"), month)
}

save.image("peste_inla.pred.RData") # save workspace

rm(list = ls()) # remove all objects from workspace
