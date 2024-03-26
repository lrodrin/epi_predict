library(dplyr)
library(readxl)
library(INLA)
library(tmap)
library(sf)
library(reshape2)
library(spdep)
library(ggplot2)
library(terra)
library(viridis)

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
CASES_STR <- "Cases"
DEATHS_STR <- "Deaths"
ESPG_CODE <- 25830 # EPSG code for UTM zone 30N (Spain)
MONTHS_INT <- c(6, 7, 8, 9, 10)
MONTHS_STR <- c("June", "July", "August", "September", "October")
COEFFICIENTS <- c("mean", "0.025quant", "0.975quant")


# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


# replace NA values with 0
df_peste <- df_peste %>% mutate(Casos = replace(Casos, is.na(Casos), 0))
df_peste.defunciones <- df_peste.defunciones %>% mutate(Casos = replace(Casos, is.na(Casos), 0))

# group by municipality, month and population and sum cases or deaths
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

df_distances <- df_distances[, !(names(df_distances) %in% c("OD_Id", "CD_INE5", "ALTURA", "POP_1877", "Dist_Roads1861", "Dist_Port"))] # remove unnecessary columns
colnames(df_distances)[3] <- MUNICIPIO_STR # rename column

# merge peste data with distances data
df_peste <- merge(df_peste, df_distances, by = MUNICIPIO_STR)
df_peste.defunciones <- merge(df_peste.defunciones, df_distances, by = MUNICIPIO_STR)

rm(df_distances)


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

# merge new data of cases and deaths to map data of Mallorca
mapS.peste_inla7.pred <- merge(mapS.municipios.mallorca, newdata.invasiones, by.x = NAMEUNIT_STR, by.y = MUNICIPIO_STR)
mapS.peste_inla7.defunciones.pred <- merge(mapS.municipios.mallorca, newdata.defunciones, by.x = NAMEUNIT_STR, by.y = MUNICIPIO_STR)
head(mapS.peste_inla7.pred)
head(mapS.peste_inla7.defunciones.pred)

rm(newdata, newdata.invasiones, newdata.defunciones)


# observed and predicted data ---------------------------------------------


# convert multipolygon to point the observed and preddicted data for cases and deaths
mapS.peste_inla7.points <- st_centroid(mapS.peste_inla7)
mapS.peste_inla7.points.defunciones <- st_centroid(mapS.peste_inla7.defunciones)
mapS.peste_inla7.pred.points <- st_centroid(mapS.peste_inla7.pred)
mapS.peste_inla7.defunciones.pred.points <- st_centroid(mapS.peste_inla7.defunciones.pred)

# add "Long" and "Lat" coordinates to predicted data for cases and deaths
mapS.peste_inla7.pred.points[, c("Long", "Lat")] <- st_coordinates(mapS.peste_inla7.pred.points)
mapS.peste_inla7.defunciones.pred.points[, c("Long", "Lat")] <- st_coordinates(mapS.peste_inla7.defunciones.pred.points)

# assign the observed and predicted data for cases and deaths as variables for the model
d <- mapS.peste_inla7.points
d.defunciones <- mapS.peste_inla7.points.defunciones
dp <- mapS.peste_inla7.pred.points
dp.defunciones <- mapS.peste_inla7.defunciones.pred.points
map <- subset(mapS.municipios.mallorca, NAMEUNIT != "Palma")

# transform the observed and predicted data for cases and deaths to the same coordinate system
d <- d %>% st_transform(ESPG_CODE)
d.defunciones <- d.defunciones %>% st_transform(ESPG_CODE)
dp <- dp %>% st_transform(ESPG_CODE)
dp.defunciones <- dp.defunciones %>% st_transform(ESPG_CODE)
map <- map %>% st_transform(ESPG_CODE)

# plot the observed data for cases and deaths

# by month

for (month in MONTHS_INT) {

  map_invasiones <- tm_shape(map) + tm_borders() +
    tm_shape(d[d$mes == month,]) + tm_dots(size = 0.5, col = CASOS_STR, palette = "Reds", title = "", style = "jenks") +
    tm_legend(title = CASES_STR) + tm_layout(panel.labels = c(MONTHS_STR[month-5])) 

  map_defunciones <- tm_shape(map) + tm_borders() +
    tm_shape(d.defunciones[d.defunciones$mes == month,]) + tm_dots(size = 0.5, col = CASOS_STR, palette = "Reds", title = "", style = "jenks") +
    tm_legend(title = DEATHS_STR) + tm_layout(panel.labels = c(MONTHS_STR[month-5])) 

  tmap_save(map_invasiones, filename = paste(PESTE_MAPS_DIR, paste0("map.municipios.pred.", month, ".", INVASIONES_STR, ".png"), sep = "/"), dpi = 300)
  tmap_save(map_defunciones, filename = paste(PESTE_MAPS_DIR, paste0("map.municipios.pred.", month, ".", DEFUNCIONES_STR, ".png"), sep = "/"), dpi = 300)
}

# all months

# group by municipality
d.grouped <- d %>% group_by(NAMEUNIT) %>% summarise(Casos = sum(Casos))
d.defunciones.grouped <- d.defunciones %>% group_by(NAMEUNIT) %>% summarise(Casos = sum(Casos))

map_all_invasiones <- tm_shape(map) + tm_borders() +
    tm_shape(d.grouped) + tm_dots(size = 0.5, col = CASOS_STR, palette = "Reds", title = "", style = "jenks") +
    tm_legend(title = CASES_STR) 

map_all_defunciones <- tm_shape(map) + tm_borders() +
  tm_shape(d.defunciones.grouped) + tm_dots(size = 0.5, col = CASOS_STR, palette = "Reds", title = "", style = "jenks") +
  tm_legend(title = DEATHS_STR) 

tmap_save(map_all_invasiones, filename = paste(PESTE_MAPS_DIR, paste0("map_all.municipios.pred.", INVASIONES_STR, ".png"), sep = "/"), dpi = 300)
tmap_save(map_all_defunciones, filename = paste(PESTE_MAPS_DIR, paste0("map_all.municipios.pred.", DEFUNCIONES_STR, ".png"), sep = "/"), dpi = 300)


# clean environment -------------------------------------------------------


rm(d.grouped, d.defunciones.grouped, map_invasiones, map_defunciones, map_all_invasiones, map_all_defunciones)


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
dev.copy(png, paste(PESTE_MAPS_DIR, "mesh_", INVASIONES_STR, ".png", sep = "/"))
dev.off()

plot(mesh.defunciones)
points(coo.defunciones, col = "red")
dev.copy(png, paste(PESTE_MAPS_DIR, "mesh_", DEFUNCIONES_STR, ".png",  sep = "/"))
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

# contruct data locations and times to make predictions (5 months to predict)
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

# stack with data for estimation and prediction
stk.e.invasiones <- inla.stack(
  tag = "est",
  data = list(y = dp$Casos),
  A = list(1, A.invasiones),
  effects = list(data.frame(b0 = rep(1, nrow(A.invasiones))), s = indexs.invasiones)
)

stk.e.defunciones <- inla.stack(
  tag = "est",
  data = list(y = dp.defunciones$Casos),
  A = list(1, A.defunciones),
  effects = list(data.frame(b0 = rep(1, nrow(A.defunciones))), s = indexs.defunciones)
)

stk.p.invasiones <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap.invasiones),
  effects = list(data.frame(b0 = rep(1, nrow(Ap.invasiones))), s = indexs.invasiones)
)

stk.p.defunciones <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap.defunciones),
  effects = list(data.frame(b0 = rep(1, nrow(Ap.defunciones))), s = indexs.defunciones)
)

stk.full.invasiones <- inla.stack(stk.e.invasiones, stk.p.invasiones)
stk.full.defunciones <- inla.stack(stk.e.defunciones, stk.p.defunciones)

# model formula for cases and deaths
formula.invasiones <- y ~ 0 + b0 + f(s, model = spde.invasiones)
formula.defunciones <- y ~ 0 + b0 + f(s, model = spde.defunciones)

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

# by month

for (month in MONTHS_INT) {

  map_list.invasiones <- lapply(unique(dpm.invasiones$variable), function(var) {
    tm_shape(map) + tm_borders() +
      tm_shape(subset(dpm.invasiones, variable == var & time == month)) + tm_dots(size = 0.3, col = "value", palette = "Reds", title = "", style = "jenks") +
      tm_legend(title = var) 
  })

  map_list.defunciones <- lapply(unique(dpm.defunciones$variable), function(var) {
    tm_shape(map) + tm_borders() +
      tm_shape(subset(dpm.defunciones, variable == var & time == month)) + tm_dots(size = 0.3, col = "value", palette = "Reds", title = "", style = "jenks") +
      tm_legend(title = var)
  })

  map_invasiones <- tmap_arrange(map_list.invasiones, ncol = 3)
  map_defunciones <- tmap_arrange(map_list.defunciones, ncol = 3)

  tmap_save(map_invasiones, filename = paste(PESTE_MAPS_DIR, paste0("map.municipios.pred.", month, ".", INVASIONES_STR, ".results.png"), sep = "/"), width = 10, height = 5, dpi = 300)
  tmap_save(map_defunciones, filename = paste(PESTE_MAPS_DIR, paste0("map.municipios.pred.", month, ".", DEFUNCIONES_STR, ".results.png"), sep = "/"), width = 10, height = 5, dpi = 300)
}


# clean environment -------------------------------------------------------


rm(df_peste, df_peste.defunciones, map_invasiones, map_defunciones, map_list.invasiones, map_list.defunciones)


save.image("peste_inla.pred.RData")
