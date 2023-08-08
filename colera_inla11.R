library(readxl)
library(ggplot2)
library(sf)
library(dplyr)
library(raster)
library(terra)
library(patchwork)
library(viridis)
library(doParallel)
library(INLA)
library(rasterVis)


# load("~/epi_predict/colera_data.RData")


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)

LONG_STR <- "long"
LONGITUDE_STR <- "longitude"
LAT_STR <- "lat"
LATITUDE_STR <- "latitude"
X_STR <- "x"
Y_STR <- "y"


# data --------------------------------------------------------------------


options(scipen=999)

df_railwaylines <- read_excel(paste(DATA_DIR, "Output.xlsx", sep = "/"))


# format df_railwaylines

df_railwaylines$Orig_Cod_INE <- as.character(df_railwaylines$Orig_Cod_INE)
df_railwaylines$Dest_Cod_INE <- as.character(df_railwaylines$Dest_Cod_INE)

df_railwaylines$Orig_Cod_INE <- substring(df_railwaylines$Dest_Cod_INE, 1, 5)
df_railwaylines$Dest_Cod_INE <- substring(df_railwaylines$Dest_Cod_INE, 1, 5)

df_railwaylines$Orig_Cod_INE <- as.numeric(df_railwaylines$Orig_Cod_INE)
df_railwaylines$Dest_Cod_INE <- as.numeric(df_railwaylines$Dest_Cod_INE)


# merge df_railwaylines with df_colera.merged

df_railwaylines.subset <- df_railwaylines[, c(8, 10, 11, 12, 13)]
colnames(df_railwaylines.subset)[1] <- CODIGO_INE_STR
head(df_railwaylines.subset)

df_colera.merged$`Codigo Ine` <- as.numeric(df_colera.merged$`Codigo Ine`)
rownames(df_colera.merged) <- 1:nrow(df_colera.merged)
head(df_colera.merged)

df_railwaylines.merged <- merge(df_colera.merged, df_railwaylines.subset, by = CODIGO_INE_STR)
head(df_railwaylines.merged)


# add coordinates from df_colera and save as df_colera_inla11

df_colera_coord <- df_colera[, c(CODIGO_INE_STR, "LAT_POB_new_num", "LNG_POB_new_num")]
df_colera_coord <- distinct(df_colera_coord, .keep_all = TRUE)
df_colera_coord <- na.omit(df_colera_coord)
colnames(df_colera_coord)[2:3] <- c(LAT_STR, LONG_STR)
head(df_colera_coord)

df_colera_inla11 <- merge(df_railwaylines.merged, df_colera_coord, by = CODIGO_INE_STR)
head(df_colera_inla11)

df_colera_inla11 <- df_colera_inla11[, c(LONG_STR, LAT_STR, TOTAL_INVASIONES_STR, "Length_km", "Time_H")]
colnames(df_colera_inla11)[1:5] <- c(LONGITUDE_STR, LATITUDE_STR, "invasiones", "km", "h")
head(df_colera_inla11)

# correct incorrect "longitude" and "latitude" values (TODO: delete after data correction)

df_colera_inla11 <- df_colera_inla11 %>%
  mutate(
    latitude = ifelse(latitude < 36.0, 36.0, ifelse(latitude > 43.0, 43.0, latitude)),
    longitude = ifelse(longitude < -10.0, -10.0, ifelse(longitude > 4.0, 4.0, longitude))
  )
head(df_colera_inla11)


# map ---------------------------------------------------------------------


mapS <- getData(name = "GADM", country = "Spain", level = 0)
mapS <- mapS %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>% # remove the islands from the map (main territory of Spain)
  mutate(area = st_area(.)) %>%
  arrange(desc(area)) %>%
  slice(1) %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # UTM transformation


# prediction ------------------------------------------------------------


# transforming coordinates

df_colera_inla11 <- st_as_sf(df_colera_inla11, coords = c(LONGITUDE_STR, LATITUDE_STR))
st_crs(df_colera_inla11) <- "EPSG:4326"
df_colera_inla11 <- st_filter(df_colera_inla11, mapS)
head(df_colera_inla11)
nrow(df_colera_inla11)

# observed "invasiones" 

ggplot() + geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla11, aes(col = invasiones)) +
  scale_color_viridis()

# raster grid covering map

grid <- rast(mapS, nrows = 100, ncols = 100)

# coordinates of all cells

xy <- xyFromCell(grid, 1:ncell(grid))
head(xy)

# transform points to a sf object

df_colera_inla11p <- st_as_sf(as.data.frame(xy), coords = c(X_STR, Y_STR), crs = st_crs(mapS))
head(df_colera_inla11p)

# indices points within the map

indicespointswithin <- which(st_intersects(df_colera_inla11p, mapS, sparse = FALSE))

# points within the map

df_colera_inla11p <- st_filter(df_colera_inla11p, mapS)
head(df_colera_inla11p)

# plot
# ggplot() + geom_sf(data = mapS) +
#   geom_sf(data = df_colera_inla11p)

# covariates

nearest_features <- st_nearest_feature(df_colera_inla11p, df_colera_inla11)
df_colera_inla11p$km <- df_colera_inla11$km[nearest_features]
df_colera_inla11p$h <- df_colera_inla11$h[nearest_features]

head(df_colera_inla11)
head(df_colera_inla11p)

p1 <- ggplot() + geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla11, aes(col = km)) +
  scale_color_viridis()
p2 <- ggplot() + geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla11, aes(col = h)) +
  scale_color_viridis()
p1 / p2

# transforming coordinates to UTM

st_crs("EPSG:4326")$proj4string
projMercator <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0
+x_0=0 +y_0=0 +k=1 +units=km +nadgrids=@null +wktext +no_defs"
df_colera_inla11 <- st_transform(df_colera_inla11, crs = projMercator)
df_colera_inla11p <- st_transform(df_colera_inla11p, crs = projMercator)

# observed coordinates

coo <- st_coordinates(df_colera_inla11)

# predicted coordinates

coop <- st_coordinates(df_colera_inla11p)

# mesh construction

mesh <- inla.mesh.2d(loc = coo, max.edge = c(200, 500), cutoff = 1)
mesh$n

plot(mesh)
points(coo, col = "red")
# axis(1)
# axis(2)

# building the SPDE on the mesh

spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)

# index set

indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)

# projection matrix

A <- inla.spde.make.A(mesh = mesh, loc = coo)
dim(A)
nrow(A)

# projection matrix for the prediction

Ap <- inla.spde.make.A(mesh = mesh, loc = coop)
dim(Ap)
nrow(Ap)

# stack for estimation stk.e

stk.e <- inla.stack(
  tag = "est",
  data = list(y = df_colera_inla11$invasiones),
  A = list(1, A),
  effects = list(
    data.frame(
      b0 = rep(1, nrow(A)),
      km = df_colera_inla11$km,
      h = df_colera_inla11$h
    ),
    s = indexs
  )
)

# stack for prediction stk.p

stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap),
  effects = list(
    data.frame(
      b0 = rep(1, nrow(Ap)),
      km = df_colera_inla11p$km,
      h = df_colera_inla11p$h
    ),
    s = indexs
  )
)

# stk.full has stk.e and stk.p

stk.full <- inla.stack(stk.e, stk.p)

# model formula 

formula <- y ~ 0 + b0 + km + h + f(s, model = spde)

cl <- makePSOCKcluster(4, setup_strategy = "sequential")
registerDoParallel(cl)

# inla() call

res <- inla(
  formula,
  family = "gaussian",
  data = inla.stack.data(stk.full),
  control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full)),
  control.compute = list()
)

stopCluster(cl)

# results

res$summary.fixed

# mapping predicted "invasiones"

index <- inla.stack.index(stack = stk.full, tag = "pred")$data
pred_mean <- res$summary.fitted.values[index, "mean"]
pred_ll <- res$summary.fitted.values[index, "0.025quant"]
pred_ul <- res$summary.fitted.values[index, "0.975quant"]

grid$mean <- NA
grid$ll <- NA
grid$ul <- NA

grid$mean[indicespointswithin] <- pred_mean
grid$ll[indicespointswithin] <- pred_ll
grid$ul[indicespointswithin] <- pred_ul

summary(grid) # negative values for the lower limit

# plot

levelplot(grid, layout = c(1, 3), names.attr = c("mean", "2.5 percentile", "97.5 percentile"))

# exceed probabilities

excprob <- sapply(res$marginals.fitted.values[index],FUN = function(marg){1-inla.pmarginal(q = 100, marginal = marg)})

grid$excprob <- NA
grid$excprob[indicespointswithin] <- excprob

# plot 

levelplot(grid$excprob, margin = FALSE)
