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
library(RColorBrewer)
library(gridExtra)


# load("colera_data_month.RData")
# load("temperatures.RData")
# load("rain.RData")


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)

COVTEMP_STR <- "covtemp"
COVPREC_STR <- "covprec"
LONG_STR <- "long"
LAT_STR <- "lat"
X_STR <- "x"
Y_STR <- "y"
PLOT_LABELS <- c("low", "mid-low", "mid-high", "high")


# data --------------------------------------------------------------------


df_covtemp <- df_temperatures.parsed
df_covprec <- df_rain.parsed


# format df_covtemp and df_covprec

df_covtemp$mes <- as.integer(format(df_covtemp$mes, "%m"))
df_covprec$mes <- as.integer(format(df_covprec$mes, "%m"))

df_covtemp <- subset(df_covtemp, !(localidad %in% c("Alicante (M.)", "Zaragoza (E.P.)")))
df_covprec <- subset(df_covprec, !(localidad %in% c("Alicante (M.)", "Zaragoza (E.P.)")))


# merge df_covtemp and df_covprec with df_colera.merged

df_covtemp.subset <- df_covtemp[, c(4, 5, 6)]
df_covprec.subset <- df_covprec[, c(4, 5, 6)]
colnames(df_covtemp.subset)[1:3] <- c(CODIGO_INE_STR, FECHA_STR, COVTEMP_STR)
colnames(df_covprec.subset)[1:3] <- c(CODIGO_INE_STR, FECHA_STR, COVPREC_STR)
head(df_covtemp.subset)
head(df_covprec.subset)

df_colera.merged$`Codigo Ine` <- as.numeric(df_colera.merged$`Codigo Ine`)
rownames(df_colera.merged) <- 1:nrow(df_colera.merged)
head(df_colera.merged)

df_environmental.merged <- merge(df_covtemp.subset, df_covprec.subset, by = c(CODIGO_INE_STR, FECHA_STR))
head(df_environmental.merged)

df_covariates.merged <- merge(df_colera.merged, df_environmental.merged, by = c(CODIGO_INE_STR, FECHA_STR))
head(df_covariates.merged)


# add coordinates from df_colera and save as df_colera_inla11

df_colera_coord <- df_colera[, c(CODIGO_INE_STR, "LAT_POB_new_num", "LNG_POB_new_num")]
df_colera_coord <- distinct(df_colera_coord, .keep_all = TRUE)
df_colera_coord <- na.omit(df_colera_coord)
colnames(df_colera_coord)[2:3] <- c(LAT_STR, LONG_STR)
head(df_colera_coord)

df_colera_inla11 <- merge(df_covariates.merged, df_colera_coord, by = CODIGO_INE_STR)
head(df_colera_inla11)

df_colera_inla11 <- df_colera_inla11[, c(LONG_STR, LAT_STR, TOTAL_INVASIONES_STR, COVTEMP_STR, COVPREC_STR, FECHA_STR)]
colnames(df_colera_inla11)[3] <- INVASIONES_STR
colnames(df_colera_inla11)[6] <- "month"
head(df_colera_inla11)


# correct incorrect "longitude" and "latitude" values (TODO: delete after data correction)

df_colera_inla11 <- df_colera_inla11 %>%
  mutate(
    lat = ifelse(lat < 36.0, 36.0, ifelse(lat > 43.0, 43.0, lat)),
    long = ifelse(long < -10.0, -10.0, ifelse(long > 4.0, 4.0, long))
  )
head(df_colera_inla11)


# map ---------------------------------------------------------------------


# 1.
# mapS <- getData(name = "GADM", country = "Spain", level = 0)

# 2. 
library(rnaturalearth)
mapS <- ne_countries(country = "Spain", scale = "large", returnclass = "sf")


mapS <- mapS %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>% # remove the canary islands from the map (main territory of Spain)
  mutate(area = st_area(.)) %>%
  arrange(desc(area)) %>%
  slice(1) %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # UTM transformation


# prediction ------------------------------------------------------------


# transforming coordinates

df_colera_inla11 <- st_as_sf(df_colera_inla11, coords = c(LONG_STR, LAT_STR))
st_crs(df_colera_inla11) <- "EPSG:4326"
df_colera_inla11 <- st_filter(df_colera_inla11, mapS)
head(df_colera_inla11)
nrow(df_colera_inla11)


# observed "invasiones" 

df_colera_inla11.copy <- df_colera_inla11
df_colera_inla11.copy$invasiones_factor <-
  cut(
    df_colera_inla11$invasiones,
    breaks = c(-1, 10, 50, 100, Inf),
    labels = PLOT_LABELS
  )

ggplot() + geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla11.copy, aes(col = invasiones_factor)) +
  scale_color_manual(values = c(
    "low" = "blue",
    "mid-low" = "green",
    "mid-high" = "orange",
    "high" = "red"
  )) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(color = INVASIONES_STR) # TODO: more plots


# modelling ---------------------------------------------------------------


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

# plot points of the map
# ggplot() + geom_sf(data = mapS) +
#   geom_sf(data = df_colera_inla11p)


# covariates --------------------------------------------------------------


nearest_features <- st_nearest_feature(df_colera_inla11p, df_colera_inla11)
df_colera_inla11p$covtemp <- df_colera_inla11$covtemp[nearest_features]
df_colera_inla11p$covprec <- df_colera_inla11$covprec[nearest_features]

head(df_colera_inla11)
head(df_colera_inla11p)

# plot of "covtemp" vs "invasiones"

ggplot(df_colera_inla11, aes(x = covtemp, y = invasiones)) +
  geom_point() +
  # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(x = COVTEMP_STR, y = INVASIONES_STR) +
  theme_minimal()

# plot of "covprec" vs "invasiones"

ggplot(df_colera_inla11, aes(x = covprec, y = invasiones)) +
  geom_point() +
  # geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(x = COVPREC_STR, y = INVASIONES_STR) +
  theme_minimal()

# plot of "covtemp"

df_colera_inla11.copy$covtemp_factor <-
  cut(
    df_colera_inla11.copy$covtemp,
    breaks = c(5, 10, 15, 20, Inf),
    labels = PLOT_LABELS
  )

ggplot() + geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla11.copy, aes(col = covtemp_factor)) +
  scale_color_manual(values = c(
    "low" = "blue",
    "mid-low" = "green",
    "mid-high" = "orange",
    "high" = "red"
  )) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(color = COVTEMP_STR) 

# TODO: more plots


# transforming coordinates to UTM

st_crs("EPSG:4326")$proj4string
projMercator <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0
+x_0=0 +y_0=0 +k=1 +units=km +nadgrids=@null +wktext +no_defs"
df_colera_inla11 <- st_transform(df_colera_inla11, crs = projMercator)
df_colera_inla11p <- st_transform(df_colera_inla11p, crs = projMercator)


# inla --------------------------------------------------------------------


# observed coordinates

coo <- st_coordinates(df_colera_inla11)


# predicted coordinates

coop <- st_coordinates(df_colera_inla11p)


# mesh construction

mesh <- inla.mesh.2d(loc = coo, max.edge = c(200, 500), cutoff = 1)
mesh$n

plot(mesh)
points(coo, col = "red")


# building the SPDE on the mesh

spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)


# index set

indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)


# projection matrix

A <- inla.spde.make.A(mesh = mesh, loc = coo)
dim(A)


# projection matrix for the prediction

Ap <- inla.spde.make.A(mesh = mesh, loc = coop)
dim(Ap)


# stack with data for estimation and prediction

stk.e <- inla.stack(
  tag = "est",
  data = list(y = df_colera_inla11$invasiones),
  A = list(1, A),
  effects = list(
    data.frame(
      b0 = rep(1, nrow(A)),
      covtemp  = df_colera_inla11$covtemp 
    ),
    s = indexs
  )
)

stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap),
  effects = list(
    data.frame(
      b0 = rep(1, nrow(Ap)),
      covtemp  = df_colera_inla11p$covtemp
    ),
    s = indexs
  )
)


# model formula and inla() call

formula <- y ~ 0 + b0 + covtemp + f(s, model = spde)

stk.full <- inla.stack(stk.e, stk.p)

cl <- makePSOCKcluster(4, setup_strategy = "sequential")
registerDoParallel(cl)

res <- inla(
  formula,
  family = "gaussian",
  data = inla.stack.data(stk.full),
  control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full)),
  control.compute = list()
)

stopCluster(cl)


# results -----------------------------------------------------------------


# coefficients

res$summary.fixed
res$summary.fixed[, c("mean", "sd", "0.025quant", "0.975quant")]

index <- inla.stack.index(stack = stk.full, tag = "pred")$data
pred_mean <- res$summary.fitted.values[index, "mean"]
pred_ll <- res$summary.fitted.values[index, "0.025quant"]
pred_ul <- res$summary.fitted.values[index, "0.975quant"]


# marginal effects of "covtemp" 

effects_covtemp <- res$marginals.fitted.values$covtemp


# partial dependence plots

plot(df_colera_inla11$covtemp, effects_covtemp, type = "l", xlab = COVTEMP_STR, ylab = "Effect")


# correlation and significance

covtemp_values <- df_colera_inla11p$covtemp

correlation_covtemp <- cor(pred_mean, covtemp_values)
significance_covtemp <- ifelse(correlation_covtemp > 0.05, "significativa", "no significativa")
cat("Correlation between predictions and", COVTEMP_STR, ":", correlation_covtemp, significance_covtemp, "\n")


# mapping predicted "invasiones" ------------------------------------------

grid$mean <- NA
grid$ll <- NA
grid$ul <- NA

grid$mean[indicespointswithin] <- pred_mean
grid$ll[indicespointswithin] <- pred_ll
grid$ul[indicespointswithin] <- pred_ul

summary(grid) # negative values for the lower limit

# plots

colorkey_vals <- seq(70, 220, by = 15)
plot_mean <- levelplot(
  grid$mean,
  col.regions = colorRampPalette(brewer.pal(9, "YlOrRd")),
  margin = FALSE,
  main = "mean",
  colorkey = list(
    at = colorkey_vals,
    labels = list(at = colorkey_vals, labels = colorkey_vals)
  )
)

colorkey_vals <- seq(-5, 116, by = 12)
plot_ll <- levelplot(
  grid$ll,
  col.regions =  colorRampPalette(brewer.pal(9, "YlGnBu")),
  margin = FALSE,
  main = "2.5 percentile",
  colorkey = list(
    at = colorkey_vals,
    labels = list(at = colorkey_vals, labels = colorkey_vals)
  )
)

colorkey_vals <- seq(136, 330, by = 19)
plot_ul <- levelplot(
  grid$ul,
  col.regions = colorRampPalette(brewer.pal(9, "YlOrBr")),
  margin = FALSE,
  main = "97.5 percentile",
  colorkey = list(
    at = colorkey_vals,
    labels = list(at = colorkey_vals, labels = colorkey_vals)
  )
)

grid.arrange(plot_mean, plot_ll, plot_ul, ncol = 3)

# levelplot(
#   grid,
#   layout = c(1, 3),
#   names.attr = c("mean", "2.5 percentile", "97.5 percentile"),
#   col.regions = colorRampPalette(brewer.pal(9, "YlOrRd"))
# )


# exceed probabilities ----------------------------------------------------


excprob <- sapply(res$marginals.fitted.values[index],FUN = function(marg){1-inla.pmarginal(q = 25, marginal = marg)})

gridexcprob <- grid
gridexcprob$excprob <- NA
gridexcprob$excprob[indicespointswithin] <- excprob


# plot

colorkey_vals <- seq(0.90, 1.00, by = 0.01)
levelplot(
  gridexcprob$excprob,
  col.regions = colorRampPalette(brewer.pal(9, "YlOrRd")),
  margin = FALSE,
  colorkey = list(
    at = colorkey_vals,
    labels = list(at = colorkey_vals, labels = colorkey_vals)
  )
)

# levelplot(gridexcprob$excprob, margin = FALSE)
