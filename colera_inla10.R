# https://www.paulamoraga.com/book-geospatial/index.html
# https://inla.r-inla-download.org/R/stable/bin/

# install.packages("INLA_21.02.23.tar", repos = NULL, type = "source")
# install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)


library(lwgeom)
library(sf)
library(viridis)
library(foreach)
library(INLA)
library(splancs)
library(raster)
library(reshape2)
library(doParallel)


# load("~/epi_predict/colera_data.RData")


# constants ---------------------------------------------------------------


ESPG_CODE <- 25830 # ESPG code of Spain which is code 25830 and corresponds to UTM zone 30 North
MONTH_STR <- "month"
LONG_STR <- "long"
LAT_STR <- "lat"
X_STR <- "x"
Y_STR <- "y"


# main --------------------------------------------------------------------

  
# map Spain ---------------------------------------------------------------


# GADM which is the database of global administrative boundaries, country equal to Spain, 
# and level equal to 0 which corresponds to the level of administrative subdivision country
mapS <- getData(name = "GADM", country = "Spain", level = 0)
mapS <- mapS %>%
  st_as_sf() %>%
  st_cast("POLYGON") %>% # remove the islands from the map (main territory of Spain)
  mutate(area = st_area(.)) %>%
  arrange(desc(area)) %>%
  slice(1) %>%
  st_transform(ESPG_CODE) # UTM transformation


# data --------------------------------------------------------------------


# format column "Fecha" as POSIXlt
df_colera_invasiones$Fecha <- month(as.POSIXlt(df_colera_invasiones$Fecha, format = DATE_FORMAT))

# select columns "Fecha", "Codigo Ine", "LNG_POB_new_num", "LAT_POB_new_num", "invasiones"
dataCi <- df_colera_invasiones[, c(
  FECHA_STR, CODIGO_INE_STR,
  "LNG_POB_new_num",
  "LAT_POB_new_num",
  INVASIONES_STR
)]

# rename column names
names(dataCi) <- c(MONTH_STR, "id", LONG_STR, LAT_STR, INVASIONES_STR)

# remove NA
dataCi <- na.omit(dataCi) 

# transform long and lat to UTM coordinates
p <- st_as_sf(data.frame(long = dataCi$long, lat = dataCi$lat), coords = c(LONG_STR, LAT_STR))
st_crs(p) <- st_crs(4326) # EPSG code 4326 
p <- p %>% st_transform(ESPG_CODE) # EPSG code 25830
dataCi[, c(X_STR, Y_STR)] <- st_coordinates(p)

# invasiones located within the main territory of Spain and remove from the islands
ind <- st_intersects(mapS, p)
dataCi <- dataCi[ind[[1]], ]
head(dataCi)

ggplot(mapS) + geom_sf() + coord_sf(datum = st_crs(mapS)) +
  geom_point(data = dataCi, aes(x = x, y = y)) + 
  xlab("") + ylab("") +
  ggtitle(paste0(INVASIONES_STR, " de colera, ", ANO_STR)) + 
  theme_bw()

ggplot(mapS) + geom_sf() + coord_sf(datum = NA) +
  geom_point(
    data = dataCi, aes(x = x, y = y, color = invasiones)
  ) +
  labs(x = "", y = "") +
  ggtitle(paste0(INVASIONES_STR, " de colera por mes, ", ANO_STR)) +
  scale_color_viridis() +
  facet_wrap(~month) +
  theme_bw()

# invasiones over time
ggplot(dataCi, aes(x = month, y = invasiones, group = id, color = id)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(6, 7, 8, 9, 10, 11)) + # months
  theme_bw() + theme(legend.position = "none")


# model -------------------------------------------------------------------


# mesh construction
# construct a triangulated mesh on top of which the GMRF representation is built
coo <- cbind(dataCi$x, dataCi$y) # matrix with the coordinates as initial mesh vertices
bnd <- inla.nonconvex.hull(st_coordinates(mapS)[, 1:2]) # boundary of the mesh is equal to a polygon containing the map locations
mesh <- inla.mesh.2d(
  loc = coo, boundary = bnd,
  max.edge = c(100000, 200000), # maximum allowed triangle edge lengths in the region and in the extension
  cutoff = 1000 # minimum allowed distance between points
)

# number of vertices of the mesh and plot the mesh
mesh$n
plot(mesh)
points(coo, col = "red")

# build the SPDE model and specify Penalised Complexity (PC) priors
spde <- inla.spde2.pcmatern(
  mesh = mesh, alpha = 2, constr = TRUE, # spatial data (d = 2) and set constr = TRUE to impose an integrate-to-zero constraint
  prior.range = c(10000, 0.01), # P(range < 10000) = 0.01 / probability that the range is less than 10 km is very small
  prior.sigma = c(3, 0.01) # P(sigma > 3) = 0.01 / variability of the data 
)

# index set for the latent spatio-temporal Gaussian model 
timesn <- length(unique(dataCi$month))
indexs <- inla.spde.make.index("s", 
                               n.spde = spde$n.spde, # number of vertices in the SPDE model
                               n.group = timesn # the number of times 
)
lengths(indexs)
# s: indices of the SPDE vertices repeated the number of times
# s.group: indices of the times repeated the number of mesh vertices
# s.repl: vector of 1s with length given by the number of mesh vertices times the number of times (spde$n.spde*timesn)

# build projection matrix A
group <- dataCi$month - min(dataCi$month) + 1 # vector of length equal to number of months (6, 7, 8, 9, 10, 11)
A <- inla.spde.make.A(mesh = mesh, loc = coo, group = group)

# construct the data with the locations and times where we want to make predictions
bb <- st_bbox(mapS)
x <- seq(bb$xmin - 1, bb$xmax + 1, length.out = 50)
y <- seq(bb$ymin - 1, bb$ymax + 1, length.out = 50)
dp <- as.matrix(expand.grid(x, y))
plot(dp, asp = 1, main = "grid locations for prediction")

# only the locations that lie within the map of Spain
p <- st_as_sf(data.frame(x = dp[, 1], y = dp[, 2]), coords = c(X_STR, Y_STR))
st_crs(p) <- st_crs(25830)
ind <- st_intersects(mapS, p)
dp <- dp[ind[[1]], ]
plot(dp, asp = 1, main = "locations for prediction")


# forecasting -------------------------------------------------------------


# construct the data that includes the coordinates and the six times
# time 1 is 6 (June), time 2 is 7(July), time 3 is 8(August), etc.
dp <- rbind(cbind(dp, 1), cbind(dp, 2), cbind(dp, 3), cbind(dp, 4), cbind(dp, 5), cbind(dp, 6))
summary(dp)

# construct the matrix Ap
coop <- dp[, 1:2] # prediction locations 
groupp <- dp[, 3] # time indices
Ap <- inla.spde.make.A(mesh = mesh, loc = coop, group = groupp)

# stack with data for estimation and prediction
stk.e <- inla.stack(
  tag = "est", # string for identifying the data
  data = list(y = dataCi$invasiones), # list of data invasiones
  A = list(1, A), # list of projection matrix A
  effects = list(data.frame(b0 = rep(1, nrow(dataCi))), s = indexs) # list with fixed and random effects
)

stk.p <- inla.stack(
  tag = "pred",
  data = list(y = NA),
  A = list(1, Ap), # list of projection matrix Ap
  effects = list(data.frame(b0 = rep(1, nrow(dp))), s = indexs)
)

# both stacks together
stk.full <- inla.stack(stk.e, stk.p)

# model formula to fit the model
rprior <- list(theta = list(prior = "pccor1", param = c(0, 0.9)))
# define rprior with the prior "pccor1" which is a PC prior for the autocorrelation a = 1 in the base model
# assuming P(a > 0) = 0.9
formula <- y ~ 0 + b0 + f(s,
                          model = spde, group = s.group,
                          control.group = list(model = "ar1", hyper = rprior)
) # specified that across time, the process evolves according to an AR(1) process where the prior for the autocorrelation
# remove intercept (adding 0) and add it as a covariate term (adding b0)

cl <- makePSOCKcluster(4, setup_strategy = "sequential")
registerDoParallel(cl)

# inla() call
res <- inla(
  formula,
  data = inla.stack.data(stk.full),
  control.predictor = list(compute = TRUE, A = inla.stack.A(stk.full))
)

stopCluster(cl)

# results
summary(res)

# distributions of the intercept, the precision of the measurement error, 
# and the standard deviation, range and autocorrelation parameters of the spatio-temporal random effect 
list_marginals <- list(
  "b0" = res$marginals.fixed$b0,
  "precision Gaussian obs" = res$marginals.hyperpar$"Precision for the Gaussian observations",
  "range" = res$marginals.hyperpar$"Range for s",
  "stdev" = res$marginals.hyperpar$"Stdev for s",
  "rho" = res$marginals.hyperpar$"GroupRho for s"
)
marginals <- data.frame(do.call(rbind, list_marginals))
marginals$parameter <- rep(names(list_marginals),
                           times = sapply(list_marginals, nrow)
)

# plot marginals, one plot for parameter
ggplot(marginals, aes(x = x, y = y)) + geom_line() +
  facet_wrap(~parameter, scales = "free") +
  ggtitle("posterior distributions of the model parameters") +
  labs(x = "", y = "Density") + theme_bw()

# mapping invasiones predictions
index <- inla.stack.index(stack = stk.full, tag = "pred")$data
dp <- data.frame(dp)
names(dp) <- c(X_STR, Y_STR, MONTH_STR)

dp$pred_mean <- res$summary.fitted.values[index, "mean"]
dp$pred_ll <- res$summary.fitted.values[index, "0.025quant"]
dp$pred_ul <- res$summary.fitted.values[index, "0.975quant"]
dpm <- melt(dp,
            id.vars = c(X_STR, Y_STR, MONTH_STR),
            measure.vars = c("pred_mean", "pred_ll", "pred_ul")
)
colnames(dpm)[5] <- INVASIONES_STR
head(dpm)

# plot predictions and lower and upper limits of 95% CI in Spain for month 6, 7, 8, 9, 10 and 11
ggplot(mapS) + geom_sf() + coord_sf(datum = NA) +
  geom_tile(data = dpm, aes(x = x, y = y, fill = invasiones)) +
  labs(x = "", y = "") +
  facet_wrap(variable ~ month, ncol = 6) +
  scale_fill_viridis(INVASIONES_STR) +
  theme_bw()
