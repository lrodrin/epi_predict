# https://inla.r-inla-download.org/R/stable/bin/

# install.packages("INLA_21.02.23.tar", repos = NULL, type = "source")
# install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)


library(ggplot2)
library(sf)
library(dplyr)
library(zoo)
library(mapview)
library(RColorBrewer)
library(leafpop)
library(INLA)
library(spdep)
library(doParallel)
library(leafsync)


# load("colera_data_month.RData")
# load("temperatures.RData")
# load("rain.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)

COVTEMP_STR <- "covtemp"
COVPREC_STR <- "covprec"
TOTAL_POBLACION_STR <- "Total_poblacion"
TOTAL_INVASIONES_STR <- paste("Total", INVASIONES_STR, sep = "_")
TOTAL_DEFUNCIONES_STR <- paste("Total", DEFUNCIONES_STR, sep = "_")
TASA_INCIDENCIA_STR <- "Tasa_incidencia"
TASA_MORTALIDAD_STR <- "Tasa_mortalidad"
LONG_STR <- "long"
LAT_STR <- "lat"
X_STR <- "x"
Y_STR <- "y"
SMR_STR <- "SMR"
RR_STR <- "RR"

PALETTE <- colorRampPalette(brewer.pal(9, "YlOrRd"))
PLOT_LABELS <- c("low", "mid-low", "mid-high", "high")
PLOT_COLORS_BY_LABELS <- c(
  "low" = "blue",
  "mid-low" = "green",
  "mid-high" = "orange",
  "high" = "red"
)


# functions ---------------------------------------------------------------


generate_maps <- function(data, numeric_col, month_col, months, palette) {
  
  map_list <- list()
  
  for (month in months) {
    
    filtered_data <- data %>%
      filter({{ month_col }} == month)
    
    map <- mapview(filtered_data, zcol = {{ numeric_col }}, color = "gray", alpha.regions = 0.8,
                   layer.name = paste0({{ numeric_col }}, ".month_", month), col.regions = palette,
                   map.types = "CartoDB.Positron")
    
    map_list[[as.character(month)]] <- map
  }
  
  return(map_list)
  
}


# main --------------------------------------------------------------------


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


# add coordinates from df_colera and save as df_colera_inla7

df_colera_coord <- df_colera[, c(CODIGO_INE_STR, "LAT_POB_new_num", "LNG_POB_new_num")]
df_colera_coord <- distinct(df_colera_coord, .keep_all = TRUE)
df_colera_coord <- na.omit(df_colera_coord)
colnames(df_colera_coord)[2:3] <- c(LAT_STR, LONG_STR)
head(df_colera_coord)

df_colera_inla7 <- merge(df_covariates.merged, df_colera_coord, by = CODIGO_INE_STR)
head(df_colera_inla7)

# df_colera_inla7 <- df_colera_inla7[, !colnames(df_colera_inla7) %in% c(PROVINCIA_STR, MUNICIPIO_STR, "Tasa_incidencia", "Tasa_mortalidad")]
df_colera_inla7 <- df_colera_inla7[, !colnames(df_colera_inla7) %in% c(PROVINCIA_STR, MUNICIPIO_STR)]
head(df_colera_inla7)


# correct incorrect "long" and "lat" values (TODO: delete after data correction)

df_colera_inla7 <- df_colera_inla7 %>%
  mutate(
    lat = ifelse(lat < 36.0, 36.0, ifelse(lat > 43.0, 43.0, lat)),
    long = ifelse(long < -10.0, -10.0, ifelse(long > 4.0, 4.0, long))
  )
head(df_colera_inla7)


# map ---------------------------------------------------------------------


# 1.
# mapS <- getData(name = "GADM", country = "Spain", level = 0)

# 2. 
# library(rnaturalearth)
# mapS <- ne_countries(country = "Spain", scale = "large", returnclass = "sf")
# 
# 
# mapS <- mapS %>%
#   st_as_sf() %>%
#   st_cast("POLYGON") %>% # remove the canary islands from the map (main territory of Spain)
#   mutate(area = st_area(.)) %>%
#   arrange(desc(area)) %>%
#   slice(1) %>%
#   st_transform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # UTM transformation

# 3. 
mapS <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
head(mapS)


# observations ------------------------------------------------------------


# # transforming coordinates
# 
# df_colera_inla7 <- st_as_sf(df_colera_inla7, coords = c(LONG_STR, LAT_STR))
# st_crs(df_colera_inla7) <- "EPSG:4326"
# df_colera_inla7 <- st_filter(df_colera_inla7, mapS)
# head(df_colera_inla7)
# nrow(df_colera_inla7)


# merge mapS and df_colera_inla7

df_colera_inla7 <- merge(mapS, df_colera_inla7, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(df_colera_inla7)


# "invasiones" 

df_colera_inla7.copy <- df_colera_inla7
df_colera_inla7.copy$invasiones_factor <-
  cut(
    df_colera_inla7$Total_invasiones,
    breaks = c(-1, 10, 50, 90, Inf),
    labels = PLOT_LABELS
  )

ggplot() +
  geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla7.copy, aes(col = invasiones_factor, fill = invasiones_factor)) +
  scale_color_manual(values = PLOT_COLORS_BY_LABELS) +
  scale_fill_manual(values = PLOT_COLORS_BY_LABELS) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(color = INVASIONES_STR, fill = INVASIONES_STR)


# "defunciones" 

df_colera_inla7.copy$defunciones_factor <-
  cut(
    df_colera_inla7$Total_defunciones,
    breaks = c(-1, 10, 30, 100, Inf),
    labels = PLOT_LABELS
  )

ggplot() + 
  geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla7.copy, aes(col = defunciones_factor, fill = defunciones_factor)) +
  scale_color_manual(values = PLOT_COLORS_BY_LABELS) +
  scale_fill_manual(values = PLOT_COLORS_BY_LABELS) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(color = DEFUNCIONES_STR, fill = DEFUNCIONES_STR) 


# "Tasa_incidencia" 

df_colera_inla7.copy$incidencia_factor <-
  cut(
    df_colera_inla7$Tasa_incidencia,
    breaks = c(-1, 0.5, 1, 2, Inf),
    labels = PLOT_LABELS
  )

ggplot() + 
  geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla7.copy, aes(col = incidencia_factor, fill = incidencia_factor)) +
  scale_color_manual(values = PLOT_COLORS_BY_LABELS) +
  scale_fill_manual(values = PLOT_COLORS_BY_LABELS) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(color = TASA_INCIDENCIA_STR, fill = TASA_INCIDENCIA_STR) 


# "Tasa_mortalidad" 

df_colera_inla7.copy$mortalidad_factor <-
  cut(
    df_colera_inla7$Tasa_mortalidad,
    breaks = c(-1, 0.5, 1, 2, Inf),
    labels = PLOT_LABELS
  )

ggplot() + 
  geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla7.copy, aes(col = mortalidad_factor, fill = mortalidad_factor)) +
  scale_color_manual(values = PLOT_COLORS_BY_LABELS) +
  scale_fill_manual(values = PLOT_COLORS_BY_LABELS) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(color = TASA_MORTALIDAD_STR, fill = TASA_MORTALIDAD_STR) # TODO: more plots


# SMR ---------------------------------------------------------------------


df_colera_inla7$SMR <- ifelse(
  df_colera_inla7$Total_defunciones == 0,
  0,  # set to 0 when Total_defunciones is zero
  df_colera_inla7$Total_invasiones / df_colera_inla7$Total_defunciones
)
head(df_colera_inla7)


# plots

mapview(df_colera_inla7, zcol = SMR_STR, color = "gray", alpha.regions = 0.8,
        layer.name = SMR_STR, col.regions = PALETTE,
        map.types = "CartoDB.Positron")

# for each month

multi_maps_SMR <-
  generate_maps(
    data = df_colera_inla7,
    numeric_col = SMR_STR,
    month_col = Fecha,
    months = c(6, 7, 8, 9, 10, 11),
    palette = PALETTE
  )

multi_maps_SMR
leafsync::sync(multi_maps_SMR)


# model -------------------------------------------------------------------


df_colera_inla7.sf <- st_as_sf(df_colera_inla7, crs = 4326)

# coordinates <- st_coordinates(df_colera_inla7.sf)
# df_colera_inla7.sf$long <- coordinates[, 1]
# df_colera_inla7.sf$lat <- coordinates[, 2]
# 
# df_colera_inla7.sf <- df_colera_inla7.sf %>% select(-geometry)
# head(df_colera_inla7.sf)


# neighbourhood matrix

nb <- poly2nb(df_colera_inla7.sf)
head(nb)

nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")


# model formula and inla() call

df_colera_inla7.sf$re_u <- 1:nrow(df_colera_inla7.sf)
df_colera_inla7.sf$re_v <- 1:nrow(df_colera_inla7.sf)

# formula <- Total_invasiones ~ covtemp + covprec +
#   f(re_u, model = "besag", graph = g, scale.model = TRUE) +
#   f(re_v, model = "iid")

formula <- Total_invasiones ~ covtemp +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

cl <- makePSOCKcluster(4, setup_strategy = "sequential")
registerDoParallel(cl)

res <- inla(formula, family = "poisson", data = df_colera_inla7.sf, E = Total_defunciones,
            control.predictor = list(compute = TRUE),
            control.compute = list())

stopCluster(cl)


# results -----------------------------------------------------------------


res$summary.fixed
res$summary.fitted.values[1:3, ]

df_colera_inla7.sf$RR <- res$summary.fitted.values[, "mean"] # relative risk
df_colera_inla7.sf$LL <- res$summary.fitted.values[, "0.025quant"] # lower limit 95% CI
df_colera_inla7.sf$UL <- res$summary.fitted.values[, "0.975quant"] # upper limit 95% CI


# correlation and significance

cor_rr_covtemp <- cor(df_colera_inla7.sf$RR, df_colera_inla7.sf$covtemp)
# cor_rr_covprec <- cor(df_colera_inla7.sf$RR, df_colera_inla7.sf$covprec)

significance_covtemp <- ifelse(cor_rr_covtemp > 0.05, "significativa", "no significativa")
cat("Correlation between predictions and", COVTEMP_STR, ":", cor_rr_covtemp, significance_covtemp, "\n")

# significance_covprec <- ifelse(cor_rr_covprec > 0.05, "significativa", "no significativa")
# cat("Correlation between predictions and", COVPREC_STR, ":", cor_rr_covprec, significance_covprec, "\n")


# disease risk ------------------------------------------------------------


mapview(df_colera_inla7.sf, zcol = RR_STR, color = "gray", alpha.regions = 0.8,
        layer.name = RR_STR, col.regions = PALETTE,
        map.types = "CartoDB.Positron")


# comparing SMR and RR 

at <- seq(min(df_colera_inla7.sf$SMR), max(df_colera_inla7.sf$SMR), length.out = 7)

m1 <- mapview(df_colera_inla7.sf, zcol = SMR_STR, color = "gray",
              col.regions = PALETTE, at = at, layer.name = SMR_STR, map.types = "CartoDB.Positron")
m2 <- mapview(df_colera_inla7.sf, zcol = RR_STR, color = "gray",
              col.regions = PALETTE, at = at, layer.name = RR_STR, map.types = "CartoDB.Positron")

leafsync::sync(m1, m2)


# for each month

multi_maps_RR <-
  generate_maps(
    data = df_colera_inla7.sf,
    numeric_col = RR_STR,
    month_col = Fecha,
    months = c(6, 7, 8, 9, 10, 11),
    palette = PALETTE
  )

multi_maps_RR
leafsync::sync(multi_maps_RR)


# exceed probabilities ------------------------------------------------


c <- 2
df_colera_inla7.sf$exc <- sapply(res$marginals.fitted.values,
                  FUN = function(marg){1 - inla.pmarginal(q = c, marginal = marg)})

# plot

mapview(df_colera_inla7.sf, zcol = "exc", color = "gray", 
        layer.name = "exc", col.regions = PALETTE,
        map.types = "CartoDB.Positron")


# for each month

multi_maps_exc <-
  generate_maps(
    data = df_colera_inla7.sf,
    numeric_col = "exc",
    month_col = Fecha,
    months = c(6, 7, 8, 9, 10, 11),
    palette = PALETTE
  )

multi_maps_exc
leafsync::sync(multi_maps_exc)
