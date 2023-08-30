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
library(lubridate)


# load("colera_data_month.RData")
# load("temperatures.RData")
# load("rain.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
COLERA_PLOTS_DIR <- "colera_plots"
dir.create(COLERA_PLOTS_DIR, showWarnings = FALSE)

COVTEMP_STR <- "covtemp"
COVPREC_STR <- "covprec"
TOTAL_POBLACION_STR <- "Total_poblacion"
TOTAL_INVASIONES_STR <- paste("Total", INVASIONES_STR, sep = "_")
TOTAL_DEFUNCIONES_STR <- paste("Total", DEFUNCIONES_STR, sep = "_")
TASA_INCIDENCIA_STR <- "Tasa_incidencia"
TASA_MORTALIDAD_STR <- "Tasa_mortalidad"
INVASIONES_FACTOR_STR <- "invasiones_factor"
DEFUNCIONES_FACTOR_STR <- "defunciones_factor"
TASA_INCIDENCIA_FACTOR_STR <- "incidencia_factor"
TASA_MORTALIDAD_FACTOR_STR <- "mortalidad_factor"
LONG_STR <- "long"
LAT_STR <- "lat"
X_STR <- "x"
Y_STR <- "y"
SIR_STR <- "SIR"
SMR_STR <- "SMR"
RR_STR <- "RR"

MONTHS <- c(6, 7, 8, 9, 10, 11)

PALETTE <- colorRampPalette(brewer.pal(9, "YlOrRd"))
PLOT_LABELS <- c("low", "mid-low", "mid-high", "high")
PLOT_COLORS_BY_LABELS <- c(
  "low" = "blue",
  "mid-low" = "green",
  "mid-high" = "orange",
  "high" = "red"
)


# functions ---------------------------------------------------------------


factorize <- function(df_colera, var_col, factor_col, breaks) {
  #' Factorize numeric values into discrete intervals.
  #'
  #' This function takes a numeric column from a data frame and converts it into factors
  #' by dividing it into discrete intervals specified by the parameters.
  #'
  #' @param df_colera Data frame containing the data.
  #' @param var_col Name of the column to be factorized.
  #' @param factor_col Name of the new factor column.
  #' @param breaks Vector defining the interval breakpoints.
  #'
  #' @return The data frame with the new factor column added.
  
  df_colera[[factor_col]] <-
    cut(
      df_colera[[var_col]],
      breaks = breaks,
      labels = PLOT_LABELS
    )
  
  return(df_colera)
  
}


plot_observations <- function(df_colera, var_col, var_colname) {
  #' Create a plot of observations on a spatial map.
  #'
  #' This function generates a plot that displays observations on a spatial map.
  #' It uses the specified data frame and variable column to visualize the observations.
  #'
  #' @param df_colera Data frame containing the observations.
  #' @param var_col Column with numeric values for visualization.
  #' @param var_colname Name of the variable used for colour representation.
  #'
  #' @return A plot displaying observations on a spatial map.
  
  ggplot() +
    geom_sf(data = mapS) +
    geom_sf(data = df_colera, aes(col = !!sym(var_col), fill = !!sym(var_col))) +
    scale_color_manual(values = PLOT_COLORS_BY_LABELS) +
    scale_fill_manual(values = PLOT_COLORS_BY_LABELS) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(color = var_colname, fill = var_colname) +
    theme_void()
  
}


plot_observationsByMonth <- function(df_colera, var_col, var_colname) {
  #' Create a list of plots displaying observations by month on spatial maps.
  #'
  #' This function generates a list of plots, one for each specified month, displaying observations on spatial maps.
  #' It uses the provided data frame and variable column to visualize the observations.
  #'
  #' @param df_colera Data frame containing the observations.
  #' @param var_col Column with numeric values for visualization.
  #' @param var_colname Name of the variable used for colour representation.
  #'
  #' @return A list of plots displaying observations by month on spatial maps.
  
  plot_list <- list()
  
  for (month in MONTHS) {  
    
    month_data <- df_colera[df_colera$Fecha == month, ]
    
    plot <- ggplot() +
      geom_sf(data = mapS) +
      geom_sf(data = month_data, aes(col = !!sym(var_col), fill = !!sym(var_col))) +
      scale_color_manual(values = PLOT_COLORS_BY_LABELS) +
      scale_fill_manual(values = PLOT_COLORS_BY_LABELS) +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      labs(color = var_colname, fill = var_colname) +
      theme_void() +
      ggtitle(paste("month:", month))  # add month-specific title
    
    plot_list[[as.character(month)]] <- plot
    
  }
  
  return(plot_list)
  
}


generate_mapsByMonth <- function(data, numeric_col, month_col, months, palette) {
  #' Generate interactive maps for each specified month.
  #'
  #' This function creates a list of interactive maps, one for each specified month, based on the provided data.
  #'
  #' @param data The data frame containing the spatial data.
  #' @param numeric_col Name of the column with numeric data to be visualized.
  #' @param month_col Name of the column containing the month information.
  #' @param months Vector of months for which maps will be generated.
  #' @param palette Color palette for the map visualization.
  #'
  #' @return A list containing interactive maps for each specified month.
  
  map_list <- list()
  
  for (month in MONTHS) {
    
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
df_colera_inla7 <- df_colera_inla7[, !colnames(df_colera_inla7) %in% c(MUNICIPIO_STR)]
head(df_colera_inla7)


# correct incorrect "long" and "lat" values (TODO: delete after data correction)

df_colera_inla7 <- df_colera_inla7 %>%
  mutate(
    lat = ifelse(lat < 36.0, 36.0, ifelse(lat > 43.0, 43.0, lat)),
    long = ifelse(long < -10.0, -10.0, ifelse(long > 4.0, 4.0, long))
  )
head(df_colera_inla7)


# map ---------------------------------------------------------------------


mapS <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS <- subset(mapS, CODNUT1 != "ES7") # remove Canary Islands
mapS <- subset(mapS, !(CODIGOINE %in% c(51001, 52001))) # remove "Ceuta" and "Melilla"
head(mapS)


# observations ------------------------------------------------------------


# merge mapS and df_colera_inla7

df_colera_inla7 <- merge(mapS, df_colera_inla7, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(df_colera_inla7)


# factorize "Total_invasiones", "Total_defunciones", "Tasa_incidencia" and "Tasa_mortalidad"    

df_colera_inla7.copy <- df_colera_inla7

df_colera_inla7.copy <- factorize(
  df_colera_inla7.copy,
  TOTAL_INVASIONES_STR,
  INVASIONES_FACTOR_STR,
  c(-1, 10, 50, 90, Inf)
)

df_colera_inla7.copy <- factorize(
  df_colera_inla7.copy,
  TOTAL_DEFUNCIONES_STR,
  DEFUNCIONES_FACTOR_STR,
  c(-1, 10, 30, 100, Inf)
)

df_colera_inla7.copy <- factorize(
  df_colera_inla7.copy,
  TASA_INCIDENCIA_STR,
  TASA_INCIDENCIA_FACTOR_STR,
  c(-1, 0.5, 1, 2, Inf)
)

df_colera_inla7.copy <- factorize(
  df_colera_inla7.copy,
  TASA_MORTALIDAD_STR,
  TASA_MORTALIDAD_FACTOR_STR,
  c(-1, 0.5, 1, 2, Inf)
)

plot_observations(df_colera_inla7.copy, INVASIONES_FACTOR_STR, TOTAL_INVASIONES_STR)
plot_observations(df_colera_inla7.copy, DEFUNCIONES_FACTOR_STR, TOTAL_DEFUNCIONES_STR)
plot_observations(df_colera_inla7.copy, TASA_INCIDENCIA_FACTOR_STR, TASA_INCIDENCIA_STR)
plot_observations(df_colera_inla7.copy, TASA_MORTALIDAD_FACTOR_STR, TASA_MORTALIDAD_STR)


# for each month

plot_list.invasiones <- plot_observationsByMonth(df_colera_inla7.copy, INVASIONES_FACTOR_STR, TOTAL_INVASIONES_STR)
plot_list.defunciones <- plot_observationsByMonth(df_colera_inla7.copy, DEFUNCIONES_FACTOR_STR, TOTAL_DEFUNCIONES_STR)
plot_list.incidencia <- plot_observationsByMonth(df_colera_inla7.copy, TASA_INCIDENCIA_FACTOR_STR, TASA_INCIDENCIA_STR)
plot_list.mortalidad <- plot_observationsByMonth(df_colera_inla7.copy, TASA_MORTALIDAD_FACTOR_STR, TASA_MORTALIDAD_STR)


for (month in MONTHS) { # TODO: change type of plots ???
  # print(plot_list.invasiones[[month]])
  # print(plot_list.defunciones[[month]])
  # print(plot_list.incidencia[[month]])
  # print(plot_list.mortalidad[[month]])
  ggsave(paste0(COLERA_PLOTS_DIR, "/colera_total_invasionesXmunicipios_", month, ".png"), plot_list.invasiones[[month]])
  ggsave(paste0(COLERA_PLOTS_DIR, "/colera_total_defuncionesXmunicipios_", month, ".png"), plot_list.defunciones[[month]])
  ggsave(paste0(COLERA_PLOTS_DIR, "/colera_total_incidenciaXmunicipios_", month, ".png"), plot_list.incidencia[[month]])
  ggsave(paste0(COLERA_PLOTS_DIR, "/colera_total_mortalidadXmunicipios_", month, ".png"), plot_list.mortalidad[[month]])
  
}


# reference dataset -------------------------------------------------------


df_colera_ref <- df_colera.groupByProvinciaFecha
df_colera_ref$Fecha <- month(as.POSIXlt(df_colera_ref$Fecha, format = DATE_FORMAT))
df_colera_ref <- df_colera_ref %>%
  group_by(Provincia, Fecha) %>%
  summarize(
    Total_invasiones_Provincia = sum(Total_invasiones),
    Total_defunciones_Provincia = sum(Total_defunciones)
  )
head(df_colera_ref)

# merge reference dataset with df_colera_inla7

df_colera_inla7 <- merge(df_colera_inla7, df_colera_ref, by = c(PROVINCIA_STR, FECHA_STR))
head(df_colera_inla7)


# SIR and SMR -------------------------------------------------------------


# 1. "Tasa_incidencia" and "Tasa_mortalidad" by county

df_colera_inla7$Tasa_incidencia_Provincia <- (df_colera_inla7$Total_invasiones_Provincia / df_colera_inla7$Total_poblacion) * 100
df_colera_inla7$Tasa_mortalidad_Provincia <- (df_colera_inla7$Total_defunciones_Provincia / df_colera_inla7$Total_poblacion) * 100
df_colera_inla7 <- df_colera_inla7 %>%
  select(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 17, 13, 22, 14, 25, 15, 23, 16, 26, 18, 19, 20, 21, 24) %>%
  arrange(CODIGOINE, Fecha)

df_colera_inla7$SIR <-
  ifelse(
    df_colera_inla7$Tasa_incidencia_Provincia == 0,
    0,
    df_colera_inla7$Tasa_incidencia / df_colera_inla7$Tasa_incidencia_Provincia
  )

df_colera_inla7$SMR <-
  ifelse(
    df_colera_inla7$Tasa_mortalidad_Provincia == 0,
    0,
    df_colera_inla7$Tasa_mortalidad / df_colera_inla7$Tasa_mortalidad_Provincia
  )


# 2. total of "Tasa_incidencia" and "Tasa_mortalidad" 

tasa_incidencia_referencia <- (sum(df_colera_inla7$Total_invasiones) / sum(df_colera_inla7$Total_poblacion)) * 100
tasa_mortalidad_referencia <- (sum(df_colera_inla7$Total_defunciones) / sum(df_colera_inla7$Total_poblacion)) * 100

df_colera_inla7$SIR_global <- df_colera_inla7$Tasa_incidencia / tasa_incidencia_referencia
df_colera_inla7$SMR_global <- df_colera_inla7$Tasa_mortalidad / tasa_mortalidad_referencia


# plots

mapview(df_colera_inla7, zcol = SIR_STR, color = "gray", alpha.regions = 0.8,
        layer.name = SIR_STR, col.regions = PALETTE,
        map.types = "CartoDB.Positron")

mapview(df_colera_inla7, zcol = SMR_STR, color = "gray", alpha.regions = 0.8,
        layer.name = SMR_STR, col.regions = PALETTE,
        map.types = "CartoDB.Positron")


# for each month

multi_maps_SIR <-
  generate_mapsByMonth(
    data = df_colera_inla7,
    numeric_col = SIR_STR,
    month_col = Fecha,
    months = MONTHS,
    palette = PALETTE
  )

multi_maps_SIR
leafsync::sync(multi_maps_SIR)

multi_maps_SMR <-
  generate_mapsByMonth(
    data = df_colera_inla7,
    numeric_col = SMR_STR,
    month_col = Fecha,
    months = MONTHS,
    palette = PALETTE
  )

multi_maps_SMR
leafsync::sync(multi_maps_SMR)


# model -------------------------------------------------------------------


df_colera_inla7.sf <- st_as_sf(df_colera_inla7, crs = 4326)


# neighbourhood matrix

nb <- poly2nb(df_colera_inla7.sf)
head(nb)

nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")


# model formula and inla() call

df_colera_inla7.sf$re_u <- 1:nrow(df_colera_inla7.sf)
df_colera_inla7.sf$re_v <- 1:nrow(df_colera_inla7.sf)

formula_covtemp_covprec <- Tasa_mortalidad ~ covtemp + covprec +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

formula_covtemp <- Tasa_mortalidad ~ covtemp +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

cl <- makePSOCKcluster(4, setup_strategy = "sequential")
registerDoParallel(cl)

res <- inla(formula_covtemp_covprec, family = "gaussian", data = df_colera_inla7.sf, E = log(Tasa_mortalidad_Provincia),
            control.predictor = list(compute = TRUE),
            control.compute = list(), verbose = TRUE)

stopCluster(cl)


# results -----------------------------------------------------------------


res$summary.fixed
res$summary.fitted.values[1:3, ]

df_colera_inla7.sf$RR <- res$summary.fitted.values[, "mean"] # relative risk
df_colera_inla7.sf$LL <- res$summary.fitted.values[, "0.025quant"] # lower limit 95% CI
df_colera_inla7.sf$UL <- res$summary.fitted.values[, "0.975quant"] # upper limit 95% CI


# correlation and significance

cor_rr_covtemp <- cor(df_colera_inla7.sf$RR, df_colera_inla7.sf$covtemp)
cor_rr_covprec <- cor(df_colera_inla7.sf$RR, df_colera_inla7.sf$covprec)

significance_covtemp <- ifelse(cor_rr_covtemp > 0.05, "significativa", "no significativa")
cat("Correlation between predictions and", COVTEMP_STR, ":", cor_rr_covtemp, significance_covtemp, "\n")

significance_covprec <- ifelse(cor_rr_covprec > 0.05, "significativa", "no significativa")
cat("Correlation between predictions and", COVPREC_STR, ":", cor_rr_covprec, significance_covprec, "\n")


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
  generate_mapsByMonth(
    data = df_colera_inla7.sf,
    numeric_col = RR_STR,
    month_col = Fecha,
    months = MONTHS,
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
  generate_mapsByMonth(
    data = df_colera_inla7.sf,
    numeric_col = "exc",
    month_col = Fecha,
    months = MONTHS,
    palette = PALETTE
  )

multi_maps_exc
leafsync::sync(multi_maps_exc)
