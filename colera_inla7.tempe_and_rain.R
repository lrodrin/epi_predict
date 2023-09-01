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


run_inla <- function(distribution, formula, df_colera, var_col) {
  #' Run an INLA model for regression analysis.
  #'
  #' This function runs an Integrated Nested Laplace Approximations (INLA) model for regression analysis.
  #' It uses the specified distribution, formula, data frame, and variable column to fit the model.
  #'
  #' @param distribution Distribution family for the model (e.g., "gaussian", "poisson").
  #' @param formula Model formula.
  #' @param df_colera Data frame containing the observations.
  #' @param var_col Column with numeric values for regression.
  #'
  #' @return The result of the INLA model fitting.
  
  if (distribution == "gaussian") {
    
    expected_cases <- log(df_colera[[var_col]])
    
  } else if (distribution == "poisson") {
    
    expected_cases <- df_colera[[var_col]]
  }
  
  res <- inla(formula, family = distribution, data = df_colera, E = expected_cases,
              control.predictor = list(compute = TRUE),
              control.compute = list(), verbose = TRUE)
  
  return(res)
  
}


results_inla <- function(df_colera, res) {
  #' Extract and process results from the INLA model.
  #'
  #' This function extracts and processes the results from the INLA model and adds relevant
  #' summary statistics to the provided data frame.
  #'
  #' @param df_colera Data frame containing the observations.
  #' @param res Result object obtained from the INLA model.
  #'
  #' @return The data frame with added relative risk and confidence interval columns.
  
  print(res$summary.fixed)
  # print(res$summary.fitted.values[1:3, ])
  
  df_colera$RR <- res$summary.fitted.values[, "mean"] # relative risk
  df_colera$LL <- res$summary.fitted.values[, "0.025quant"] # lower limit 95% CI
  df_colera$UL <- res$summary.fitted.values[, "0.975quant"] # upper limit 95% CI
  
  return(df_colera)
  
}


calculate_correlation <- function(df_res, covariate) {
  #' Calculate the correlation between relative risk and a covariate.
  #'
  #' This function calculates the Pearson correlation coefficient between the relative risk (RR) and a specified covariate.
  #'
  #' @param df_res Data frame containing the result obtained from the INLA model.
  #' @param covariate Name of the covariate column.
  #'
  #' @return The calculated correlation coefficient.
  
  cor_rr <- cor(df_res$RR, df_res[[covariate]])
  return(cor_rr)
  
}


calculate_significance <- function(cor_rr, covariate) {
  #' Determine the significance of correlation.
  #'
  #' This function determines the significance of a correlation coefficient by comparing it to a threshold.
  #'
  #' @param cor_rr Correlation coefficient value.
  #' @param covariate Name of the covariate.
  
  significance <- ifelse(cor_rr > 0.05, "significativa", "no significativa")
  cat("Correlation between predictions and", covariate, ":", cor_rr, significance, "\n")
  
}


# main --------------------------------------------------------------------


# raw data ----------------------------------------------------------------


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


for (month in names(plot_list.invasiones)) { # TODO: change type of plots ???
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


# 1. 
# "Tasa_incidencia" and "Tasa_mortalidad" by county

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


# 2. 
# total of "Tasa_incidencia" and "Tasa_mortalidad" 

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


# with ggplot2

ggplot() + 
  geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla7, aes(fill = SIR)) +
  facet_wrap(~Fecha, dir = "h", ncol = 3) +
  ggtitle(SIR_STR) + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 0.5, low = "blue", mid = "white", high = "red"
  )

ggplot() + 
  geom_sf(data = mapS) +
  geom_sf(data = df_colera_inla7, aes(fill = SMR)) +
  facet_wrap(~Fecha, dir = "h", ncol = 3) +
  ggtitle(SMR_STR) + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 0.5, low = "blue", mid = "white", high = "red"
  )

ggplot(df_colera_inla7, aes(x = Fecha, y = SIR, group = CODIGOINE, color = CODIGOINE)) +
  geom_line() + geom_point(size = 2) + theme_bw()

ggplot(df_colera_inla7, aes(x = Fecha, y = SMR, group = CODIGOINE, color = CODIGOINE)) +
  geom_line() + geom_point(size = 2) + theme_bw()


# model -------------------------------------------------------------------


# neighbourhood matrix

nb <- poly2nb(df_colera_inla7)
head(nb)

nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")


# model formula 

# 1.
df_colera_inla7$re_u <- 1:nrow(df_colera_inla7)
df_colera_inla7$re_v <- 1:nrow(df_colera_inla7)

# 2.
df_colera_inla7$idarea <- as.numeric(as.factor(df_colera_inla7$CODIGOINE))
df_colera_inla7$idarea1 <- df_colera_inla7$idarea
df_colera_inla7$idtime <- 1 + df_colera_inla7$Fecha - min(df_colera_inla7$Fecha)

# 1.
formula_covtemp_covprec.invasiones <- Total_invasiones ~ covtemp + covprec +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

formula_covprec.invasiones <- Total_invasiones ~ covprec +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

formula_covtemp_covprec.defunciones <- Total_defunciones ~ covtemp + covprec +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

formula_covprec.defunciones <- Total_defunciones ~ covprec +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

formula_covtemp_covprec.incidencia <- Tasa_incidencia ~ covtemp + covprec +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

formula_covtemp.incidencia <- Tasa_incidencia ~ covtemp +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

formula_covtemp_covprec.mortalidad <- Tasa_mortalidad ~ covtemp + covprec +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

formula_covtemp.mortalidad <- Tasa_mortalidad ~ covtemp +
  f(re_u, model = "besag", graph = g, scale.model = TRUE) +
  f(re_v, model = "iid")

# 2. taking to account "Fecha"
formula_covtemp_covprec.invasiones <- Total_invasiones ~ covtemp + covprec +
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

formula_covprec.invasiones <- Total_invasiones ~ covprec +
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

formula_covtemp_covprec.defunciones <- Total_defunciones ~ covtemp + covprec +
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

formula_covprec.defunciones <- Total_defunciones ~ covprec +
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

formula_covtemp_covprec.incidencia <- Tasa_incidencia ~ covtemp + covprec +
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

formula_covtemp.incidencia <- Tasa_incidencia ~ covtemp +
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

formula_covtemp_covprec.mortalidad <- Tasa_mortalidad ~ covtemp + covprec +
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime

formula_covtemp.mortalidad <- Tasa_mortalidad ~ covtemp +
  f(idarea, model = "bym", graph = g) +
  f(idarea1, idtime, model = "iid") + idtime


# inla() call

cl <- makePSOCKcluster(4, setup_strategy = "sequential")
registerDoParallel(cl)

res_covtemp_covprec.invasiones <- run_inla("poisson", formula_covtemp_covprec.invasiones, df_colera_inla7, paste0(TOTAL_INVASIONES_STR, "_", PROVINCIA_STR))

res_covprec.invasiones <- run_inla("gaussian", formula_covprec.invasiones, df_colera_inla7, paste0(TASA_INCIDENCIA_STR, "_", PROVINCIA_STR))

res_covtemp_covprec.defunciones <- run_inla("poisson", formula_covtemp_covprec.defunciones, df_colera_inla7, paste0(TOTAL_DEFUNCIONES_STR, "_", PROVINCIA_STR))

res_covprec.defunciones <- run_inla("gaussian", formula_covprec.defunciones, df_colera_inla7, paste0(TASA_MORTALIDAD_STR, "_", PROVINCIA_STR))

res_covtemp_covprec.incidencia <- run_inla("gaussian", formula_covtemp_covprec.incidencia, df_colera_inla7, paste0(TASA_INCIDENCIA_STR, "_", PROVINCIA_STR))

res_covtemp.incidencia <- run_inla("gaussian", formula_covtemp.incidencia, df_colera_inla7, paste0(TASA_INCIDENCIA_STR, "_", PROVINCIA_STR))

res_covtemp_covprec.mortalildad <- run_inla("gaussian", formula_covtemp_covprec.mortalidad, df_colera_inla7, paste0(TASA_MORTALIDAD_STR, "_", PROVINCIA_STR))

res_covtemp.mortalildad <- run_inla("gaussian", formula_covtemp.mortalidad, df_colera_inla7, paste0(TASA_MORTALIDAD_STR, "_", PROVINCIA_STR))

stopCluster(cl)


# results -----------------------------------------------------------------


df_res_covtemp_covprec.invasiones <- results_inla(df_colera_inla7, res_covtemp_covprec.invasiones)
df_res_covprec.invasiones <- results_inla(df_colera_inla7, res_covprec.invasiones)
df_res_covtemp_covprec.defunciones <- results_inla(df_colera_inla7, res_covtemp_covprec.defunciones)
df_res_covprec.defunciones <- results_inla(df_colera_inla7, res_covprec.defunciones)
df_res_covtemp_covprec.incidencia <- results_inla(df_colera_inla7, res_covtemp_covprec.incidencia)
df_res_covtemp.incidencia <- results_inla(df_colera_inla7, res_covtemp.incidencia)
df_res_covtemp_covprec.mortalildad <- results_inla(df_colera_inla7, res_covtemp_covprec.mortalildad)
df_res_covtemp.mortalildad <- results_inla(df_colera_inla7, res_covtemp.mortalildad)


# correlation and significance 

cor_rr_covtemp.invasiones <- calculate_correlation(df_res_covtemp_covprec.invasiones, COVTEMP_STR)
cor_rr_covprec.invasiones <- calculate_correlation(df_res_covtemp_covprec.invasiones, COVPREC_STR)
cor_rr_covtemp.defunciones <- calculate_correlation(df_res_covtemp_covprec.defunciones, COVTEMP_STR)
cor_rr_covprec.defunciones <- calculate_correlation(df_res_covtemp_covprec.defunciones, COVPREC_STR)
cor_rr_covtemp.incidencia <- calculate_correlation(df_res_covtemp_covprec.incidencia, COVTEMP_STR)
cor_rr_covprec.incidencia <- calculate_correlation(df_res_covtemp_covprec.incidencia, COVPREC_STR)
cor_rr_covtemp.mortalildad <- calculate_correlation(df_res_covtemp_covprec.mortalildad, COVTEMP_STR)
cor_rr_covprec.mortalildad <- calculate_correlation(df_res_covtemp_covprec.mortalildad, COVPREC_STR)

calculate_significance(cor_rr_covtemp.invasiones, COVTEMP_STR)
calculate_significance(cor_rr_covprec.invasiones, COVPREC_STR) # yes(2)
calculate_significance(cor_rr_covtemp.defunciones, COVTEMP_STR)
calculate_significance(cor_rr_covprec.defunciones, COVPREC_STR) # yes(2)
calculate_significance(cor_rr_covtemp.incidencia, COVTEMP_STR) # yes(1,2)
calculate_significance(cor_rr_covprec.incidencia, COVPREC_STR)
calculate_significance(cor_rr_covtemp.mortalildad, COVTEMP_STR) # yes(1,2)
calculate_significance(cor_rr_covprec.mortalildad, COVPREC_STR)

# TODO: more than one covariate at the same time ???


# cholera risk ------------------------------------------------------------


visualize_choleraRisk <- function(df_res, isSMR) {
  #' Visualize cholera risk using interactive maps.
  #'
  #' This function generates interactive maps to visualize cholera risk based on the provided data frame.
  #'
  #' @param df_res Data frame containing cholera risk data.
  #' @param isSMR Logical indicating if SMR (Standardized Mortality Ratio) should be used. The other option is SIR (Standardized Incidence Ratio).
  
  print(mapview(df_res, zcol = RR_STR, color = "gray", alpha.regions = 0.8,
                layer.name = RR_STR, col.regions = PALETTE,
                map.types = "CartoDB.Positron"))
  
  Sys.sleep(1)  
  
  if (isSMR) {
    
    at <- seq(min(df_res$SMR), max(df_res$SMR), length.out = 7)
    m1 <- mapview(df_res, zcol = SMR_STR, color = "gray",
                  col.regions = PALETTE, at = at, layer.name = SMR_STR, map.types = "CartoDB.Positron")
  
  } else { # SIR
    
    at <- seq(min(df_res$SIR), max(df_res$SIR), length.out = 7)
    m1 <- mapview(df_res, zcol = SIR_STR, color = "gray",
                  col.regions = PALETTE, at = at, layer.name = SIR_STR, map.types = "CartoDB.Positron")
  
  }
  
  m2 <- mapview(df_res, zcol = RR_STR, color = "gray",
                col.regions = PALETTE, at = at, layer.name = RR_STR, map.types = "CartoDB.Positron")
  
  print(leafsync::sync(m1, m2))
  
  Sys.sleep(1)

  multi_maps_RR <-
    generate_mapsByMonth(
      data = df_res,
      numeric_col = RR_STR,
      month_col = Fecha,
      months = MONTHS,
      palette = PALETTE
    )

  print(multi_maps_RR)
  print(leafsync::sync(multi_maps_RR))
  
}

visualize_choleraRisk(df_res_covprec.invasiones, FALSE)
visualize_choleraRisk(df_res_covprec.defunciones, TRUE)
visualize_choleraRisk(df_res_covtemp.incidencia, FALSE)
visualize_choleraRisk(df_res_covtemp.mortalildad, TRUE)


# with ggplot2

ggplot() +
  geom_sf(data = mapS) +
  geom_sf(data = df_res_covprec.invasiones, aes(fill = RR)) +
  facet_wrap(~Fecha, dir = "h", ncol = 3) +
  ggtitle(RR_STR) + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 198, low = "blue", mid = "white", high = "red"
  )

ggplot() +
  geom_sf(data = mapS) +
  geom_sf(data = df_res_covprec.defunciones, aes(fill = RR)) +
  facet_wrap(~Fecha, dir = "h", ncol = 3) +
  ggtitle(RR_STR) + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 86, low = "blue", mid = "white", high = "red"
  )

ggplot() +
  geom_sf(data = mapS) +
  geom_sf(data = df_res_covtemp.incidencia, aes(fill = RR)) +
  facet_wrap(~Fecha, dir = "h", ncol = 3) +
  ggtitle(RR_STR) + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 0.46, low = "blue", mid = "white", high = "red"
  )

ggplot() +
  geom_sf(data = mapS) +
  geom_sf(data = df_res_covtemp.mortalildad, aes(fill = RR)) +
  facet_wrap(~Fecha, dir = "h", ncol = 3) +
  ggtitle(RR_STR) + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 0.18, low = "blue", mid = "white", high = "red"
  )

# TODO: add legend limits


# exceed probabilities ------------------------------------------------


visualize_choleraExceedProb <- function(df_res, res, c) {
  #' Visualize Cholera Exceed Probabilities Using Interactive Maps
  #'
  #' This function generates interactive maps to visualize cholera exceed probabilities based on the provided data frame.
  #'
  #' @param df_res Data frame containing cholera exceed probabilities data.
  #' @param res Result object obtained from the INLA model.
  #' @param c Cut-off point for determining whether relative risk exceeds probabilities.
  #'
  #' @return A series of interactive maps displaying cholera exceed probabilities.

  df_res$exc <- sapply(res$marginals.fitted.values, FUN = function(marg){1 - inla.pmarginal(q = c, marginal = marg)})

  print(mapview(df_res, zcol = "exc", color = "gray", 
                layer.name = "exc", col.regions = PALETTE,
                map.types = "CartoDB.Positron"))
  
  Sys.sleep(1)

  multi_maps_exc <-
    generate_mapsByMonth(
      data = df_res,
      numeric_col = "exc",
      month_col = Fecha,
      months = MONTHS,
      palette = PALETTE
    )

  print(multi_maps_exc)
  print(leafsync::sync(multi_maps_exc))
  
}

# values above the mean

visualize_choleraExceedProb(df_res_covprec.invasiones, res_covprec.invasiones, 0.9)
visualize_choleraExceedProb(df_res_covprec.defunciones, res_covprec.defunciones, 0.9)
visualize_choleraExceedProb(df_res_covtemp.incidencia, res_covtemp.incidencia, 0.03)
# visualize_choleraExceedProb(df_res_covtemp.mortalildad, res_covtemp.mortalildad, 0.000002) # very small
