# https://inla.r-inla-download.org/R/stable/bin/

# install.packages("INLA_21.02.23.tar", repos = NULL, type = "source")
# install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)


library(sf)
library(dplyr)
library(zoo)
library(leafpop)
library(INLA)
library(spdep)
library(doParallel)
library(lubridate)
library(tmap)


# load("colera_data.RData")
# load("temperatures.RData")
# load("rain.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
COLERA_MAPS_DIR <- "colera_maps"
dir.create(COLERA_MAPS_DIR, showWarnings = FALSE)

COVTEMP_STR <- "covtemp"
COVPREC_STR <- "covprec"
CODIGOINE_STR <- "CODIGOINE"
TOTAL_POBLACION_STR <- "Total_poblacion"
TOTAL_INVASIONES_STR <- paste("Total", INVASIONES_STR, sep = "_")
TOTAL_DEFUNCIONES_STR <- paste("Total", DEFUNCIONES_STR, sep = "_")
TASA_INCIDENCIA_STR <- "Tasa_incidencia"
TASA_MORTALIDAD_STR <- "Tasa_mortalidad"
LONG_STR <- "long"
LAT_STR <- "lat"
SIR_STR <- "SIR"
SMR_STR <- "SMR"

MONTHS_INT <- c(6, 7, 8, 9, 10, 11)
MONTHS_STR <- c("June", "July", "August", "September", "October", "November")
COEFFICIENTS <- c("mean", "0.025quant", "0.975quant")


# functions ---------------------------------------------------------------


create_tmap <- function(df_mes, mes, map, var_col, style, coords) {
  #' Create a thematic map using the tmap package.
  #'
  #' This function generates a thematic map using the tmap package. It allows you to visualize
  #' spatial data with various styles and legends.
  #'
  #' @param df_mes A data frame containing spatial data to be plotted.
  #' @param mes The label or title for the map panel.
  #' @param map A shapefile or spatial object used as the background map.
  #' @param var_col The column in the data frame to be used for coloring the map.
  #' @param style The style of coloring for the map.
  #' @param coords The bounding box coordinates (bbox) for the map view.
  #'
  #' @return A thematic map visualization.
  
  return(
    tm_shape(df_mes) +
      tm_polygons(
        col = var_col,
        border.col = NULL,
        title = "",
        palette = "Reds",
        style = style,
        legend.is.portrait = FALSE
      ) +
      tm_shape(map) + tm_borders() +
      tm_layout(
        legend.position =  c("right", "bottom"),
        inner.margins = c(0, 0, 0, 0),
        panel.labels = mes,
        panel.label.size = 1.5, panel.label.color = "black",
        panel.label.bg.color = "gray", panel.label.height = 1.1
      ) +
      tm_view(bbox = coords)
  )
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
  
  res <- inla(formula, family = distribution, data = df_colera, E = expected_cases, control.predictor = list(compute = TRUE), control.compute = list())
  return(res)
  
}


# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


df_covdist <- df_distances[, c(1, 3:6)]
colnames(df_covdist) <- c(CODIGO_INE_STR, PROVINCIA_STR, MUNICIPIO_STR, LONG_STR, LAT_STR)
df_colera.merged.month <- df_colera.merged.month[, c(1, 4:9)]
df_colera.merged.month$`Codigo Ine` <- as.numeric(df_colera.merged.month$`Codigo Ine`)
head(df_covdist)
head(df_colera.merged.month)


# create df_covtemp and df_covprec

df_covtemp <- subset(df_temperatures.parsed, !(localidad %in% c("Alicante (M.)", "Zaragoza (E.P.)")))
df_covprec <- subset(df_rain.parsed, !(localidad %in% c("Alicante (M.)", "Zaragoza (E.P.)")))
df_covtemp <- df_covtemp[, c(4:6)]
df_covprec <- df_covprec[, c(4:6)]
colnames(df_covtemp)[1:3] <- c(CODIGO_INE_STR, FECHA_STR, COVTEMP_STR)
colnames(df_covprec)[1:3] <- c(CODIGO_INE_STR, FECHA_STR, COVPREC_STR)
head(df_covtemp)
head(df_covprec)


# merge df_covtemp and df_covprec with df_colera.merged.month as df_colera_inla7

df_environmental <- merge(df_covtemp, df_covprec, by = c(CODIGO_INE_STR, FECHA_STR))
df_environmental <- merge(df_covdist, df_environmental, by = CODIGO_INE_STR)
head(df_environmental)

df_colera_inla7 <- merge(df_colera.merged.month, df_environmental, by = c(CODIGO_INE_STR, FECHA_STR))
df_colera_inla7 <- df_colera_inla7[, c(1, 8:9, 2, 3:7, 12:13, 10:11)]
head(df_colera_inla7)


# clean environment -------------------------------------------------------


rm(df_distances, df_covdist, df_temperatures.parsed, df_rain.parsed, df_covtemp, df_covprec, df_environmental)


# map and data ------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7") # remove Canary Islands
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) # remove "Ceuta" and "Melilla"
head(mapS.municipios)


# merge mapS.municipios and df_colera_inla7

df_colera_inla7 <- merge(mapS.municipios, df_colera_inla7, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(df_colera_inla7)


# Observed cases ----------------------------------------------------------


for (month in MONTHS_INT) {
  
  map_invasiones <-
    create_tmap(
      df_colera_inla7[df_colera_inla7$Fecha == month,],
      c(MONTHS_STR[month-5]), 
      mapS.municipios, 
      TOTAL_INVASIONES_STR, 
      "jenks",
      c(min(df_colera_inla7$long), min(df_colera_inla7$lat), max(df_colera_inla7$long), max(df_colera_inla7$lat)))
  
  map_defunciones <-
    create_tmap(
      df_colera_inla7[df_colera_inla7$Fecha == month,],
      c(MONTHS_STR[month-5]),
      mapS.municipios,
      TOTAL_DEFUNCIONES_STR,
      "jenks",
      c(min(df_colera_inla7$long), min(df_colera_inla7$lat), max(df_colera_inla7$long), max(df_colera_inla7$lat)))
  
  map_incidencia <-
    create_tmap(
      df_colera_inla7[df_colera_inla7$Fecha == month,],
      c(MONTHS_STR[month-5]),
      mapS.municipios,
      TASA_INCIDENCIA_STR,
      "jenks",
      c(min(df_colera_inla7$long), min(df_colera_inla7$lat), max(df_colera_inla7$long), max(df_colera_inla7$lat)))
  
  map_mortalidad <-
    create_tmap(
      df_colera_inla7[df_colera_inla7$Fecha == month,],
      c(MONTHS_STR[month-5]),
      mapS.municipios,
      TASA_MORTALIDAD_STR,
      "jenks",
      c(min(df_colera_inla7$long), min(df_colera_inla7$lat), max(df_colera_inla7$long), max(df_colera_inla7$lat)))
  
  tmap_save(map_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.invasiones", month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.defunciones", month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_incidencia, filename = paste(COLERA_MAPS_DIR, paste0("tmap.incidencia", month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_mortalidad, filename = paste(COLERA_MAPS_DIR, paste0("tmap.mortalidad", month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  
}


# Expected cases and deaths ---------------------------------------------


df_colera_ref <- df_colera_inla7[, c(PROVINCIA_STR, TOTAL_POBLACION_STR)]
df_colera_ref$geometry <- NULL
df_colera_ref <- subset(df_colera_ref, !duplicated(df_colera_ref))
df_colera_ref <- df_colera_ref %>%
  group_by(Provincia) %>% summarize(Total_poblacion_Provincia = sum(Total_poblacion))


# merge reference dataset with df_colera_inla7

df_colera_inla7 <- left_join(df_colera_inla7, df_colera_ref, by = PROVINCIA_STR)
df_colera_inla7 <- df_colera_inla7[, c(1:18, 28, 19:27, 29)]
head(df_colera_inla7)

df_colera_inla7$Total_invasiones_Esperadas <- (df_colera_inla7$Total_invasiones / df_colera_inla7$Total_poblacion_Provincia) * df_colera_inla7$Total_poblacion
df_colera_inla7$Total_defunciones_Esperadas <- (df_colera_inla7$Total_defunciones / df_colera_inla7$Total_poblacion_Provincia) * df_colera_inla7$Total_poblacion
df_colera_inla7 <- df_colera_inla7[, c(1:14, 30, 15:16, 31, 17:29)]
head(df_colera_inla7)


# SIR and SMR -------------------------------------------------------------


df_colera_inla7$SIR <- 
  with(
    df_colera_inla7,
    ifelse(
      Total_invasiones == 0 | Total_invasiones_Esperadas == 0, 0,
      Total_invasiones / Total_invasiones_Esperadas
    )
  )
df_colera_inla7$SMR <-
  with(
    df_colera_inla7,
    ifelse(
      Total_defunciones == 0 | Total_defunciones_Esperadas == 0, 0,
      Total_defunciones / Total_defunciones_Esperadas
    )
  )

for (month in MONTHS_INT) {
  
  map_SIR <-
    create_tmap(
      df_colera_inla7[df_colera_inla7$Fecha == month,],
      c(MONTHS_STR[month-5]),
      mapS.municipios, 
      SIR_STR, 
      "jenks",
      c(min(df_colera_inla7$long), min(df_colera_inla7$lat), max(df_colera_inla7$long), max(df_colera_inla7$lat))
    ) 
  
  map_SMR <-
    create_tmap(
      df_colera_inla7[df_colera_inla7$Fecha == month,],
      c(MONTHS_STR[month-5]),
      mapS.municipios, 
      SMR_STR, 
      "jenks",
      c(min(df_colera_inla7$long), min(df_colera_inla7$lat), max(df_colera_inla7$long), max(df_colera_inla7$lat))
    ) 
  
  tmap_save(map_SIR, filename = paste(COLERA_MAPS_DIR, paste0("tmap.", SIR_STR, month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_SMR, filename = paste(COLERA_MAPS_DIR, paste0("tmap.", SMR_STR, month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  
}


# clean environment -------------------------------------------------------


rm(map_invasiones, map_defunciones, map_incidencia, map_mortalidad, map_SIR, map_SMR, df_colera_ref)


# modelling ---------------------------------------------------------------


# neighbourhood matrix

nb <- poly2nb(df_colera_inla7)
head(nb)

nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")


# inference using INLA

df_colera_inla7$idarea <- as.numeric(as.factor(df_colera_inla7$CODIGOINE))
df_colera_inla7$idarea1 <- df_colera_inla7$idarea
df_colera_inla7$idtime <- 1 + df_colera_inla7$Fecha - min(df_colera_inla7$Fecha)


# formulas of the Bernardinelli model

formula_all.invasiones <- Total_invasiones ~ covtemp + covprec +
  f(idarea, model = "bym", graph = g) + f(idarea1, idtime, model = "iid") + idtime

formula_all.defunciones <- Total_defunciones ~ covtemp + covprec +
  f(idarea, model = "bym", graph = g) + f(idarea1, idtime, model = "iid") + idtime


# inla() call

cl <- makePSOCKcluster(4, setup_strategy = "sequential")
registerDoParallel(cl)

res_all.invasiones <- run_inla("poisson", formula_all.invasiones, df_colera_inla7, paste0(TOTAL_INVASIONES_STR, "_Esperadas"))

res_all.defunciones <- run_inla("poisson", formula_all.defunciones, df_colera_inla7, paste0(TOTAL_DEFUNCIONES_STR, "_Esperadas"))

stopCluster(cl)


df_colera_inla7$idarea <- NULL
df_colera_inla7$idarea1 <- NULL
df_colera_inla7$idtime <- NULL


# results -----------------------------------------------------------------


# relative risks 

res_all.invasiones$summary.fixed
res_all.defunciones$summary.fixed

df_colera_inla7$RR_invasiones <- res_all.invasiones$summary.fitted.values[, COEFFICIENTS[1]]
df_colera_inla7$LL_invasiones <- res_all.invasiones$summary.fitted.values[, COEFFICIENTS[2]]
df_colera_inla7$UL_invasiones <- res_all.invasiones$summary.fitted.values[, COEFFICIENTS[3]]
df_colera_inla7$RR_defunciones <- res_all.defunciones$summary.fitted.values[, COEFFICIENTS[1]]
df_colera_inla7$LL_defunciones <- res_all.defunciones$summary.fitted.values[, COEFFICIENTS[2]]
df_colera_inla7$UL_defunciones <- res_all.defunciones$summary.fitted.values[, COEFFICIENTS[2]]


# save coefficients and RRS in res_table

res_table <- data.frame(matrix(ncol = 5, nrow = 4))
colnames(res_table) <- c("Covariates", "Coefficient (95% CrI)", "RR (95% CrI)", "Coefficient (95% CrI)", "RR (95% CrI)")


# add covariates names

res_table[1,1] <- "Intercept*"
res_table[2,1] <- COVTEMP_STR
res_table[3,1] <- COVPREC_STR
res_table[4,1] <- "Unstructured random effect"

covariates <- res_table$Covariates


# add coefficients and RRs for "Total_invasiones"

for (i in 1:4) {
  res_table[i, 2] <- paste0(
    sprintf("%.7f", res_all.invasiones$summary.fixed[, COEFFICIENTS[1]][i]),
    " (",
    sprintf("%.7f", res_all.invasiones$summary.fixed[, COEFFICIENTS[2]][i]),
    ", ",
    sprintf("%.7f", res_all.invasiones$summary.fixed[, COEFFICIENTS[3]][i]),
    ")"
  )
  res_table[i, 3] <- paste0(
    sprintf("%.7f", exp(res_all.invasiones$summary.fixed[, COEFFICIENTS[1]][i])),
    " (",
    sprintf("%.7f", exp(res_all.invasiones$summary.fixed[, COEFFICIENTS[2]][i])),
    ", ",
    sprintf("%.7f", exp(res_all.invasiones$summary.fixed[, COEFFICIENTS[3]][i])),
    ")"
  )
  if (res_all.invasiones$summary.fixed[, COEFFICIENTS[2]][i] < 0 && res_all.invasiones$summary.fixed[, COEFFICIENTS[3]][i] > 0) {
    cat("The confidence interval of", covariates[i], "goes through the value 0.\n")
  } else {
    cat("The confidence interval of", covariates[i], "it doesn't go through the value 0.\n")
  }
}


# add coefficients and RRs for "Total_defunciones"

for (i in 1:4) {
  res_table[i, 4] <- paste0(
    sprintf("%.7f", res_all.defunciones$summary.fixed[, COEFFICIENTS[1]][i]),
    " (",
    sprintf("%.7f", res_all.defunciones$summary.fixed[, COEFFICIENTS[2]][i]),
    ", ",
    sprintf("%.7f", res_all.defunciones$summary.fixed[, COEFFICIENTS[3]][i]),
    ")"
  )
  res_table[i, 5] <- paste0(
    sprintf("%.7f", exp(res_all.defunciones$summary.fixed[, COEFFICIENTS[1]][i])),
    " (",
    sprintf("%.7f", exp(res_all.defunciones$summary.fixed[, COEFFICIENTS[2]][i])),
    ", ",
    sprintf("%.7f", exp(res_all.defunciones$summary.fixed[, COEFFICIENTS[3]][i])),
    ")"
  )
  if (res_all.defunciones$summary.fixed[, COEFFICIENTS[2]][i] < 0 && res_all.defunciones$summary.fixed[, COEFFICIENTS[3]][i] > 0) {
    cat("The confidence interval of", covariates[i], "goes through the value 0.\n")
  } else {
    cat("The confidence interval of", covariates[i], "it doesn't go through the value 0.\n")
  }
}

# TODO: RR, LL and UL plots
