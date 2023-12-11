library(sf)
library(dplyr)
library(zoo)
library(leafpop)
library(INLA)
library(spdep)
library(lubridate)
library(tmap)
library(openxlsx)
library(Dict)
library(stringi)


load("colera_data.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
COLERA_MAPS_DIR <- "colera_maps"
dir.create(COLERA_MAPS_DIR, showWarnings = FALSE)
COLERA_INLA_DIR <- "colera_inla"
dir.create(COLERA_INLA_DIR, showWarnings = FALSE)

COVTEMP_STR <- "covtemp"
COVPREC_STR <- "covprec"
COVALL_STR <- c(COVTEMP_STR, COVPREC_STR)
MONTHS_INT <- c(6, 7, 8, 9, 10, 11)
MONTHS_STR <- c("June", "July", "August", "September", "October", "November")
COEFFICIENTS <- c("mean", "0.025quant", "0.975quant")


# functions ---------------------------------------------------------------


create_tmap <- function(df_mes, mes, map, var_col, style) {
  #' Create a thematic map using the tmap package.
  #'
  #' This function generates a thematic map using the tmap package. It allows you to visualize
  #' spatial data with various styles and legends.
  #'
  #' @param df_mes A data frame containing spatial data to be plotted.
  #' @param mes The label or title for the map panel.
  #' @param map A shape file or spatial object used as the background map.
  #' @param var_col The column in the data frame to be used for colouring the map.
  #' @param style The style of colouring for the map.
  #'
  #' @return A thematic map visualization.
  
  map.tmp <- tm_shape(df_mes, bbox = map) +
    tm_polygons(
      col = var_col,
      border.col = NULL,
      title = "",
      palette = "Reds",
      style = style
    ) +
    tm_shape(map) + tm_borders() +
    tm_layout(
      legend.position =  c("right", "bottom"), legend.text.size = 1.5,
      inner.margins = c(0, 0, 0, 0),
      panel.label.size = 1.5, panel.label.height = 1.1, 
      panel.label.color = "black", panel.label.bg.color = "gray"
    )
  
  if (!is.null(mes)) { map.tmp <- map.tmp + tm_layout(panel.labels = mes) }
  return(map.tmp)
}


run_inla <- function(mapS, province, covariate) {
  #' Run INLA Analysis for Disease Data
  #'
  #' This function runs an Integrated Nested Laplace Approximation (INLA) analysis for disease data.
  #'
  #' @param mapS A spatial map data frame.
  #' @param province A character vector specifying the province for the analysis. If left blank ("") or NULL, the analysis is performed on the entire dataset.
  #' @param covariate A character vector specifying the covariate to include in the analysis.
  #'
  #' @return A list of results including INLA analysis results for "invasiones" and "defunciones".
  
  # create a neighbor list by province
  if (province != "") { mapS.tmp <- mapS_byProvincias(mapS, province) }
  else { mapS.tmp <- mapS}
  nb <- poly2nb(mapS.tmp)
  head(nb)
  
  # create a graph
  nb2INLA("map.adj", nb)
  g <- inla.read.graph(filename = "map.adj")
  
  # assign IDs
  mapS.tmp$idarea <- as.numeric(as.factor(mapS.tmp$CODIGOINE))
  mapS.tmp$idarea1 <- mapS.tmp$idarea
  mapS.tmp$idtime <- 1 + mapS.tmp$Fecha - min(mapS.tmp$Fecha)
  
  # define the formula
  covariates <- c(covariate, "f(idarea, model = 'bym', graph = g)", "f(idarea1, idtime, model = 'iid')", "idtime")
  formula_invasiones <- as.formula(paste(paste(TOTAL_INVASIONES_STR, "~"), paste(covariates, collapse = " + ")))
  formula_defunciones <- as.formula(paste(paste(TOTAL_DEFUNCIONES_STR, "~"), paste(covariates, collapse = " + ")))
  print(formula_invasiones)
  print(formula_defunciones)
  
  # run the INLA analysis
  res_invasiones <- inla(formula_invasiones, family = "poisson", data = mapS.tmp, offset = log(Total_poblacion), control.predictor = list(compute = TRUE), verbose = TRUE)
  res_defunciones <- inla(formula_defunciones, family = "poisson", data = mapS.tmp, offset = log(Total_poblacion), control.predictor = list(compute = TRUE), verbose = TRUE)
  return(dict(invasiones = res_invasiones, defunciones = res_defunciones, .class = "any", .overwrite = TRUE))
}


create_empty.table <- function() {
  #' Create an Empty Table for Coefficients and RR (Risk Ratios)
  #'
  #' This function creates an empty table for storing coefficients and risk ratios (RR) with their corresponding 95% credible intervals.
  #'
  #' @return A data frame representing the empty table with column names and structure.
  
  columnames <- c("Covariates", "Coefficient (95% CrI)", "RR (95% CrI)", "Coefficient (95% CrI)", "RR (95% CrI)")
  
  table <- data.frame(matrix(ncol = 5, nrow = 4))
  colnames(table) <- columnames
  table[1,1] <- "Intercept*"
  table[2,1] <- COVTEMP_STR
  table[3,1] <- COVPREC_STR
  table[4,1] <- "Unstructured random effect"
  
  return(table)
}


add_results.table <- function(res_invasiones, res_defunciones, res_table) {
  #' Add INLA Results to Coefficients and RR Table
  #'
  #' This function adds the results of INLA analyses for invasions and defunciones to a coefficients and risk ratios (RR) table.
  #'
  #' @param res_invasiones The result object for invasions from the INLA analysis.
  #' @param res_defunciones The result object for defunciones from the INLA analysis.
  #' @param res_table The coefficients and RR table to which the results will be added.
  #' 
  #' @return The updated coefficients and RR table with INLA results.
    
  for (i in 1:4) {
    
    res_table[i, 2] <- paste0(
      sprintf("%.7f", res_invasiones$summary.fixed[, COEFFICIENTS[1]][i]), " (",
      sprintf("%.7f", res_invasiones$summary.fixed[, COEFFICIENTS[2]][i]), ", ",
      sprintf("%.7f", res_invasiones$summary.fixed[, COEFFICIENTS[3]][i]), ")"
    ) 
    res_table[i, 4] <- paste0(
      sprintf("%.7f", res_defunciones$summary.fixed[, COEFFICIENTS[1]][i]), " (",
      sprintf("%.7f", res_defunciones$summary.fixed[, COEFFICIENTS[2]][i]), ", ",
      sprintf("%.7f", res_defunciones$summary.fixed[, COEFFICIENTS[3]][i]), ")"
    )
    
    if (!(i %in% c(1, 4))) {
      res_table[i, 3] <- paste0(
        sprintf("%.7f", exp(res_invasiones$summary.fixed[, COEFFICIENTS[1]][i])), " (",
        sprintf("%.7f", exp(res_invasiones$summary.fixed[, COEFFICIENTS[2]][i])), ", ",
        sprintf("%.7f", exp(res_invasiones$summary.fixed[, COEFFICIENTS[3]][i])), ")"
      ) 
      res_table[i, 5] <- paste0(
        sprintf("%.7f", exp(res_defunciones$summary.fixed[, COEFFICIENTS[1]][i])), " (",
        sprintf("%.7f", exp(res_defunciones$summary.fixed[, COEFFICIENTS[2]][i])), ", ",
        sprintf("%.7f", exp(res_defunciones$summary.fixed[, COEFFICIENTS[3]][i])), ")"
      )
    }
  }
  return(res_table)
}


add_results_excel <- function(res_invasiones, res_defunciones, res_table, filename) {
  #' Add INLA Results to an Excel File
  #'
  #' This function takes INLA results for "invasiones" and "defunciones," a covariate table, and a file name, 
  #' and creates an Excel file with three worksheets: one for "invasiones" results, one for "defunciones" results,
  #' and one for the covariate table.
  # 
  #' @param res_invasiones A list of INLA model results for "invasiones"
  #' @param res_defunciones A list of INLA model results for "defunciones"
  #' @param res_table A predefined covariate table data frame.
  #' @param filename The file name for the output Excel file.
  
  wb <- createWorkbook()
  
  addWorksheet(wb, paste0("res_", INVASIONES_STR, "_summary"))
  writeData(wb, paste0("res_", INVASIONES_STR, "_summary"), res_invasiones$summary.fixed, rowNames = TRUE)
  
  addWorksheet(wb, paste0("res_", DEFUNCIONES_STR, "_summary"))
  writeData(wb, paste0("res_", DEFUNCIONES_STR, "_summary"), res_defunciones$summary.fixed, rowNames = TRUE)
  
  addWorksheet(wb, "res_table")
  writeData(wb, "res_table", res_table)
  
  saveWorkbook(wb, paste(COLERA_INLA_DIR, paste0("results", filename, ".xlsx"), sep = "/"), overwrite = TRUE)
}


# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


head(df_colera.merged.month)


# create df_covtemp and df_covprec

df_covtemp <- subset(df_temperatures.parsed, !(Municipio %in% c("alicante (m.)", "zaragoza (e.p.)")))
df_covprec <- subset(df_rain.parsed, !(Municipio %in% c("alicante (m.)", "zaragoza (e.p.)")))
head(df_covtemp)
head(df_covprec)


# merge df_covtemp and df_covprec with df_colera.merged.month as df_colera_inla7

df_climatic <- merge(df_covtemp, df_covprec, by = c(CODIGO_INE_STR, MUNICIPIO_STR, FECHA_STR, LONG_STR, LAT_STR))
head(df_climatic)

df_colera_inla7 <- merge(df_colera.merged.month, df_climatic, by = c(CODIGO_INE_STR, FECHA_STR))
df_colera_inla7 <- df_colera_inla7[, c(1, 6, 2:5, 7:10)]
head(df_colera_inla7)


# map and data ------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7" & CODNUT2 != "ES53") 
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) 


# merge mapS.municipios and df_colera_inla7

mapS.colera_inla7 <- merge(mapS.municipios, df_colera_inla7, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.colera_inla7)


# observed cases ----------------------------------------------------------


summary(mapS.colera_inla7)


# by month

for (month in MONTHS_INT) {
  
  map_invasiones <- create_tmap(mapS.colera_inla7[mapS.colera_inla7$Fecha == month,], c(MONTHS_STR[month-5]), mapS.municipios, TOTAL_INVASIONES_STR, "jenks")
  map_defunciones <- create_tmap(mapS.colera_inla7[mapS.colera_inla7$Fecha == month,], c(MONTHS_STR[month-5]), mapS.municipios, TOTAL_DEFUNCIONES_STR, "jenks")
  
  tmap_save(map_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.", INVASIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.", DEFUNCIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
}


# clean environment -------------------------------------------------------


rm(df_climatic, df_colera_inla7, df_covprec, df_covtemp, map_invasiones, map_defunciones, generate_totalsByMonth)


# modelling ---------------------------------------------------------------


model_all <- run_inla(mapS.colera_inla7, "", COVALL_STR)
model_all[INVASIONES_STR]$summary.fixed
model_all[DEFUNCIONES_STR]$summary.fixed


# results -----------------------------------------------------------------


res_table <- create_empty.table()
res_table <- add_results.table(model_all[INVASIONES_STR], model_all[DEFUNCIONES_STR], res_table)
add_results_excel(model_all[INVASIONES_STR], model_all[DEFUNCIONES_STR], res_table, "") 


save.image("colera_inla7.climate.RData")
