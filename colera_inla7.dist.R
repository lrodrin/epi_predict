library(sf)
library(dplyr)
library(zoo)
library(leafpop)
library(INLA)
library(spdep)
library(lubridate)
library(tmap)
library(openxlsx)


load("colera_data.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
COLERA_MAPS_DIR <- "colera_maps"
dir.create(COLERA_MAPS_DIR, showWarnings = FALSE)

COVPROV_STR <- "covdist_caprov"
COVSTATION_STR <- "covdist_station"
COVRAIL_STR <- "covdist_rail" 
COVRIVER_STR <- "covdist_river"
COVROAD_STR <- "covdist_road"
COVCOAST_STR <- "covdist_coast"
COVPORT_STR <- "covdist_port"
COVALL_STR <- c(COVPROV_STR, COVSTATION_STR, COVRAIL_STR, COVRIVER_STR, COVROAD_STR, COVCOAST_STR, COVPORT_STR)
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
  
  return(
    tm_shape(df_mes, bbox = map) +
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
      )
  )
}


mapS_byProvincias <- function(map, provincias) {
  #' Subset Spatial Data by Provinces
  #'
  #' This function subsets a spatial map based on the specified provinces.
  #'
  #' @param map A spatial map data frame.
  #' @param provincias A character vector containing the names of provinces to subset.
  #' 
  #' @return A subset of the input spatial map containing only the specified provinces.
  #' 
  
  if (length(provincias) != 1) { mapS.tmp <- subset(map, Provincia %in% provincias) }
  else {mapS.tmp <- subset(map, Provincia == provincias) }
  return(mapS.tmp)
}


create_inla.graph <- function(map, mapname) {
  #' Create an INLA graph representation for a spatial map
  #'
  #' This function creates an INLA graph representation for a given spatial map.
  # The graph is useful for spatial modelling with the Integrated Nested Laplace Approximation (INLA).
  # The function generates an adjacency matrix and returns it as an INLA graph object.
  # The adjacency matrix is used to define spatial neighbourhood relationships in INLA models.
  # 
  #' @param map A spatial map for which you want to create an INLA graph.
  #' @param mapname A character string representing the name of the map.
  # 
  #' @return An INLA graph object that represents the adjacency matrix for the provided spatial map.
  
  outputname <- paste0("map.", mapname, ".adj")
  
  # neighbourhood matrix
  nb <- poly2nb(map)
  
  nb2INLA(outputname, nb)
  g <- inla.read.graph(filename = outputname)
  return(g)
}


create_index.vectors <- function(map) {
  #' Create Index Vectors for Spatial and Temporal Variables
  #'
  #' This function creates index vectors for spatial and temporal variables in a given spatial map.
  # The index vectors are used to uniquely identify areas and time points in the map, which can be
  # useful for various spatial and temporal analyses.
  # 
  #' @param map A spatial map containing the relevant variables, including "CODIGOINE" and "Fecha".
  # 
  #' @return The map with two additional columns: "idarea" and "idtime", which represent the index vectors
  # for the spatial and temporal variables, respectively.
  
  map$idarea <- as.numeric(as.factor(map$CODIGOINE))
  map$idarea1 <- map$idarea
  map$idtime <- 1 + map$Fecha - min(map$Fecha)
  return(map)
}


formula_inla <- function(response_var, covariates, g) {
  #' Create INLA Formula with Spatial and Temporal Terms
  #'
  #' This function generates a formula for use in the INLA package by combining a response variable,
  #' covariates, spatial terms, and a time term.
  # 
  #' @param response_var The name of the response variable.
  #' @param covariates A character vector of covariates to include in the formula.
  #' @param g The graph structure for the spatial term (e.g., neighbourhood relationships).
  # 
  #' @return An INLA formula that combines the response variable, covariates, spatial terms, and a time term.
  
  spatial_terms <- c(paste("f(idarea, model = 'bym', graph =", g, ")"), "f(idarea1, idtime, model = 'iid')")
  time_term <- "idtime"
  
  formula <- as.formula(paste(response_var, "~", paste(covariates, collapse = " + "), "+", paste(spatial_terms, collapse = " + "), "+", time_term))
  return(formula)
}


run_inla <- function(distribution, formula, df_colera, var_col) {
  #' Run an INLA model for regression analysis.
  #'
  #' This function runs an Integrated Nested Laplace Approximations (INLA) model for regression analysis.
  #' It uses the specified distribution, formula, and data frame to fit the model.
  #'
  #' @param distribution Distribution family for the model (e.g., "gaussian", "poisson").
  #' @param formula Model formula.
  #' @param df_colera Data frame containing the observations.
  #'
  #' @return The result of the INLA model fitting.
  
  res <- inla(formula, family = distribution, data = df_colera, offset = log(Total_poblacion), control.predictor = list(compute = TRUE), control.compute = list())
  return(res)
}


create_empty.table <- function() {
  #' Create an Empty Covariate Table
  #'
  #' This function creates an empty table for storing covariate coefficients and relative risks
  #' with default column names and place holders for covariate names.
  # 
  #' @return An empty data frame with predefined column names and placeholders for covariate names.
  
  table <- data.frame(matrix(ncol = 5, nrow = 9))
  colnames(table) <- c("Covariates", "Coefficient (95% CrI)", "RR (95% CrI)", "Coefficient (95% CrI)", "RR (95% CrI)")
  
  # add covariates names
  
  table[1,1] <- "Intercept*"
  table[2,1] <- COVPROV_STR
  table[3,1] <- COVSTATION_STR
  table[4,1] <- COVRAIL_STR
  table[5,1] <- COVRIVER_STR
  table[6,1] <- COVROAD_STR
  table[7,1] <- COVCOAST_STR
  table[8,1] <- COVPORT_STR
  table[9,1] <- "Unstructured random effect"
  
  covariates <- table$Covariates
  
  return(table)
}


add_results.table <- function(res_invasiones, res_defunciones, res_table) {
  #' Add Results to a Covariate Table
  #'
  #' This function adds coefficient and relative risk results from an INLA analysis to a predefined
  #' covariate table for "Total_invasiones" and "Total_defunciones".
  # 
  #' @param res_invasiones A list of INLA model results for "Total_invasiones.
  #' @param res_defunciones A list of INLA model results for Total_defunciones".
  #' @param res_table A predefined covariate table data frame where the results will be added.
  # 
  #' @return The covariate table with added coefficient and relative risk results.
  
  # add coefficients and RRs for "Total_invasiones" and "Total_defunciones"
  for (i in 1:9) {
    
    # coefficients 
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
    
    # RRs
    if (!(i %in% c(1, 9))) {
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
  #' This function takes INLA results for "Total_invasiones" and "Total_defunciones," a covariate table, and a file name, 
  #' and creates an Excel file with three worksheets: one for "Total_invasiones" results, one for "Total_defunciones" results,
  #' and one for the covariate table.
  # 
  #' @param res_invasiones A list of INLA model results for "Total_invasiones."
  #' @param res_defunciones A list of INLA model results for "Total_defunciones."
  #' @param res_table A predefined covariate table data frame.
  #' @param filename The file name for the output Excel file.
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "res_invasiones_summary")
  writeData(wb, "res_invasiones_summary", res_invasiones$summary.fixed, rowNames = TRUE)
  
  addWorksheet(wb, "res_defunciones_summary")
  writeData(wb, "res_defunciones_summary", res_defunciones$summary.fixed, rowNames = TRUE)
  
  addWorksheet(wb, "res_table")
  writeData(wb, "res_table", res_table)
  
  saveWorkbook(wb, paste0("results", filename, ".xlsx"), overwrite = TRUE)
  
}


# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


head(df_distances)
head(df_colera.merged.month)


# merge df_distances with df_colera.merged.month as df_colera_inla7

df_colera_inla7 <- merge(df_colera.merged.month, df_distances, by = CODIGO_INE_STR)
df_colera_inla7 <- df_colera_inla7[, c(1, 6:7, 2, 3:5, 8:9, 10:16)]
head(df_colera_inla7)


# map ---------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7") # remove Canary Islands
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) # remove "Ceuta" and "Melilla"
head(mapS.municipios)


# merge mapS.municipios and df_colera_inla7

mapS.colera_inla7 <- merge(mapS.municipios, df_colera_inla7, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.colera_inla7)


# observed cases ----------------------------------------------------------


mapS.colera_inla7$Tasa_invasiones <- mapS.colera_inla7$Total_invasiones / mapS.colera_inla7$Total_poblacion
mapS.colera_inla7$Tasa_defunciones <- mapS.colera_inla7$Total_defunciones / mapS.colera_inla7$Total_poblacion
mapS.colera_inla7 <- mapS.colera_inla7[, c(1:14, 27, 15, 28, 16:26)]
head(mapS.colera_inla7)


for (month in MONTHS_INT) {

  map_invasiones <-
    create_tmap(
      mapS.colera_inla7[mapS.colera_inla7$Fecha == month,],
      c(MONTHS_STR[month-5]),
      mapS.municipios,
      TOTAL_INVASIONES_STR,
      "jenks")

  map_defunciones <-
    create_tmap(
      mapS.colera_inla7[mapS.colera_inla7$Fecha == month,],
      c(MONTHS_STR[month-5]),
      mapS.municipios,
      TOTAL_DEFUNCIONES_STR,
      "jenks")

  map_tasa.invasiones <-
    create_tmap(
      mapS.colera_inla7[mapS.colera_inla7$Fecha == month,],
      c(MONTHS_STR[month-5]),
      mapS.municipios,
      "Tasa_invasiones",
      "jenks")

  map_tasa.defunciones <-
    create_tmap(
      mapS.colera_inla7[mapS.colera_inla7$Fecha == month,],
      c(MONTHS_STR[month-5]),
      mapS.municipios,
      "Tasa_defunciones",
      "jenks")

  tmap_save(map_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.invasiones", month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.defunciones", month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_tasa.invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.tasa_invasiones", month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_tasa.defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.tasa_defunciones", month,".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")

}


# clean environment -------------------------------------------------------


rm(map_invasiones, map_defunciones, map_tasa.invasiones, map_tasa.defunciones, create_covariatesTS, generate_totalsByMonth, df_colera_inla7)


# modelling ---------------------------------------------------------------


# maps by "provincias"

# mapS.colera_inla7.comunitat_valenciana <- mapS_byProvincias(mapS.colera_inla7, c("alicante", "castellon", "valencia"))
mapS.colera_inla7.alicante <- mapS_byProvincias(mapS.colera_inla7, "alicante")
mapS.colera_inla7.castellon <- mapS_byProvincias(mapS.colera_inla7, "castellon")
mapS.colera_inla7.valencia <- mapS_byProvincias(mapS.colera_inla7, "valencia")
# mapS.colera_inla7.aragon <- mapS_byProvincias(mapS.colera_inla7, c("huesca", "teruel", "zaragoza"))
mapS.colera_inla7.huesca<- mapS_byProvincias(mapS.colera_inla7, "huesca")
mapS.colera_inla7.teruel <- mapS_byProvincias(mapS.colera_inla7, "teruel")
mapS.colera_inla7.zaragoza <- mapS_byProvincias(mapS.colera_inla7, "zaragoza")
# mapS.colera_inla7.andalucia <- mapS_byProvincias(mapS.colera_inla7, c("almeria", "cadiz", "cordoba", "granada", "jaen", "malaga"))
mapS.colera_inla7.murcia <- mapS_byProvincias(mapS.colera_inla7, "murcia")


# graphs by "provincias"

g <- create_inla.graph(mapS.colera_inla7, "")
# g.comunitat_valenciana <- create_inla.graph(mapS.colera_inla7.comunitat_valenciana, "comunitat_valenciana")
g.alicante <- create_inla.graph(mapS.colera_inla7.alicante, "alicante")
g.castellon <- create_inla.graph(mapS.colera_inla7.castellon, "castellon")
g.valencia <- create_inla.graph(mapS.colera_inla7.valencia, "valencia")
# g.aragon <- create_inla.graph(mapS.colera_inla7.aragon, "aragon")
g.huesca <- create_inla.graph(mapS.colera_inla7.huesca, "huesca")
g.teruel <- create_inla.graph(mapS.colera_inla7.teruel, "teruel")
g.zaragoza <- create_inla.graph(mapS.colera_inla7.zaragoza, "zaragoza")
# g.andalucia <- create_inla.graph(mapS.colera_inla7.andalucia, "andalucia")
g.murcia <- create_inla.graph(mapS.colera_inla7.murcia, "murcia")


# create index vectors by "provincias"

mapS.colera_inla7 <- create_index.vectors(mapS.colera_inla7)
# mapS.colera_inla7.comunitat_valenciana <- create_index.vectors(mapS.colera_inla7.comunitat_valenciana)
mapS.colera_inla7.alicante <- create_index.vectors(mapS.colera_inla7.alicante)
mapS.colera_inla7.castellon <- create_index.vectors(mapS.colera_inla7.castellon)
mapS.colera_inla7.valencia <- create_index.vectors(mapS.colera_inla7.valencia)
# mapS.colera_inla7.aragon <- create_index.vectors(mapS.colera_inla7.aragon)
mapS.colera_inla7.huesca <- create_index.vectors(mapS.colera_inla7.huesca)
mapS.colera_inla7.teruel <- create_index.vectors(mapS.colera_inla7.teruel)
mapS.colera_inla7.zaragoza <- create_index.vectors(mapS.colera_inla7.zaragoza)
# mapS.colera_inla7.andalucia <- create_index.vectors(mapS.colera_inla7.andalucia)
mapS.colera_inla7.murcia <- create_index.vectors(mapS.colera_inla7.murcia)


# formulas of the Bernardinelli model

# formula_all.invasiones <- Total_invasiones ~ covdist_caprov + covdist_station + covdist_rail + covdist_river + covdist_road + covdist_coast + covdist_port +
#   f(idarea, model = "bym", graph = g) + f(idarea1, idtime, model = "iid") + idtime
# 
# formula_all.defunciones <- Total_defunciones ~ covdist_caprov + covdist_station + covdist_rail + covdist_river + covdist_road + covdist_coast + covdist_port +
#   f(idarea, model = "bym", graph = g) + f(idarea1, idtime, model = "iid") + idtime

formula_all.invasiones <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g")
formula_all.defunciones <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g")
# formula_all.invasiones.comunitat_valenciana <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g.comunitat_valenciana")
# formula_all.defunciones.comunitat_valenciana <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g.comunitat_valenciana")
formula_all.invasiones.alicante <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g.alicante")
formula_all.defunciones.alicante <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g.alicante")
formula_all.invasiones.castellon <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g.castellon")
formula_all.defunciones.castellon <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g.castellon")
formula_all.invasiones.valencia <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g.valencia")
formula_all.defunciones.valencia <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g.valencia")
# formula_all.invasiones.aragon <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g.aragon")
# formula_all.defunciones.aragon <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g.aragon")
formula_all.invasiones.huesca <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g.huesca")
formula_all.defunciones.huesca <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g.huesca")
formula_all.invasiones.teruel <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g.teruel")
formula_all.defunciones.teruel <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g.teruel")
formula_all.invasiones.zaragoza <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g.zaragoza")
formula_all.defunciones.zaragoza <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g.zaragoza")
# formula_all.invasiones.andalucia <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g.andalucia")
# formula_all.defunciones.andalucia <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g.andalucia")
formula_all.invasiones.murcia <- formula_inla(TOTAL_INVASIONES_STR, COVALL_STR, "g.murcia")
formula_all.defunciones.murcia <- formula_inla(TOTAL_DEFUNCIONES_STR, COVALL_STR, "g.murcia")

# formula_covdist_caprov.invasiones <- formula_inla(TOTAL_INVASIONES_STR, COVPROV_STR, "g")
# formula_covdist_caprov.defunciones <- formula_inla(TOTAL_DEFUNCIONES_STR, COVPROV_STR, "g")
# formula_covdist_station.invasiones <- formula_inla(TOTAL_INVASIONES_STR, COVSTATION_STR, "g")
# formula_covdist_station.defunciones <- formula_inla(TOTAL_DEFUNCIONES_STR, COVSTATION_STR, "g")
# formula_covdist_rail.invasiones <- formula_inla(TOTAL_INVASIONES_STR, COVRAIL_STR, "g")
# formula_covdist_rail.defunciones <- formula_inla(TOTAL_DEFUNCIONES_STR, COVRAIL_STR, "g")
# formula_covdist_river.invasiones <- formula_inla(TOTAL_INVASIONES_STR, COVRIVER_STR, "g")
# formula_covdist_river.defunciones <- formula_inla(TOTAL_DEFUNCIONES_STR, COVRIVER_STR, "g")
# formula_covdist_road.invasiones <- formula_inla(TOTAL_INVASIONES_STR, COVROAD_STR, "g")
# formula_covdist_road.defunciones <- formula_inla(TOTAL_DEFUNCIONES_STR, COVROAD_STR, "g")
# formula_covdist_coast.invasiones <- formula_inla(TOTAL_INVASIONES_STR, COVCOAST_STR, "g")
# formula_covdist_coast.defunciones <- formula_inla(TOTAL_DEFUNCIONES_STR, COVCOAST_STR, "g")
# formula_covdist_port.invasiones <- formula_inla(TOTAL_INVASIONES_STR, COVPORT_STR, "g")
# formula_covdist_port.defunciones <- formula_inla(TOTAL_DEFUNCIONES_STR, COVPORT_STR, "g")
  

# inference using INLA

res_all.invasiones <- run_inla("poisson", formula_all.invasiones, mapS.colera_inla7)
res_all.defunciones <- run_inla("poisson", formula_all.defunciones, mapS.colera_inla7)
# res_all.invasiones.comunitat_valenciana <- run_inla("poisson", formula_all.invasiones.comunitat_valenciana, mapS.colera_inla7.comunitat_valenciana)
# res_all.defunciones.comunitat_valenciana <- run_inla("poisson", formula_all.defunciones.comunitat_valenciana, mapS.colera_inla7.comunitat_valenciana)
res_all.invasiones.alicante <- run_inla("poisson", formula_all.invasiones.alicante, mapS.colera_inla7.alicante)
res_all.defunciones.alicante <- run_inla("poisson", formula_all.defunciones.alicante, mapS.colera_inla7.alicante)
res_all.invasiones.castellon <- run_inla("poisson", formula_all.invasiones.castellon, mapS.colera_inla7.castellon)
res_all.defunciones.castellon <- run_inla("poisson", formula_all.defunciones.castellon, mapS.colera_inla7.castellon)
res_all.invasiones.valencia <- run_inla("poisson", formula_all.invasiones.valencia, mapS.colera_inla7.valencia)
res_all.defunciones.valencia <- run_inla("poisson", formula_all.defunciones.valencia, mapS.colera_inla7.valencia)
# res_all.invasiones.aragon <- run_inla("poisson", formula_all.invasiones.aragon, mapS.colera_inla7.aragon)
# res_all.defunciones.aragon <- run_inla("poisson", formula_all.defunciones.aragon, mapS.colera_inla7.aragon)
res_all.invasiones.huesca <- run_inla("poisson", formula_all.invasiones.huesca, mapS.colera_inla7.huesca)
res_all.defunciones.huesca <- run_inla("poisson", formula_all.defunciones.huesca, mapS.colera_inla7.huesca)
res_all.invasiones.teruel <- run_inla("poisson", formula_all.invasiones.teruel, mapS.colera_inla7.teruel)
res_all.defunciones.teruel <- run_inla("poisson", formula_all.defunciones.teruel, mapS.colera_inla7.teruel)
res_all.invasiones.zaragoza <- run_inla("poisson", formula_all.invasiones.zaragoza, mapS.colera_inla7.zaragoza)
res_all.defunciones.zaragoza <- run_inla("poisson", formula_all.defunciones.zaragoza, mapS.colera_inla7.zaragoza)
# res_all.invasiones.andalucia <- run_inla("poisson", formula_all.invasiones.andalucia, mapS.colera_inla7.andalucia)
# res_all.defunciones.andalucia <- run_inla("poisson", formula_all.defunciones.andalucia, mapS.colera_inla7.andalucia)
# res_all.invasiones.murcia <- run_inla("poisson", formula_all.invasiones.murcia, mapS.colera_inla7.murcia)
res_all.defunciones.murcia <- run_inla("poisson", formula_all.defunciones.murcia, mapS.colera_inla7.murcia)

# res_covdist_caprov.invasiones <- run_inla("poisson", formula_covdist_caprov.invasiones, mapS.colera_inla7)
# res_covdist_caprov.defunciones <- run_inla("poisson", formula_covdist_caprov.defunciones, mapS.colera_inla7)
# res_covdist_station.invasiones <- run_inla("poisson", formula_covdist_station.invasiones, mapS.colera_inla7)
# res_covdist_station.defunciones <- run_inla("poisson", formula_covdist_station.defunciones, mapS.colera_inla7)
# res_covdist_rail.invasiones <- run_inla("poisson", formula_covdist_rail.invasiones, mapS.colera_inla7)
# res_covdist_rail.defunciones <- run_inla("poisson", formula_covdist_rail.defunciones, mapS.colera_inla7)
# res_covdist_river.invasiones <- run_inla("poisson", formula_covdist_river.invasiones, mapS.colera_inla7)
# res_covdist_river.defunciones <- run_inla("poisson", formula_covdist_river.defunciones, mapS.colera_inla7)
# res_covdist_road.invasiones <- run_inla("poisson", formula_covdist_road.invasiones, mapS.colera_inla7)
# res_covdist_road.defunciones <- run_inla("poisson", formula_covdist_road.defunciones, mapS.colera_inla7)
# res_covdist_coast.invasiones <- run_inla("poisson", formula_covdist_coast.invasiones, mapS.colera_inla7)
# res_covdist_coast.defunciones <- run_inla("poisson", formula_covdist_coast.defunciones, mapS.colera_inla7)
# res_covdist_port.invasiones <- run_inla("poisson", formula_covdist_port.invasiones, mapS.colera_inla7)
# res_covdist_port.defunciones <- run_inla("poisson", formula_covdist_port.defunciones, mapS.colera_inla7)


# results -----------------------------------------------------------------


res_all.invasiones$summary.fixed
res_all.defunciones$summary.fixed
# res_all.invasiones.comunitat_valenciana$summary.fixed
# res_all.defunciones.comunitat_valenciana$summary.fixed
res_all.invasiones.alicante$summary.fixed
res_all.defunciones.alicante$summary.fixed
res_all.invasiones.castellon$summary.fixed
res_all.defunciones.castellon$summary.fixed
res_all.invasiones.valencia$summary.fixed
res_all.defunciones.valencia$summary.fixed
# res_all.invasiones.aragon$summary.fixed
# res_all.defunciones.aragon$summary.fixed
res_all.invasiones.huesca$summary.fixed
res_all.defunciones.huesca$summary.fixed
res_all.invasiones.teruel$summary.fixed
res_all.defunciones.teruel$summary.fixed
res_all.invasiones.zaragoza$summary.fixed
res_all.defunciones.zaragoza$summary.fixed
# res_all.invasiones.andalucia$summary.fixed
# res_all.defunciones.andalucia$summary.fixed
res_all.invasiones.murcia$summary.fixed
res_all.defunciones.murcia$summary.fixed

res_all.invasiones.list <-
  list(
    res_all.invasiones,
    # res_all.invasiones.comunitat_valenciana,
    res_all.invasiones.alicante, res_all.invasiones.castellon, res_all.invasiones.valencia,
    # res_all.invasiones.aragon,
    res_all.invasiones.huesca, res_all.invasiones.teruel, res_all.invasiones.zaragoza,
    # res_all.invasiones.andalucia,
    res_all.invasiones.murcia
  )

res_all.defunciones.list <-
  list(
    res_all.defunciones,
    # res_all.defunciones.comunitat_valenciana,
    res_all.defunciones.alicante, res_all.defunciones.castellon, res_all.defunciones.valencia,
    # res_all.defunciones.aragon,
    res_all.defunciones.huesca, res_all.defunciones.teruel, res_all.defunciones.zaragoza,
    # res_all.defunciones.andalucia,
    res_all.defunciones.murcia
  )

# res_covdist_caprov.invasiones
# res_covdist_caprov.defunciones
# res_covdist_station.invasiones
# res_covdist_station.defunciones
# res_covdist_rail.invasiones
# res_covdist_rail.defunciones
# res_covdist_river.invasiones
# res_covdist_river.defunciones
# res_covdist_road.invasiones
# res_covdist_river.defunciones
# es_covdist_coast.invasiones
# res_covdist_coast.defunciones
# res_covdist_port.invasiones
# res_covdist_port.defunciones


# save coefficients and RRs in res_table

res_table <- create_empty.table()
# res_table.comunitat_valenciana <- create_empty.table()
res_table.alicante <- create_empty.table()
res_table.castellon <- create_empty.table()
res_table.valencia<- create_empty.table()
# res_table.aragon <- create_empty.table()
res_table.huesca <- create_empty.table()
res_table.teruel <- create_empty.table()
res_table.zaragoza<- create_empty.table()
# res_table.andalucia <- create_empty.table()
res_table.murcia <- create_empty.table()

res_table <- add_results.table(res_all.invasiones.list[[1]], res_all.defunciones.list[[1]], res_table)
# res_table.comunitat_valenciana <- add_results.table(res_all.invasiones.list[[2]], res_all.defunciones.list[[2]], res_table.comunitat_valenciana)
res_table.alicante <- add_results.table(res_all.invasiones.list[[2]], res_all.defunciones.list[[2]], res_table.alicante)
res_table.castellon <- add_results.table(res_all.invasiones.list[[3]], res_all.defunciones.list[[3]], res_table.castellon)
res_table.valencia <- add_results.table(res_all.invasiones.list[[4]], res_all.defunciones.list[[4]], res_table.valencia)
# res_table.aragon <- add_results.table(res_all.invasiones.list[[3]], res_all.defunciones.list[[3]], res_table.aragon)
res_table.huesca <- add_results.table(res_all.invasiones.list[[5]], res_all.defunciones.list[[5]], res_table.huesca)
res_table.teruel <- add_results.table(res_all.invasiones.list[[6]], res_all.defunciones.list[[6]], res_table.teruel)
res_table.zaragoza <- add_results.table(res_all.invasiones.list[[7]], res_all.defunciones.list[[7]], res_table.zaragoza)
# res_table.andalucia <- add_results.table(res_all.invasiones.list[[4]], res_all.defunciones.list[[4]], res_table.andalucia)
# res_table.murcia <- add_results.table(res_all.invasiones.list[[5]], res_all.defunciones.list[[5]], res_table.murcia)
res_table.murcia <- add_results.table(res_all.invasiones.list[[8]], res_all.defunciones.list[[8]], res_table.murcia)

# res_table.list <- list(res_table, res_table.comunitat_valenciana, res_table.aragon, res_table.andalucia, res_table.murcia)
res_table.list <- list(res_table, res_table.alicante, res_table.castellon, res_table.valencia, res_table.huesca, res_table.teruel, res_table.zaragoza, res_table.murcia)


# save results as workbook

# provincias <- c("", ".comunitat_valenciana", ".aragon", ".andalucia", ".murcia")
provincias <- c("", ".alicante", ".castellon", ".valencia", ".huesca", ".teruel", ".zaragoza", ".murcia")
# for (i in 1:5) {
for (i in 1:8) {
  
  add_results_excel(res_all.invasiones.list[[i]], res_all.defunciones.list[[i]], res_table.list[i], provincias[i])
  
}

# save.image("colera_inla7.dist.RData")
