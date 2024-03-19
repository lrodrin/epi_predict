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

COVPROV_STR <- "covdist_caprov"
COVSTATION_STR <- "covdist_station"
COVRAIL_STR <- "covdist_rail" 
COVRIVER_STR <- "covdist_river"
COVWATER_STR <- "covdist_water"
COVROAD_STR <- "covdist_road"
COVCOAST_STR <- "covdist_coast"
COVPORT_STR <- "covdist_port"
COVALL_STR <- c(COVPROV_STR, COVSTATION_STR, COVRAIL_STR, COVRIVER_STR, COVWATER_STR, COVROAD_STR, COVCOAST_STR, COVPORT_STR)
PROVINCIAS_STR <- c("zaragoza", "valencia", "granada", "murcia", "teruel", "castellon", "alicante", "navarra", "cuenca", "albacete")
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


create_tmap.transport <- function(map) {
  #' Add Transportation Features to a Tmap
  #'
  #' This function adds transportation features, such as railway lines and rivers, to a Tmap.
  #'
  #' @param map The base map to which transportation features will be added.
  #' 
  #' @return A Tmap with added transportation features (railway lines and rivers).
  
  return(map +
           tm_shape(mapS.railways) + tm_lines(lwd = 1.5, col = "red") +
           tm_shape(mapS.rivers) + tm_lines(lwd = 1.5, col = "blue") +
           tm_add_legend(
             type = "line",
             labels = c("Railway lines", "Rivers"),
             col = c("red", "blue")
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
  
  if (length(provincias) != 1) { mapS.tmp <- subset(map, Provincia %in% provincias) }
  else {mapS.tmp <- subset(map, Provincia == provincias) }
  return(mapS.tmp)
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
  
  # run the INLA analysis with futures
  res_invasiones <- inla(formula_invasiones, family = "poisson", data = mapS.tmp, offset = log(Total_poblacion), control.predictor = list(compute = TRUE), verbose = TRUE)
  print(res_invasiones)
  res_defunciones <- inla(formula_defunciones, family = "poisson", data = mapS.tmp, offset = log(Total_poblacion), control.predictor = list(compute = TRUE), verbose = TRUE)
  print(res_defunciones)
  return(dict(invasiones = res_invasiones, defunciones = res_defunciones, .class = "any", .overwrite = TRUE))
}


run_inla.provincias <- function(provincia) {
  #' Run INLA Analysis for Multiple Covariates in a Specific Province
  #'
  #' This function runs an Integrated Nested Laplace Approximation (INLA) analysis for multiple covariates in a specific province.
  #'
  #' @param provincia A character vector specifying the province for the analysis.
  #'
  #' @return A list of INLA analysis results for each covariate in the specified province.
  
  model <- list()
  for (i in 1:length(COVALL_STR)) {
    model_i <- run_inla(mapS.colera_inla7, provincia, COVALL_STR[i])
    print(model_i[INVASIONES_STR]$summary.fixed)
    print(model_i[DEFUNCIONES_STR]$summary.fixed) 
    model[[i]] <- model_i
  }
  return(model)
}


create_empty.table <- function(isAll=FALSE) {
  #' Create an Empty Table for Coefficients and RR (Risk Ratios)
  #'
  #' This function creates an empty table for storing coefficients and risk ratios (RR) with their corresponding 95% credible intervals.
  #'
  #' @param isAll A logical value indicating whether the table should include all covariates or only a subset.
  #' 
  #' @return A data frame representing the empty table with column names and structure.
  
  columnames <- c("Covariates", "Coefficient (95% CrI)", "RR (95% CrI)", "Coefficient (95% CrI)", "RR (95% CrI)")
  
  if (isAll) {
    
    table <- data.frame(matrix(ncol = 5, nrow = 10))
    colnames(table) <- columnames
    table[1,1] <- "Intercept*"
    table[2,1] <- COVPROV_STR
    table[3,1] <- COVSTATION_STR
    table[4,1] <- COVRAIL_STR
    table[5,1] <- COVRIVER_STR
    table[6,1] <- COVWATER_STR
    table[7,1] <- COVROAD_STR
    table[8,1] <- COVCOAST_STR
    table[9,1] <- COVPORT_STR
    table[10,1] <- "Unstructured random effect"
  }
  else {
    
    table <- data.frame(matrix(ncol = 5, nrow = 8))
    colnames(table) <- columnames
    table[1,1] <- COVPROV_STR
    table[2,1] <- COVSTATION_STR
    table[3,1] <- COVRAIL_STR
    table[4,1] <- COVRIVER_STR
    table[5,1] <- COVWATER_STR
    table[6,1] <- COVROAD_STR
    table[7,1] <- COVCOAST_STR
    table[8,1] <- COVPORT_STR
  }
  return(table)
}


add_results.table <- function(res_invasiones, res_defunciones, res_table, isAll=FALSE) {
  #' Add INLA Results to Coefficients and RR Table
  #'
  #' This function adds the results of INLA analyses for invasions and defunciones to a coefficients and risk ratios (RR) table.
  #'
  #' @param res_invasiones The result object for invasions from the INLA analysis.
  #' @param res_defunciones The result object for defunciones from the INLA analysis.
  #' @param res_table The coefficients and RR table to which the results will be added.
  #' @param isAll A logical value indicating whether the table includes all covariates or only a subset.
  #' 
  #' @return The updated coefficients and RR table with INLA results.
  
  if(isAll) {
    
    for (i in 1:10) {
      
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
      
      if (!(i %in% c(1, 10))) {
        res_table[i, 3] <- paste0(
          sprintf("%.7f", exp(res_invasiones$summary.fixed[, COEFFICIENTS[1]][i] * 1000)), " (",
          sprintf("%.7f", exp(res_invasiones$summary.fixed[, COEFFICIENTS[2]][i] * 1000)), ", ",
          sprintf("%.7f", exp(res_invasiones$summary.fixed[, COEFFICIENTS[3]][i] * 1000)), ")"
        ) 
        res_table[i, 5] <- paste0(
          sprintf("%.7f", exp(res_defunciones$summary.fixed[, COEFFICIENTS[1]][i] * 1000)), " (",
          sprintf("%.7f", exp(res_defunciones$summary.fixed[, COEFFICIENTS[2]][i] * 1000)), ", ",
          sprintf("%.7f", exp(res_defunciones$summary.fixed[, COEFFICIENTS[3]][i] * 1000)), ")"
        )
      }
    }
  }
  else {
    
    for (i in 1:8) {
      
      res_table[i, 2] <- paste0(
        sprintf("%.7f", res_invasiones[[i]][INVASIONES_STR]$summary.fixed[2, COEFFICIENTS[1]]), " (",
        sprintf("%.7f", res_invasiones[[i]][INVASIONES_STR]$summary.fixed[2, COEFFICIENTS[2]]), ", ",
        sprintf("%.7f", res_invasiones[[i]][INVASIONES_STR]$summary.fixed[2, COEFFICIENTS[3]]), ")"
      ) 
      res_table[i, 4] <- paste0(
        sprintf("%.7f", res_defunciones[[i]][DEFUNCIONES_STR]$summary.fixed[2, COEFFICIENTS[1]]), " (",
        sprintf("%.7f", res_defunciones[[i]][DEFUNCIONES_STR]$summary.fixed[2, COEFFICIENTS[2]]), ", ",
        sprintf("%.7f", res_defunciones[[i]][DEFUNCIONES_STR]$summary.fixed[2, COEFFICIENTS[3]]), ")"
      ) 
      res_table[i, 3] <- paste0(
        sprintf("%.7f", exp(res_invasiones[[i]][INVASIONES_STR]$summary.fixed[2, COEFFICIENTS[1]] * 1000)), " (",
        sprintf("%.7f", exp(res_invasiones[[i]][INVASIONES_STR]$summary.fixed[2, COEFFICIENTS[2]] * 1000)), ", ",
        sprintf("%.7f", exp(res_invasiones[[i]][INVASIONES_STR]$summary.fixed[2, COEFFICIENTS[3]] * 1000)), ")"
      ) 
      res_table[i, 5] <- paste0(
        sprintf("%.7f", exp(res_defunciones[[i]][DEFUNCIONES_STR]$summary.fixed[2, COEFFICIENTS[1]] * 1000)), " (",
        sprintf("%.7f", exp(res_defunciones[[i]][DEFUNCIONES_STR]$summary.fixed[2, COEFFICIENTS[2]] * 1000)), ", ",
        sprintf("%.7f", exp(res_defunciones[[i]][DEFUNCIONES_STR]$summary.fixed[2, COEFFICIENTS[3]] * 1000)), ")"
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


head(df_distances)
head(df_colera.merged.month)


# merge df_distances with df_colera.merged.month as df_colera_inla7

df_colera_inla7 <- merge(df_colera.merged.month, df_distances, by = CODIGO_INE_STR)
df_colera_inla7 <- df_colera_inla7[, c(1, 6:7, 2, 3:5, 8:9, 10:17)]
head(df_colera_inla7)


# group df_colera_inla7 by provinces

df_colera_inla7.provincias <- df_colera_inla7 %>% group_by(Provincia, Fecha) %>%  
  summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones), Total_poblacion = sum(Total_poblacion))


# maps --------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.provincias <- st_read(paste(SHAPES_DATA_DIR, "Provincias_ETRS89_30N.shp", sep = "/"), quiet = TRUE)
mapS.railways <- st_read(paste(SHAPES_DATA_DIR, "Railways_1887.shp", sep = "/"), quiet = TRUE)
mapS.railways <- na.omit(mapS.railways)
mapS.rivers <- st_read(paste(SHAPES_DATA_DIR, "Rivers_Primary_Miteco_proj.shp", sep = "/"), quiet = TRUE)

mapS.provincias <- subset(mapS.provincias, !(Cod_CCAA %in% c("04", "05", "18", "19")))
mapS.provincias$Texto <- tolower(stri_trans_general(mapS.provincias$Texto, "Latin-ASCII"))
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7" & CODNUT2 != "ES53") 
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) 


# merge mapS.provincias with df_colera_inla7.provincias

mapS.colera_inla7.provincias <- merge(mapS.provincias, df_colera_inla7.provincias, by.x = "Texto", by.y = PROVINCIA_STR)
mapS.colera_inla7.provincias$Texto_Alt <- NULL
colnames(mapS.colera_inla7.provincias)[1:2] <- c(PROVINCIA_STR, paste0("Cod_", PROVINCIA_STR))
head(mapS.colera_inla7.provincias)


# merge mapS.municipios with df_colera_inla7

mapS.colera_inla7 <- merge(mapS.municipios, df_colera_inla7, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.colera_inla7)


# observed cases ----------------------------------------------------------


summary(mapS.colera_inla7)
# mapS.colera_inla7$Tasa_invasiones <- mapS.colera_inla7$Total_invasiones / mapS.colera_inla7$Total_poblacion
# mapS.colera_inla7$Tasa_defunciones <- mapS.colera_inla7$Total_defunciones / mapS.colera_inla7$Total_poblacion
# mapS.colera_inla7 <- mapS.colera_inla7[, c(1:14, 27, 15, 28, 16:26)]
# head(mapS.colera_inla7)


# by month

for (month in MONTHS_INT) {
  
  map_invasiones.provincias <- create_tmap(mapS.colera_inla7.provincias[mapS.colera_inla7.provincias$Fecha == month,], c(MONTHS_STR[month-5]), mapS.provincias, TOTAL_INVASIONES_STR, "jenks") +
    tm_shape(mapS.colera_inla7.provincias[mapS.colera_inla7.provincias$Fecha == month,]) + tm_text("Cod_Provincia", size = 1)
  
  map_defunciones.provincias <- create_tmap(mapS.colera_inla7.provincias[mapS.colera_inla7.provincias$Fecha == month,], c(MONTHS_STR[month-5]), mapS.provincias, TOTAL_DEFUNCIONES_STR, "jenks") +
    tm_shape(mapS.colera_inla7.provincias[mapS.colera_inla7.provincias$Fecha == month,]) + tm_text("Cod_Provincia", size = 1)
  
  map_invasiones <- create_tmap(mapS.colera_inla7[mapS.colera_inla7$Fecha == month,], c(MONTHS_STR[month-5]), mapS.municipios, TOTAL_INVASIONES_STR, "jenks")
  map_defunciones <- create_tmap(mapS.colera_inla7[mapS.colera_inla7$Fecha == month,], c(MONTHS_STR[month-5]), mapS.municipios, TOTAL_DEFUNCIONES_STR, "jenks")
  # map_tasa.invasiones <- create_tmap(mapS.colera_inla7[mapS.colera_inla7$Fecha == month,], c(MONTHS_STR[month-5]), mapS.municipios, "Tasa_invasiones", "jenks")
  # map_tasa.defunciones <- create_tmap(mapS.colera_inla7[mapS.colera_inla7$Fecha == month,], c(MONTHS_STR[month-5]), mapS.municipios, "Tasa_defunciones", "jenks")
  
  tmap_save(map_invasiones.provincias, filename = paste(COLERA_MAPS_DIR, paste0("tmap.provincias.", INVASIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_defunciones.provincias, filename = paste(COLERA_MAPS_DIR, paste0("tmap.provincias.", DEFUNCIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.", INVASIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.", DEFUNCIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  # tmap_save(map_tasa.invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.tasa_", INVASIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  # tmap_save(map_tasa.defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.tasa_", DEFUNCIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
}


# totals 

df_colera_inla7.grouped <- df_colera_inla7 %>% group_by(`Codigo Ine`, Municipio, Total_poblacion) %>% summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))
mapS.colera_inla7.grouped <- merge(mapS.municipios, df_colera_inla7.grouped, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.colera_inla7.grouped)

map_all_invasiones <- create_tmap(mapS.colera_inla7.grouped, NULL, mapS.municipios, TOTAL_INVASIONES_STR, "jenks") +
  tm_shape(mapS.colera_inla7.grouped[mapS.colera_inla7.grouped$Total_invasiones > 3915, ]) + tm_text(CODIGOINE_STR, size = 1.3, xmod = 2, fontface = "bold")

map_all_defunciones <- create_tmap(mapS.colera_inla7.grouped, NULL, mapS.municipios, TOTAL_DEFUNCIONES_STR, "jenks") +
  tm_shape(mapS.colera_inla7.grouped[mapS.colera_inla7.grouped$Total_defunciones > 1818, ]) + tm_text(CODIGOINE_STR, size = 1.3, xmod = 2, fontface = "bold")


# adding railway lines and rivers

map_all_invasiones <- create_tmap.transport(map_all_invasiones)
map_all_defunciones <- create_tmap.transport(map_all_defunciones)

tmap_save(map_all_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.", INVASIONES_STR, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
tmap_save(map_all_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.", DEFUNCIONES_STR, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# clean environment -------------------------------------------------------


rm(map_invasiones.provincias, map_defunciones.provincias, map_invasiones, map_defunciones, create_covariatesTS, generate_totalsByMonth, 
   df_colera_inla7, df_colera_inla7.provincias, df_colera_inla7.grouped, map_all_invasiones, map_all_defunciones, mapS.colera_inla7.provincias, mapS.colera_inla7.grouped)


# modelling ---------------------------------------------------------------


model_all <- run_inla(mapS.colera_inla7, "", COVALL_STR)
model_all[INVASIONES_STR]$summary.fixed
model_all[DEFUNCIONES_STR]$summary.fixed


# by province

# for (provincia in PROVINCIA_STR) { assign(paste0("model_", provincia), run_inla.provincias(provincia), envir = .GlobalEnv) }
model_zaragoza <- run_inla.provincias("zaragoza")
model_valencia <- run_inla.provincias("valencia")
model_granada <- run_inla.provincias("granada")
model_murcia <- run_inla.provincias("murcia")
model_castellon <- run_inla.provincias("castellon")
model_teruel <- run_inla.provincias("teruel")
model_alicante <- run_inla.provincias("alicante")
model_navarra <- run_inla.provincias("navarra")
model_cuenca <- run_inla.provincias("cuenca")
model_albacete <- run_inla.provincias("albacete")


# results -----------------------------------------------------------------


res_table <- create_empty.table(TRUE)
res_table <- add_results.table(model_all[INVASIONES_STR], model_all[DEFUNCIONES_STR], res_table, TRUE)
add_results_excel(model_all[INVASIONES_STR], model_all[DEFUNCIONES_STR], res_table, "") 


# by province

for (provincia in PROVINCIAS_STR) { assign(paste0("res_table.", provincia), create_empty.table(), envir = .GlobalEnv) }
res_table.zaragoza <- add_results.table(model_zaragoza, model_zaragoza, res_table.zaragoza)
res_table.valencia <-  add_results.table(model_valencia, model_valencia, res_table.valencia)
res_table.granada <-  add_results.table(model_granada, model_granada, res_table.granada)
res_table.murcia <-  add_results.table(model_murcia, model_murcia, res_table.murcia)
res_table.teruel <-  add_results.table(model_teruel, model_teruel, res_table.teruel)
res_table.castellon <-  add_results.table(model_castellon, model_castellon, res_table.castellon)
res_table.alicante <-  add_results.table(model_alicante, model_alicante, res_table.alicante)
res_table.navarra <-  add_results.table(model_navarra, model_navarra, res_table.navarra)
res_table.cuenca <-  add_results.table(model_cuenca, model_cuenca, res_table.cuenca)
res_table.albacete <-  add_results.table(model_albacete, model_albacete, res_table.albacete)
res_table.list <- list(res_table.zaragoza, res_table.valencia, res_table.granada, res_table.murcia, res_table.teruel, res_table.castellon, res_table.alicante, res_table.navarra, res_table.cuenca, res_table.albacete)
for (i in 1:length(PROVINCIAS_STR)) { write.xlsx(res_table.list[i], file = paste(COLERA_INLA_DIR, paste0("results_", PROVINCIAS_STR[i], ".xlsx"), sep = "/")) }

save.image("colera_inla7.dist.RData")