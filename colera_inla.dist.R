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


load("colera_data.RData") # load colera data


# constants ---------------------------------------------------------------


MUNICIPIOS_SHAPEFILE <- "Municipios_IGN.shp"
PROVINCIAS_SHAPEFILE <- "Provincias_ETRS89_30N.shp"
RAILWAYS_SHAPEFILE <- "Railways_1887.shp"
RIVERS_SHAPEFILE <- "A3RIOS_proj.shp"

SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
COLERA_MAPS_DIR <- "colera_maps"
dir.create(COLERA_MAPS_DIR, showWarnings = FALSE)
COLERA_INLA_DIR <- "colera_inla"
dir.create(COLERA_INLA_DIR, showWarnings = FALSE)

TOTAL_INVASIONES_STR <- paste("Total", INVASIONES_STR, sep = "_")
TOTAL_DEFUNCIONES_STR <- paste("Total", DEFUNCIONES_STR, sep = "_")
COVPROV_STR <- "covdist_caprov"
COVSTATION_STR <- "covdist_station"
COVRAIL_STR <- "covdist_rail" 
COVRIVER_STR <- "covdist_river"
COVWATER_STR <- "covdist_water"
COVROAD_STR <- "covdist_road"
COVCOAST_STR <- "covdist_coast"
COVPORT_STR <- "covdist_port"
COVALL_STR <- c(COVPROV_STR, COVSTATION_STR, COVRAIL_STR, COVRIVER_STR, COVWATER_STR, COVROAD_STR, COVCOAST_STR, COVPORT_STR)
PROVINCIAS_STR <- c("zaragoza", "valencia", "granada", "murcia", "teruel", "castellon", "alicante", "navarra", "cuenca", "albacete", "almeria")
MONTHS_STR <- c("June", "July", "August", "September", "October", "November")
COEFFICIENTS <- c("mean", "0.025quant", "0.975quant")


# functions ---------------------------------------------------------------


create_tmap <- function(df_mes, mes, map, var_col, style) {
  #' Create a Tmap
  #'
  #' This function creates a Tmap with a specified style and color palette.
  #'
  #' @param df_mes A data frame containing the data for the map.
  #' @param mes A character vector specifying the month for the map.
  #' @param map A spatial map data frame.
  #' @param var_col A character vector specifying the column name for the variable to be mapped.
  #' @param style A character vector specifying the style for the map.
  #'
  #' @return A Tmap with the specified style and color palette.
  
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
  #' Create a Tmap with Transport Data
  #'
  #' This function creates a Tmap with transport data including railway lines and rivers.
  #'
  #' @param map A spatial map data frame.
  #'
  #' @return A Tmap with transport data including railway lines and rivers.

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
  #' Subset a Spatial Map by Provinces
  #'
  #' This function subsets a spatial map by provinces.
  #'
  #' @param map A spatial map data frame.
  #' @param provincias A character vector specifying the provinces to include in the subset.
  #'
  #' @return A spatial map data frame subset by provinces.
  
  if (length(provincias) != 1) { mapS.tmp <- subset(map, Provincia %in% provincias) }
  else {mapS.tmp <- subset(map, Provincia == provincias) }
  return(mapS.tmp)
}


run_inla <- function(mapS, province, covariate) {
  #' Run INLA Analysis for Multiple Covariates
  #'
  #' This function runs an Integrated Nested Laplace Approximation (INLA) analysis for multiple covariates of cases and deaths.
  #'
  #' @param mapS A spatial map data frame.
  #' @param province A character vector specifying the province for the analysis.
  #' @param covariate A character vector specifying the covariate for the analysis.
  #'
  #' @return A list of INLA analysis results for each covariate of cases and deaths.
  
  # create neighbourhood matrix
  if (province != "") { mapS.tmp <- mapS_byProvincias(mapS, province) } # subset by province
  else { mapS.tmp <- mapS} # use the entire map
  nb <- poly2nb(mapS.tmp)
  head(nb)
  nb2INLA("map.adj", nb)
  g <- inla.read.graph(filename = "map.adj")
  
  # create index vectors
  mapS.tmp$idarea <- as.numeric(as.factor(mapS.tmp$CODIGOINE))
  mapS.tmp$idarea1 <- mapS.tmp$idarea
  mapS.tmp$idtime <- 1 + mapS.tmp$Fecha - min(mapS.tmp$Fecha)
  
  # define formula for cases and deaths
  covariates <- c(covariate, "f(idarea, model = 'bym', graph = g)", "f(idarea1, idtime, model = 'iid')", "idtime")
  formula_invasiones <- as.formula(paste(paste(TOTAL_INVASIONES_STR, "~"), paste(covariates, collapse = " + ")))
  formula_defunciones <- as.formula(paste(paste(TOTAL_DEFUNCIONES_STR, "~"), paste(covariates, collapse = " + ")))
  print(formula_invasiones)
  print(formula_defunciones)
  
  # run INLA for cases and deaths
  res_invasiones <- inla(formula_invasiones, family = "poisson", data = mapS.tmp, offset = log(Total_poblacion), control.predictor = list(compute = TRUE), verbose = TRUE)
  print(res_invasiones)
  res_defunciones <- inla(formula_defunciones, family = "poisson", data = mapS.tmp, offset = log(Total_poblacion), control.predictor = list(compute = TRUE), verbose = TRUE)
  print(res_defunciones)
  return(dict(invasiones = res_invasiones, defunciones = res_defunciones, .class = "any", .overwrite = TRUE)) # return results
}


run_inla.provincias <- function(provincia) {
  #' Run INLA Analysis for Multiple Covariates by Province
  #'
  #' This function runs an Integrated Nested Laplace Approximation (INLA) analysis for multiple covariates of cases and deaths by province.
  #'
  #' @param provincia A character vector specifying the province for the analysis.
  #'
  #' @return A list of INLA analysis results for each covariate of cases and deaths by province.
  
  model <- list() # create a list to store results

  for (i in 1:length(COVALL_STR)) { # for each covariate

    model_i <- run_inla(mapS.colera_inla7, provincia, COVALL_STR[i]) # run INLA analysis
    print(model_i[INVASIONES_STR]$summary.fixed)
    print(model_i[DEFUNCIONES_STR]$summary.fixed) 
    model[[i]] <- model_i # store results
  }
  return(model)
}


create_empty.table <- function(isAll=FALSE) {
  #' Create an Empty Coefficients and RR Table
  #'
  #' This function creates an empty coefficients and risk ratios (RR) table for covariates.
  #'
  #' @param isAll A logical value indicating whether the table includes all provinces or only a province.
  #'
  #' @return An empty coefficients and RR table for provinces.

  columnames <- c("Covariates", "Coefficient (95% CrI)", "RR (95% CrI)", "Coefficient (95% CrI)", "RR (95% CrI)")
  
  if (isAll) { # if all provinces
    
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
  else { # if a subset of province
    
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
  #' Add INLA Results to a Coefficients and RR Table
  #'
  #' This function takes INLA results for "invasiones" and "defunciones," a predefined covariate table, and a logical value,
  #' and adds the results to the table.
  #'
  #' @param res_invasiones A list of INLA model results for "invasiones".
  #' @param res_defunciones A list of INLA model results for "defunciones".
  #' @param res_table A predefined covariate table data frame.
  #' @param isAll A logical value indicating whether the table includes all provinces or only a province.
  #'
  #' @return A coefficients and RR table with the INLA results added.
  
  if(isAll) { # if all provinces
    
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
      
      if (!(i %in% c(1, 10))) { # if not intercept or random effect
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
  else { # if a subset of province
    
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
  #' This function takes INLA results for cases and deaths, a predefined covariate table, and a filename,
  #' and adds the results to an Excel file.
  #'
  #' @param res_invasiones A list of INLA model results for cases.
  #' @param res_defunciones A list of INLA model results for deaths.
  #' @param res_table A predefined covariate table data frame.
  #' @param filename A character vector specifying the filename for the Excel file.
  #'
  #' @return An Excel file with the INLA results added.
  
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


head(df_distances) # distances data
head(df_colera.merged.month) # colera data

# merge colera data with distances data
df_colera_inla7 <- merge(df_colera.merged.month, df_distances, by = CODIGO_INE_STR)
df_colera_inla7 <- df_colera_inla7[, c(1, 6:7, 2, 3:5, 8:9, 10:17)] # reorder columns
head(df_colera_inla7)

# group by province and month
df_colera_inla7.provincias <- df_colera_inla7 %>% group_by(Provincia, Fecha) %>%  
  summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones), Total_poblacion = sum(Total_poblacion))


# maps --------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, MUNICIPIOS_SHAPEFILE, sep = "/"), quiet = TRUE)
mapS.provincias <- st_read(paste(SHAPES_DATA_DIR, PROVINCIAS_SHAPEFILE, sep = "/"), quiet = TRUE)
mapS.railways <- st_read(paste(SHAPES_DATA_DIR, RAILWAYS_SHAPEFILE, sep = "/"), quiet = TRUE)
mapS.railways <- na.omit(mapS.railways) # remove NA values
mapS.rivers <- st_read(paste(SHAPES_DATA_DIR, RIVERS_SHAPEFILE, sep = "/"), quiet = TRUE)

# remove Ceuta, Melilla, and the Balearic and Canary Islands from the maps
mapS.provincias <- subset(mapS.provincias, !(Cod_CCAA %in% c("04", "05", "18", "19")))
mapS.provincias$Texto <- tolower(stri_trans_general(mapS.provincias$Texto, "Latin-ASCII")) # format province names
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7" & CODNUT2 != "ES53")
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001)))

# merge mapS.provincias with df_colera_inla7.provincias
mapS.colera_inla7.provincias <- merge(mapS.provincias, df_colera_inla7.provincias, by.x = "Texto", by.y = PROVINCIA_STR)
mapS.colera_inla7.provincias$Texto_Alt <- NULL # remove redundant column
colnames(mapS.colera_inla7.provincias)[1:2] <- c(PROVINCIA_STR, paste0("Cod_", PROVINCIA_STR)) # rename columns
head(mapS.colera_inla7.provincias)

# merge mapS.municipios with df_colera_inla7
mapS.colera_inla7 <- merge(mapS.municipios, df_colera_inla7, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.colera_inla7)


# observed cases ----------------------------------------------------------


# by month

for (month in c(6, 7, 8, 9, 10, 11)) {
  
  map_invasiones.provincias <- create_tmap(mapS.colera_inla7.provincias[mapS.colera_inla7.provincias$Fecha == month,], c(MONTHS_STR[month-5]), mapS.provincias, TOTAL_INVASIONES_STR, "jenks")
  map_defunciones.provincias <- create_tmap(mapS.colera_inla7.provincias[mapS.colera_inla7.provincias$Fecha == month,], c(MONTHS_STR[month-5]), mapS.provincias, TOTAL_DEFUNCIONES_STR, "jenks")
  map_invasiones <- create_tmap(mapS.colera_inla7[mapS.colera_inla7$Fecha == month,], c(MONTHS_STR[month-5]), mapS.municipios, TOTAL_INVASIONES_STR, "jenks")
  map_defunciones <- create_tmap(mapS.colera_inla7[mapS.colera_inla7$Fecha == month,], c(MONTHS_STR[month-5]), mapS.municipios, TOTAL_DEFUNCIONES_STR, "jenks")

  # adding railway lines and rivers
  map_invasiones.provincias <- create_tmap.transport(map_invasiones.provincias)
  map_defunciones.provincias <- create_tmap.transport(map_defunciones.provincias)
  map_invasiones <- create_tmap.transport(map_invasiones)
  map_defunciones <- create_tmap.transport(map_defunciones)

  tmap_save(map_invasiones.provincias, filename = paste(COLERA_MAPS_DIR, paste0("tmap.provincias.", INVASIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_defunciones.provincias, filename = paste(COLERA_MAPS_DIR, paste0("tmap.provincias.", DEFUNCIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.", INVASIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.", DEFUNCIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
}


# all months

# group by municipality
df_colera_inla7.grouped <- df_colera_inla7 %>% group_by(`Codigo Ine`, Municipio, Total_poblacion) %>%
  summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))

# merge mapS.municipios with df_colera_inla7.grouped
mapS.colera_inla7.grouped <- merge(mapS.municipios, df_colera_inla7.grouped, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.colera_inla7.grouped)

# create maps
map_all_invasiones <- create_tmap(mapS.colera_inla7.grouped, NULL, mapS.municipios, TOTAL_INVASIONES_STR, "jenks") 
map_all_defunciones <- create_tmap(mapS.colera_inla7.grouped, NULL, mapS.municipios, TOTAL_DEFUNCIONES_STR, "jenks") 

# adding railway lines and rivers
map_all_invasiones <- create_tmap.transport(map_all_invasiones)
map_all_defunciones <- create_tmap.transport(map_all_defunciones)

tmap_save(map_all_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.", INVASIONES_STR, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
tmap_save(map_all_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.", DEFUNCIONES_STR, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# clean environment -------------------------------------------------------


rm(map_invasiones.provincias, map_defunciones.provincias, map_invasiones, map_defunciones, create_covariatesTS, generate_totalsByMonth, 
   df_colera_inla7, df_colera_inla7.provincias, df_colera_inla7.grouped, map_all_invasiones, map_all_defunciones, mapS.colera_inla7.provincias, mapS.colera_inla7.grouped)


# modelling ---------------------------------------------------------------


# all provinces
model_all <- run_inla(mapS.colera_inla7, "", COVALL_STR)
model_all[INVASIONES_STR]$summary.fixed
model_all[DEFUNCIONES_STR]$summary.fixed

# by province
model_zaragoza <- run_inla.provincias(PROVINCIAS_STR[1])
model_valencia <- run_inla.provincias(PROVINCIAS_STR[2])
model_granada <- run_inla.provincias(PROVINCIAS_STR[3])
model_murcia <- run_inla.provincias(PROVINCIAS_STR[4])
model_teruel <- run_inla.provincias(PROVINCIAS_STR[5])
model_castellon <- run_inla.provincias(PROVINCIAS_STR[6])
model_alicante <- run_inla.provincias(PROVINCIAS_STR[7])
model_navarra <- run_inla.provincias(PROVINCIAS_STR[8])
model_cuenca <- run_inla.provincias(PROVINCIAS_STR[9])
model_albacete <- run_inla.provincias(PROVINCIAS_STR[10])
model_almeria <- run_inla.provincias(PROVINCIAS_STR[11])


# results -----------------------------------------------------------------


# all provinces
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
res_table.almeria <-  add_results.table(model_almeria, model_almeria, res_table.almeria)
res_table.list <- list(res_table.zaragoza, res_table.valencia, res_table.granada, res_table.murcia, res_table.teruel, res_table.castellon, res_table.alicante, res_table.navarra, res_table.cuenca, res_table.albacete, res_table.almeria)
for (i in 1:length(PROVINCIAS_STR)) { write.xlsx(res_table.list[i], file = paste(COLERA_INLA_DIR, paste0("results_", PROVINCIAS_STR[i], ".xlsx"), sep = "/")) }

save.image("colera_inla.dist.RData")
