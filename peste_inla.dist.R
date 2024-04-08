library(dplyr)
library(readxl)
library(INLA)
library(tmap)
library(sf)
library(reshape2)
library(spdep)
library(ggplot2)

load("peste_data.RData") # load peste data


# constants ---------------------------------------------------------------


DISTANCES_FILE <- "_Results.xlsx"
MUNICIPIOS_SHAPEFILE <- "Municipios_IGN.shp"

DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
PESTE_MAPS_DIR <- "peste_maps"
dir.create(PESTE_MAPS_DIR, showWarnings = FALSE)

MUNICIPIO_STR <- "Municipio"
NAMEUNIT_STR <- "NAMEUNIT"
COVROAD_STR <- "covdist_road"
COVPORT_STR <- "covdist_port"
COVALL_STR <- c(COVROAD_STR, COVPORT_STR)
LOCALIDADES_STR <- c("Artà", "Capdepera", "Sant Llorenç des Cardassar", "Son Servera")
CASOS_STR <- "Casos"
CASES_BREAKS <- c(0, 10, 68, 450, 652, 3178)
DEATHS_BREAKS <- c(0, 10, 54, 97, 184, 228)
COEFFICIENTS <- c("mean", "0.025quant", "0.975quant")


# functions ---------------------------------------------------------------


run_inla <- function(mapS, covariate, isPred = FALSE) {
  #' Run INLA
  #'
  #' This function runs INLA.
  #'
  #' @param mapS A data frame containing peste data.
  #' @param covariate A string containing the name of the covariate.
  #' @param isPred A boolean indicating whether the model is a prediction or not.
  #'
  #' @return A list containing the INLA model.

  # neighborhood matrix
  nb <- poly2nb(mapS)
  nb2INLA("map.adj", nb)
  g <- inla.read.graph(filename = "map.adj")

  # model formula
  mapS$idarea <- as.numeric(as.factor(mapS$NAMEUNIT))
  mapS$idarea1 <- mapS$idarea
  mapS$idtime <- 1 + mapS$mes - min(mapS$mes)
  covariates <- c(covariate, "f(idarea, model = 'bym', graph = g)", "f(idarea1, idtime, model = 'iid')", "idtime")
  formula <- as.formula(paste(paste(CASOS_STR, "~"), paste(covariates, collapse = " + ")))
  print(formula)

  # inla() call for prediction or relative risk
  if (isPred) { res <- inla(formula, family = "poisson", data = mapS, control.predictor = list(link = 1), verbose = TRUE) } # prediction
  else { res <- inla(formula, family = "poisson", data = mapS, control.predictor = list(compute = TRUE), control.compute = list(return.marginals = TRUE), verbose = TRUE) } # relative risk
  print(res)
  return(res)
}


generate_maps <- function (mapS.peste, columnvar, outputvar, breaks) {
  #' Generate Maps for Each Month
  #'
  #' This function generates maps for each month.
  #'
  #' @param mapS.peste A data frame containing peste data.
  #' @param columnvar A string containing the name of the column variable to map.
  #' @param outputvar A string containing the name of the output file to save the map.

  for(month in c(6, 7, 8, 9, 10)) { # for each month

    map_tmp <- tm_shape(mapS.peste[mapS.peste$mes == month,], bbox = mapS.municipios.mallorca) +
        tm_polygons(
          col = columnvar,
          border.col = NULL,
          title = "",
          palette = "Reds",
          breaks = breaks, # breaks for the colour palette (manually set)
          style = "fixed"
        ) +
        tm_shape(mapS.municipios.mallorca) + tm_borders() +
        tm_layout(
          legend.position =  c("right", "bottom"), legend.text.size = 1.5,
          inner.margins = c(0, 0, 0, 0),
          panel.labels = c(c("June", "July", "August", "September", "October")[month-5]),
          panel.label.size = 1.5, panel.label.height = 1.1, panel.label.color = "black", panel.label.bg.color = "gray"
        )

    outputfile <- paste0("tmap.municipios.", columnvar, ".", month, outputvar, ".png")
    tmap_save(map_tmp, filename = paste(PESTE_MAPS_DIR, outputfile, sep = "/"), width = 20, height = 10, dpi = 300, units = "in") # save map
  }
}


create_empty.table <- function() {
  #' Create Empty Table for Relative Risk Results
  #'
  #' This function creates an empty table for relative risk results.

  columnames <- c("Covariates", "Coefficient (95% CrI)", "RR (95% CrI)", "Coefficient (95% CrI)", "RR (95% CrI)")
  table <- data.frame(matrix(ncol = 5, nrow = 4))
  colnames(table) <- columnames
  table[1,1] <- "Intercept*"
  table[2,1] <- COVROAD_STR
  table[3,1] <- COVPORT_STR
  table[4,1] <- "Unstructured random effect"
  return(table)
}


add_results.table <- function(res_invasiones, res_defunciones, res_table, nrows) {
  #' Add Results to Table
  #'
  #' This function adds the results of the relative risk to a table.
  #'
  #' @param res_invasiones A list containing the results of the relative risk for cases.
  #' @param res_defunciones A list containing the results of the relative risk for deaths.
  #' @param res_table A data frame containing an empty table to write the relative risk results.
  #' @param nrows An integer indicating the number of rows in the table.
  #'
  #' @return A data frame containing the table of relative risk results.

  for(i in 1:nrows) {

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

    if (!(i %in% c(1, nrows))) {
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


# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


# replace NA values with 0
df_peste <- df_peste %>% mutate(Casos = replace(Casos, is.na(Casos), 0))
df_peste.defunciones <- df_peste.defunciones %>% mutate(Casos = replace(Casos, is.na(Casos), 0))

# group by month
df_peste <- df_peste %>% group_by(Municipio, mes, Poblacion) %>% summarise(Casos = sum(Casos))
df_peste.defunciones <- df_peste.defunciones %>% group_by(Municipio, mes, Poblacion) %>% summarise(Casos = sum(Casos))

# read distances data and replace "Sant Llorenç des Cardass" with "Sant Llorenç des Cardassar"
df_distances <- read_excel(paste(DATA_DIR, DISTANCES_FILE, sep = "/"), sheet = "Sheet5")
df_distances <- df_distances %>%
  mutate(
    MUNICIPIO = case_when(
      MUNICIPIO == "Sant Llorenç des Cardass" ~ LOCALIDADES_STR[3],
      TRUE ~ MUNICIPIO
    )
  )

df_distances <- df_distances[, !(names(df_distances) %in% c("OD_Id", "CD_INE5", "ALTURA", "POP_1877"))] # remove unnecessary columns
colnames(df_distances)[3:5] <- c(MUNICIPIO_STR, COVROAD_STR, COVPORT_STR) # rename columns to match peste data

# merge peste data with distances data 
df_peste <- merge(df_peste, df_distances, by = MUNICIPIO_STR)
df_peste.defunciones <- merge(df_peste.defunciones, df_distances, by = MUNICIPIO_STR)


# maps --------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, MUNICIPIOS_SHAPEFILE, sep = "/"), quiet = TRUE)
mapS.municipios.mallorca <- subset(mapS.municipios, CODNUT3 == "ES532") # only Mallorca municipalities 
mapS.municipios.mallorca.localidades <- subset(mapS.municipios.mallorca, NAMEUNIT %in% LOCALIDADES_STR) # only Mallorca municipalities with black plague cases
mapS.municipios.mallorca.notlocalidades <- subset(mapS.municipios.mallorca, !(NAMEUNIT %in% LOCALIDADES_STR)) # only Mallorca municipalities without black plague cases

# merge peste data with map data of Mallorca municipalities with black plague cases
mapS.peste_inla7 <- merge(mapS.municipios.mallorca.localidades, df_peste, by.x = NAMEUNIT_STR, by.y = MUNICIPIO_STR)
mapS.peste_inla7.defunciones <- merge(mapS.municipios.mallorca.localidades, df_peste.defunciones, by.x = NAMEUNIT_STR, by.y = MUNICIPIO_STR)
head(mapS.peste_inla7)
head(mapS.peste_inla7.defunciones)

rm(mapS.municipios)


# observed cases ----------------------------------------------------------


# generate a maps of observed cases and deaths by each month for each municipality
generate_maps(mapS.peste_inla7, CASOS_STR, ".invasiones", CASES_BREAKS)
generate_maps(mapS.peste_inla7.defunciones, CASOS_STR, ".defunciones", DEATHS_BREAKS)


# modelling relative risk --------------------------------------------------


# run inla for all covariates/municipalities on cases and deaths
model_all.invasiones <- run_inla(mapS.peste_inla7, COVALL_STR, FALSE)
model_all.defunciones <- run_inla(mapS.peste_inla7.defunciones, COVALL_STR, FALSE)
model_all.invasiones$summary.fixed
model_all.defunciones$summary.fixed

# create a table of relative risk results for all covariates/municipalities on cases and deaths
res_table <- create_empty.table()
res_table <- add_results.table(model_all.invasiones, model_all.defunciones, res_table, 4)

# marginal effects
model_all.invasiones$marginals.fixed
model_all.defunciones$marginals.fixed
model_all.invasiones.marginals <- reshape2:::melt(model_all.invasiones$marginals.fixed) %>% reshape2:::dcast(L1+Var1~Var2, value = "value")
model_all.defunciones.marginals <- reshape2:::melt(model_all.defunciones$marginals.fixed) %>% reshape2:::dcast(L1+Var1~Var2, value = "value")
ggplot(model_all.invasiones.marginals, aes(y = y, x = x)) + geom_line() + facet_wrap(~L1, scales = "free", nrow = 1) + theme_classic()
ggplot(model_all.defunciones.marginals, aes(y = y, x = x)) + geom_line() + facet_wrap(~L1, scales = "free", nrow = 1) + theme_classic()

# save relative risk results for each covariate in all municipalities on cases and deaths to map data
mapS.peste_inla7$RR <- model_all.invasiones$summary.fitted.values[, COEFFICIENTS[1]]
mapS.peste_inla7$LL <- model_all.invasiones$summary.fitted.values[, COEFFICIENTS[2]]
mapS.peste_inla7$UL <- model_all.invasiones$summary.fitted.values[, COEFFICIENTS[3]]
mapS.peste_inla7.defunciones$RR <- model_all.defunciones$summary.fitted.values[, COEFFICIENTS[1]]
mapS.peste_inla7.defunciones$LL <- model_all.defunciones$summary.fitted.values[, COEFFICIENTS[2]]
mapS.peste_inla7.defunciones$UL <- model_all.defunciones$summary.fitted.values[, COEFFICIENTS[3]]

# generate a maps of relative risk cases and deaths by each month for each municipality
generate_maps(mapS.peste_inla7, "RR", ".invasiones", CASES_BREAKS)
generate_maps(mapS.peste_inla7.defunciones, "RR", ".defunciones", DEATHS_BREAKS)

rm(model_all.invasiones.marginals, model_all.defunciones.marginals)


# prediction --------------------------------------------------------------


# create new data for prediction with municipalities without black plague cases or deaths as NA
newdata <- expand.grid(Municipio = unique(mapS.municipios.mallorca.notlocalidades$NAMEUNIT), mes = unique(df_peste$mes), Casos = NA)
newdata.invasiones <- rbind(df_peste[, c(1:2,4)], newdata)
newdata.defunciones <- rbind(df_peste.defunciones[, c(1:2,4)], newdata)

# merge new data with distances data (covdist_road and covdist_port)
newdata.invasiones <- merge(newdata.invasiones, df_distances[, c(3:5)], by = MUNICIPIO_STR)
newdata.defunciones <- merge(newdata.defunciones, df_distances[, c(3:5)], by = MUNICIPIO_STR)

# merge new data of cases and deaths to map data of Mallorca
mapS.peste_inla7.invasiones.pred <- merge(mapS.municipios.mallorca, newdata.invasiones, by.x = NAMEUNIT_STR, by.y = MUNICIPIO_STR)
mapS.peste_inla7.defunciones.pred <- merge(mapS.municipios.mallorca, newdata.defunciones, by.x = NAMEUNIT_STR, by.y = MUNICIPIO_STR)
head(mapS.peste_inla7.invasiones.pred)
head(mapS.peste_inla7.defunciones.pred)

rm(newdata, newdata.invasiones, newdata.defunciones, df_distances)

# run inla for prediction on cases and deaths
model_all.invasiones.pred <- run_inla(mapS.peste_inla7.invasiones.pred, COVALL_STR, TRUE)
model_all.defunciones.pred <- run_inla(mapS.peste_inla7.defunciones.pred, COVALL_STR, TRUE)

# save prediction results for cases and deaths to map data
mapS.peste_inla7.invasiones.pred$pred_mean <- model_all.invasiones.pred$summary.fitted.values[, COEFFICIENTS[1]]
mapS.peste_inla7.invasiones.pred$pred_lower <- model_all.invasiones.pred$summary.fitted.values[, COEFFICIENTS[2]]
mapS.peste_inla7.invasiones.pred$pred_upper <- model_all.invasiones.pred$summary.fitted.values[, COEFFICIENTS[3]]
mapS.peste_inla7.defunciones.pred$pred_mean <- model_all.defunciones.pred$summary.fitted.values[, COEFFICIENTS[1]]
mapS.peste_inla7.defunciones.pred$pred_lower <- model_all.defunciones.pred$summary.fitted.values[, COEFFICIENTS[2]]
mapS.peste_inla7.defunciones.pred$pred_upper <- model_all.defunciones.pred$summary.fitted.values[, COEFFICIENTS[3]]

# generate a maps of prediction cases and deaths by each month
generate_maps(mapS.peste_inla7.invasiones.pred, "pred_mean", ".invasiones", CASES_BREAKS)
generate_maps(mapS.peste_inla7.defunciones.pred, "pred_mean", ".defunciones", DEATHS_BREAKS)
