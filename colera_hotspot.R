# https://rpubs.com/heatherleeleary/hotspot_getisOrd_tut
# https://rpubs.com/quarcs-lab/spatial-autocorrelation


library(sf)
library(dplyr)
library(tmap)
library(sfdep)
library(spdep)
library(ggplot2)
library(tidyr)


load("colera_data.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
COLERA_DATA_DIR <- "colera_data"
dir.create(COLERA_DATA_DIR, showWarnings = FALSE)
COLERA_MAPS_DIR <- "colera_maps"
dir.create(COLERA_MAPS_DIR, showWarnings = FALSE)

MONTHS_INT <- c(6, 7, 8, 9, 10, 11)
MONTHS_STR <- c("June", "July", "August", "September", "October", "November")


# functions ---------------------------------------------------------------


create_tmap <- function(df_mes, mes, map, var_col, style, color = "Reds") {
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
  #' @param color The colour palette to be used for the map.
  #'
  #' @return A thematic map visualization.
  
  map.tmp <- tm_shape(df_mes, bbox = map) +
    tm_polygons(
      col = var_col,
      border.col = NULL,
      title = "",
      palette = color,
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


globalMoran_test <- function(df_colera, var_col, weights_matrix) {
  #' Perform global Moran's I test.
  #'
  #' This function performs a global Moran's I test to assess spatial autocorrelation of a variable.
  #'
  #' @param df_colera Data frame containing the variable of interest.
  #' @param var_col Column name of the variable to be tested.
  #' @param weights_matrix Spatial weights matrix for the test.
  #'
  #' @return A list containing the Moran's I statistic and p-value.
  
  # compute global Moran
  globalMoran <- moran.test(df_colera[[var_col]], weights_matrix)
  print(globalMoran)
  print(globalMoran[["estimate"]][["Moran I statistic"]])
  print(globalMoran[["p.value"]])
  return(globalMoran)
}


localMoran <- function(df_colera, var_col, neighbours, map)  {
  #' Perform local Moran's I test, visualize results, and return local Moran values.
  #'
  #' This function performs a local Moran's I test to assess spatial autocorrelation of a variable,
  #' visualizes the results on a map, and returns the computed local Moran values.
  #'
  #' @param df_colera Data frame containing the variable of interest.
  #' @param var_col Column name of the variable to be tested.
  #' @param neighbours Neighbours object defining spatial relationships.
  #' @param map Spatial map for visualization.
  #'
  #' @return A list containing the local Moran values.
  
  # weights matrix
  weights_matrix <- nb2listw(neighbours, style = "W")
  
  # local Moran scatter plot
  moran.plot(df_colera[[var_col]], listw = weights_matrix)
  
  # compute local Moran
  localMoran <- localmoran(x = df_colera[[var_col]], listw = weights_matrix)
  
  # plot local Moran
  moran.map <- cbind(df_colera, localMoran)
  moran.map.plot <- create_tmap(moran.map, NULL, map, "Ii", "quantile", "RdYlGn")
  tmap_save(moran.map.plot, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.hotspot.localMoran.", gsub("Total_", "", var_col), ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  return(localMoran)
} 


hotspots_classification <- function(hotspots) {
  #' Classify hotspots based on GI values and p-values.
  #'
  #' This function classifies hotspots based on their Getis-Ord Gi values and associated p-values.
  #'
  #' @param hotspots Data frame containing columns "gi" (Getis-Ord Gi values) and "p_folded_sim" (p-values of a folded permutation test).
  #'
  #' @return A modified data frame with an additional "classification" column.
  
  return(hotspots %>%
     # with the columns "gi" and "p_folded_sim"
     # "p_folded_sim" is the p-value of a folded permutation test
     mutate(
       # add a new column called "classification"
       classification = case_when(
         # classify based on the following criteria:
         gi > 0 & p_folded_sim <= 0.01 ~ "very high invasion risk",
         gi > 0 & p_folded_sim <= 0.05 ~ "high invasion risk",
         gi > 0 & p_folded_sim <= 0.1 ~ "moderate invasion risk",
         gi < 0 & p_folded_sim <= 0.01 ~ "no invasion risk",
         gi < 0 & p_folded_sim <= 0.05 ~ "very low invasion risk",
         gi < 0 & p_folded_sim <= 0.1 ~ "low invasion risk",
         TRUE ~ "insignificant"
       ),
       # convert "classification" into a factor for easier plotting
       classification = factor(
         classification,
         levels = c("very high invasion risk", "high invasion risk", "moderate invasion risk",
                    "insignificant",
                    "low invasion risk", "very low invasion risk", "no invasion risk")
      )
    )
  )
}
  

# main --------------------------------------------------------------------


# data --------------------------------------------------------------------


head(df_distances)
head(df_colera.merged.month)


# merge df_distances with df_colera.merged.month as df_colera_hotspot

df_colera_hotspot <- merge(df_colera.merged.month, df_distances, by = CODIGO_INE_STR)[, c(1:9)]
df_colera_hotspot <- df_colera_hotspot[, c(1, 6:7, 2, 3:5, 8:9)]
head(df_colera_hotspot)


# map ---------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7" & CODNUT2 != "ES53") 
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) 
head(mapS.municipios)


# merge mapS.municipios with df_colera_hotspot

mapS.colera_hotspot <- merge(mapS.municipios, df_colera_hotspot, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.colera_hotspot)


# spatial distribution ----------------------------------------------------


boxplot(list(df_colera_hotspot$Total_invasiones, df_colera_hotspot$Total_defunciones), names = c(TOTAL_INVASIONES_STR, TOTAL_DEFUNCIONES_STR), 
        main = "Boxplot of Cholera Data", ylab = "Values")


# by month

# for (month in MONTHS_INT) {
# 
#   map_invasiones <- create_tmap(mapS.colera_hotspot[mapS.colera_hotspot$Fecha == month,], c(MONTHS_STR[month-5]), mapS.municipios, TOTAL_INVASIONES_STR, "jenks") +
#     tm_compass(type = "8star", position = c("right", "top")) +
#     tm_scale_bar(breaks = c(0, 25, 50, 100), text.size = 1, position = c("center", "bottom"))
# 
#   map_defunciones <- create_tmap(mapS.colera_hotspot[mapS.colera_hotspot$Fecha == month,], c(MONTHS_STR[month-5]), mapS.municipios, TOTAL_DEFUNCIONES_STR, "jenks") +
#     tm_compass(type = "8star", position = c("right", "top")) +
#     tm_scale_bar(breaks = c(0, 25, 50, 100), text.size = 1, position = c("center", "bottom"))
# 
#   tmap_save(map_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.", INVASIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
#   tmap_save(map_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.", DEFUNCIONES_STR, ".", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
# }


# totals 

df_colera_hotspot.grouped <- df_colera_hotspot %>% group_by(`Codigo Ine`, Municipio, Total_poblacion) %>% summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))
mapS.colera_hotspot.grouped <- merge(mapS.municipios, df_colera_hotspot.grouped, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.colera_hotspot.grouped)

# map_all_invasiones <- create_tmap(mapS.colera_hotspot.grouped, NULL, mapS.municipios, TOTAL_INVASIONES_STR, "jenks") +
#   tm_shape(mapS.colera_hotspot.grouped[mapS.colera_hotspot.grouped$Total_invasiones > 3915, ]) + tm_text(CODIGOINE_STR, size = 1.3, xmod = 2, fontface = "bold") +
#   tm_compass(type = "8star", position = c("right", "top")) +
#   tm_scale_bar(breaks = c(0, 25, 50, 100), text.size = 1, position = c("center", "bottom"))
# 
# map_all_defunciones <- create_tmap(mapS.colera_hotspot.grouped, NULL, mapS.municipios, TOTAL_DEFUNCIONES_STR, "jenks") +
#   tm_shape(mapS.colera_hotspot.grouped[mapS.colera_hotspot.grouped$Total_defunciones > 1818, ]) + tm_text(CODIGOINE_STR, size = 1.3, xmod = 2, fontface = "bold") +
#   tm_compass(type = "8star", position = c("right", "top")) +
#   tm_scale_bar(breaks = c(0, 25, 50, 100), text.size = 1, position = c("center", "bottom"))
# 
# tmap_save(map_all_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.", INVASIONES_STR, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
# tmap_save(map_all_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.", DEFUNCIONES_STR, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")

map_all_invasiones <- create_tmap(mapS.colera_hotspot.grouped, NULL, mapS.municipios, TOTAL_INVASIONES_STR, "quantile") 
map_all_defunciones <- create_tmap(mapS.colera_hotspot.grouped, NULL, mapS.municipios, TOTAL_DEFUNCIONES_STR, "quantile") 
tmap_save(map_all_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.hotspot.", INVASIONES_STR, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
tmap_save(map_all_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.hotspot.", DEFUNCIONES_STR, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# clean environment -------------------------------------------------------


rm(map_invasiones, map_defunciones, df_colera_hotspot, df_colera_hotspot.grouped, generate_totalsByMonth, create_covariatesTS)


# neighbour structure ----------------------------------------------------


# queen neighbours

neighbours <- poly2nb(mapS.colera_hotspot.grouped) 
neighbours

empty_neighbours <- which(card(neighbours) == 0)
empty_neighbours


# rook neighbours

neighbours2 <- poly2nb(mapS.colera_hotspot.grouped, queen = FALSE) 
neighbours2

empty_neighbours <- which(card(neighbours2) == 0)
empty_neighbours


# remove polygons with empty neighbour sets from mapS.colera_hotspot.grouped
mapS.colera_hotspot.grouped <- mapS.colera_hotspot.grouped[-empty_neighbours, ]


# global spatial autocorrelation ------------------------------------------


neighbours2 <- poly2nb(mapS.colera_hotspot.grouped, queen = FALSE) 
neighbours2


# weights matrix

listw <- nb2listw(neighbours2) 
listw


# global Moran test

globalMoran_test.invasiones <- globalMoran_test(mapS.colera_hotspot.grouped, TOTAL_INVASIONES_STR, listw)
globalMoran_test.defunciones <- globalMoran_test(mapS.colera_hotspot.grouped, TOTAL_DEFUNCIONES_STR, listw)


# local spatial autocorrelation -------------------------------------------


localMoran.invasiones <- localMoran(mapS.colera_hotspot.grouped, TOTAL_INVASIONES_STR, neighbours2, mapS.municipios)
localMoran.defunciones <- localMoran(mapS.colera_hotspot.grouped, TOTAL_DEFUNCIONES_STR, neighbours2, mapS.municipios)


# Getis-Ord approach ------------------------------------------------------


# identify neighbours with queen contiguity (edge/vertex touching)

nb <- poly2nb(mapS.colera_hotspot.grouped, queen = TRUE)
nb


# binary weighting assigns a weight of 1 to all neighbouring features and a weight of 0 to all other features

w_binary <- nb2listw(nb, style = "B")


# calculate spatial lag for "invasiones" and "defunciones"

spatial_lag.i <- lag.listw(w_binary, mapS.colera_hotspot.grouped$Total_invasiones)
spatial_lag.d <- lag.listw(w_binary, mapS.colera_hotspot.grouped$Total_defunciones)


# test for global G statistic of "invasiones" and "defunciones"

globalG.test(mapS.colera_hotspot.grouped$Total_invasiones, w_binary)
globalG.test(mapS.colera_hotspot.grouped$Total_defunciones, w_binary)


# local Gi test -----------------------------------------------------------


# test for local spatial autocorrelation (hotspots) -----------------------


# identify neighbours, create weights, calculate spatial lag

nbs.i <- mapS.colera_hotspot.grouped |> 
  mutate(
    nb = st_contiguity(geometry),                 # neighbours share border/vertex
    wt = st_weights(nb),                          # row-standardized weights
    tes_lag = st_lag(Total_invasiones, nb, wt)    # calculate spatial lag of "invasiones"
  ) 

nbs.d <- mapS.colera_hotspot.grouped |> 
  mutate(
    nb = st_contiguity(geometry),                 
    wt = st_weights(nb),                          
    tes_lag = st_lag(Total_defunciones, nb, wt)     # calculate spatial lag of "defunciones"
  ) 


# calculate the Gi using local_g_perm

hot_spots.i <- nbs.i |> 
  mutate(
    Gi = local_g_perm(Total_invasiones, nb, wt, nsim = 999)
    # nsim = number of Monte Carlo simulations (999 is default)
  ) |> 
  # new "Gi" column itself contains a data frame 
  # can't work with that, need to "unnest" it
  unnest(Gi)

hot_spots.d <- nbs.d |> 
  mutate(
    Gi = local_g_perm(Total_defunciones, nb, wt, nsim = 999)
  ) |> 
  unnest(Gi)


map_all_invasiones <- create_tmap(hot_spots.i, NULL, mapS.municipios, "gi", "jenks", "RdYlGn") 
map_all_defunciones <- create_tmap(hot_spots.d, NULL, mapS.municipios, "gi", "jenks", "RdYlGn") 
tmap_save(map_all_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.hotspot.gi.", INVASIONES_STR, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
tmap_save(map_all_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.hotspot.gi.", DEFUNCIONES_STR, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# hotspots classification

hot_spots.i <- hotspots_classification(hot_spots.i)
hot_spots.d <- hotspots_classification(hot_spots.d)


# visualize the classification

map_all_invasiones <- create_tmap(hot_spots.i, NULL, mapS.municipios, "classification", "pretty", "RdBu") 
map_all_defunciones <- create_tmap(hot_spots.d, NULL, mapS.municipios, "classification", "pretty", "RdBu") 
tmap_save(map_all_invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.hotspot.", INVASIONES_STR, "_classification.png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
tmap_save(map_all_defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap_all.hotspot.", DEFUNCIONES_STR, "_classification.png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# save results

df_hot_spots.i <- hot_spots.i %>%
  select( CODIGOINE, NAMEUNIT, Total_invasiones, Total_defunciones, Total_poblacion,
    nb, wt, gi, e_gi, var_gi, p_value, p_sim, p_folded_sim, skewness, kurtosis, classification)

df_hot_spots.d <- hot_spots.d %>%
  select(CODIGOINE, NAMEUNIT, Total_invasiones, Total_defunciones, Total_poblacion,
    nb, wt, gi, e_gi, var_gi, p_value, p_sim, p_folded_sim, skewness, kurtosis, classification)

df_hot_spots.i$nb <- sapply(df_hot_spots.i$nb, function(x) paste(x, collapse = ", "))
df_hot_spots.i$wt <- sapply(df_hot_spots.i$wt, function(x) paste(x, collapse = ", "))
df_hot_spots.d$nb <- sapply(df_hot_spots.d$nb, function(x) paste(x, collapse = ", "))
df_hot_spots.d$wt <- sapply(df_hot_spots.d$wt, function(x) paste(x, collapse = ", "))

df_hot_spots.i$geometry <- NULL
df_hot_spots.d$geometry <- NULL

df_hot_spots.i$NAMEUNIT <- tolower(iconv(df_hot_spots.i$NAMEUNIT, from = "UTF-8", to = "ASCII//TRANSLIT"))
df_hot_spots.d$NAMEUNIT <- tolower(iconv(df_hot_spots.d$NAMEUNIT, from = "UTF-8", to = "ASCII//TRANSLIT"))

df_hot_spots.i <- df_hot_spots.i[order(df_hot_spots.i$classification),]
df_hot_spots.d <- df_hot_spots.d[order(df_hot_spots.d$classification),]

write.csv(df_hot_spots.i, paste(COLERA_DATA_DIR, "colera_hotspots.invasiones.csv", sep = "/"), row.names = FALSE)
write.csv(df_hot_spots.d, paste(COLERA_DATA_DIR, "colera_hotspots.defunciones.csv", sep = "/"), row.names = FALSE)


# clean environment -------------------------------------------------------


rm(map_all_invasiones, map_all_defunciones, listw, localMoran.defunciones, localMoran.invasiones, nb, nbs.d, nbs.i, neighbours, neighbours2, w_binary, empty_neighbours)
