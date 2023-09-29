# https://rpubs.com/heatherleeleary/hotspot_getisOrd_tut
# https://rpubs.com/quarcs-lab/spatial-autocorrelation


library(sf)
library(dplyr)
library(tmap)
library(sfdep)
library(spdep)
library(ggplot2)
library(tidyr)


# load("colera_data.RData")
# load("distances.RData")


# constants ---------------------------------------------------------------


SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)
COLERA_DATA_DIR <- "colera_data"
dir.create(COLERA_DATA_DIR, showWarnings = FALSE)
COLERA_MAPS_DIR <- "colera_maps"
dir.create(COLERA_MAPS_DIR, showWarnings = FALSE)

LONG_STR <- "long"
LAT_STR <- "lat"
CODIGOINE_STR <- "CODIGOINE"
NAMEUNIT_STR <- "NAMEUNIT"
INVASIONES_FACTOR_STR <- paste0(INVASIONES_STR, "_factor")
DEFUNCIONES_FACTOR_STR <- paste0(DEFUNCIONES_STR, "_factor")

MONTHS_INT <- c(6, 7, 8, 9, 10, 11)
PLOT_LABELS <- c("low", "mid-low", "mid-high", "high")
PLOT_COLORS_BY_LABELS <- c("low" = "#FEE4D8", "mid-low" = "#FCB195", "mid-high" = "#FB795A", "high" = "#BB1419")


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
  
  df_colera[[factor_col]] <- cut(df_colera[[var_col]], breaks = breaks, labels = PLOT_LABELS)
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
  
  ggplot() +
    geom_sf(data = mapS.municipios, aes(), fill = "white", color = "black") +
    geom_sf(data = df_colera, aes(col = !!sym(var_col), fill = !!sym(var_col)), color = "black") +
    scale_fill_manual(values = PLOT_COLORS_BY_LABELS) +
    theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
    labs(color = var_colname, fill = var_colname) +
    theme_void()
  
  ggsave(paste(COLERA_MAPS_DIR, paste0("map_hotspot.", var_col, ".png"), sep = "/"), dpi = 300, limitsize = TRUE)
  
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
  
  for (month in MONTHS_INT) {  
    
    month_data <- df_colera[df_colera$Fecha == month, ]
    
    plot <- ggplot() +
      geom_sf(data = mapS.municipios, aes(), fill = "white", color = "black") +
      geom_sf(data = month_data, aes(col = !!sym(var_col), fill = !!sym(var_col)), color = "black") +
      scale_fill_manual(values = PLOT_COLORS_BY_LABELS) +
      theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
      labs(color = var_colname, fill = var_colname) +
      theme_void() +
      ggtitle(paste("month:", month))  # add month-specific title
    
    plot_list[[as.character(month)]] <- plot
    
  }
  
  return(plot_list)
  
}


create_tmap <- function(df_mes, map, var_col, legend_title, style, coords) {
  #' Create a thematic map using tmap package.
  #'
  #' This function creates a thematic map using the tmap package based on the provided data frame,
  #' map object, variable column, legend title, style and coords.
  #'
  #' @param df_mes Data frame containing the data to be mapped.
  #' @param map Spatial object representing the map background.
  #' @param var_col Column containing the variable to be mapped.
  #' @param legend_title Title for the legend.
  #' @param style Style for mapping (e.g., "cat" for categorical).
  #' @param coords Spatial boundaries based on coordinates.
  #'
  #' @return A thematic map.
  
  return(
    tm_shape(df_mes) +
      tm_polygons(
        col = var_col,
        border.col = NULL,
        title = legend_title,
        palette = "Reds",
        style = style
      ) +
      tm_shape(map) + tm_borders() +
      tm_layout(
        legend.position =  c("right", "bottom"),
        inner.margins = c(0, 0, 0, 0)
      ) +
      tm_view(bbox = coords)
  )
}


create_tmapByFecha <- function(df_mes, map, var_col, legend_title, style, coords, nrows) {
  #' Create a thematic map with facets by date using tmap package.
  #'
  #' This function creates a thematic map with facets by date using the tmap package.
  #' It is designed to display temporal data on multiple map panels.
  #'
  #' @param df_mes Data frame containing the data to be mapped.
  #' @param map Spatial object representing the map background.
  #' @param var_col Column containing the variable to be mapped.
  #' @param legend_title Title for the legend.
  #' @param style Style for mapping (e.g., "cat" for categorical).
  #' @param coords Spatial boundaries based on coordinates.
  #' @param nrows Number of rows for arranging facets.
  #'
  #' @return A thematic map with facets.
  
  return(
    tm_shape(df_mes) +
      tm_polygons(
        col = var_col,
        border.col = NULL,
        title = legend_title,
        palette = "Reds",
        style = style
      ) +
      tm_facets(
        by = FECHA_STR,
        nrow = nrows,
        free.coords = FALSE
      ) +
      tm_shape(map) + tm_borders() +
      tm_layout(
        legend.position =  c("right", "bottom"),
        inner.margins = c(0, 0, 0, 0)
      ) +
      tm_view(bbox = coords)
  )
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
  print(
    tm_shape(moran.map) +
      tm_polygons(
        col = "Ii",
        border.col = NULL,
        title = "local moran statistic",
        style = "quantile"
      ) +
      tm_shape(map) + tm_borders() +
      tm_layout(
        legend.position =  c("right", "bottom"),
        inner.margins = c(0, 0, 0, 0)
      ) +
      tm_view(bbox = moran.map)
  ) 
  
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


# explore raw data --------------------------------------------------------


df_covdist <- df_distances[, c(1, 3:6, 9:12)]
df_colera.merged.month <- df_colera.merged.month[, c(1, 4:9)]
df_colera.merged.month$`Codigo Ine` <- as.numeric(df_colera.merged.month$`Codigo Ine`)
head(df_covdist)
head(df_colera.merged.month)


# merge df_covdist with df_colera.merged.month as df_colera_hotspot

df_colera_hotspot <- merge(df_colera.merged.month, df_covdist, by.x = CODIGO_INE_STR, by.y = "COD_INE")
df_colera_hotspot <- df_colera_hotspot[, c(1, 8:9, 2, 3:7, 12:15, 10:11)]
colnames(df_colera_hotspot)[c(2:3, 14:15)] <- c(PROVINCIA_STR, MUNICIPIO_STR, LONG_STR, LAT_STR)
head(df_colera_hotspot)


# clean environment -------------------------------------------------------


rm(df_distances, df_covdist)


# map ---------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7") # remove Canary Islands
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) # remove "Ceuta" and "Melilla"
head(mapS.municipios)


# observations ------------------------------------------------------------


# merge mapS.municipios and df_colera_hotspot

df_colera_hotspot <- merge(mapS.municipios, df_colera_hotspot, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(df_colera_hotspot)


# box plot of "Total_invasiones" and "Total_defunciones"

boxplot(list(df_colera_hotspot$Total_invasiones, df_colera_hotspot$Total_defunciones),
  names = c(TOTAL_INVASIONES_STR, TOTAL_DEFUNCIONES_STR),
  main = "Boxplot of Cholera Data",
  ylab = "Values")


# factorize "Total_invasiones" and "Total_defunciones"

df_colera_hotspot <- factorize(
  df_colera_hotspot,
  TOTAL_INVASIONES_STR,
  INVASIONES_FACTOR_STR,
  c(-1, 10, 50, 90, Inf) 
)

df_colera_hotspot <- factorize(
  df_colera_hotspot,
  TOTAL_DEFUNCIONES_STR,
  DEFUNCIONES_FACTOR_STR,
  c(-1, 10, 50, 90, Inf)  
)

plot_observations(df_colera_hotspot, INVASIONES_FACTOR_STR, TOTAL_INVASIONES_STR)
plot_observations(df_colera_hotspot, DEFUNCIONES_FACTOR_STR, TOTAL_DEFUNCIONES_STR)


# for each month

plot_list.invasiones <- plot_observationsByMonth(df_colera_hotspot, INVASIONES_FACTOR_STR, TOTAL_INVASIONES_STR)
plot_list.defunciones <- plot_observationsByMonth(df_colera_hotspot, DEFUNCIONES_FACTOR_STR, TOTAL_DEFUNCIONES_STR)

for (month in names(plot_list.invasiones)) { # TODO: change type of plots ???
  # print(plot_list.invasiones[[month]])
  # print(plot_list.defunciones[[month]])
  ggsave(paste0(COLERA_MAPS_DIR, "/map_hotspot.invasionesXmunicipios_", month, ".png"), plot_list.invasiones[[month]])
  ggsave(paste0(COLERA_MAPS_DIR, "/map_hotspot.defuncionesXmunicipios_", month, ".png"), plot_list.defunciones[[month]])
  
}

df_colera_hotspot$invasiones_factor <- NULL
df_colera_hotspot$defunciones_factor <- NULL


# visualize "Total_invasiones" and "Total_defunciones" across neighbourhoods 

breaksinvasiones <- c(412, 677, 1189, 293, 71, 20)
breaksdefunciones <- c(98, 342, 457, 93, 40, 8.8)
  
for (month in MONTHS_INT) {
  
  map.invasiones <-
    create_tmap(
      df_colera_hotspot[df_colera_hotspot$Fecha == month,],
      mapS.municipios,
      TOTAL_INVASIONES_STR,
      TOTAL_INVASIONES_STR,
      "jenks",
      c(min(df_colera_hotspot$long), min(df_colera_hotspot$lat), max(df_colera_hotspot$long), max(df_colera_hotspot$lat))
    ) +
    tm_shape(df_colera_hotspot[df_colera_hotspot$Total_invasiones > breaksinvasiones[month - 5],]) + tm_text(CODIGOINE_STR, size = 0.6) +
    tm_compass(type = "8star", position = c("right", "top")) +
    tm_scale_bar(breaks = c(0, 25, 50, 100), text.size = 1, position = c("center", "bottom"))

  map.defunciones <-
    create_tmap(
      df_colera_hotspot[df_colera_hotspot$Fecha == month,],
      mapS.municipios,
      TOTAL_DEFUNCIONES_STR,
      TOTAL_DEFUNCIONES_STR,
      "jenks",
      c(min(df_colera_hotspot$long), min(df_colera_hotspot$lat), max(df_colera_hotspot$long), max(df_colera_hotspot$lat))
    ) +
    tm_shape(df_colera_hotspot[df_colera_hotspot$Total_defunciones > breaksdefunciones[month - 5],]) + tm_text(CODIGOINE_STR, size = 0.6) +
    tm_compass(type = "8star", position = c("right", "top")) +
    tm_scale_bar(breaks = c(0, 25, 50, 100), text.size = 1, position = c("center", "bottom"))

  tmap_save(map.invasiones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.invasiones", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
  tmap_save(map.defunciones, filename = paste(COLERA_MAPS_DIR, paste0("tmap.defunciones", month, ".png"), sep = "/"), width = 20, height = 10, dpi = 300, units = "in")

}

map.invasiones611 <- 
  create_tmapByFecha(
    df_colera_hotspot,
    mapS.municipios,
    TOTAL_INVASIONES_STR,
    TOTAL_INVASIONES_STR,
    "jenks",
    c(min(df_colera_hotspot$long), min(df_colera_hotspot$lat), max(df_colera_hotspot$long), max(df_colera_hotspot$lat)),
    2
  ) + 
  # tm_shape(df_colera_hotspot) + tm_text(CODIGOINE_STR, size = 0.7) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 25, 50, 100), text.size = 1, position = c("center", "bottom"))

map.defunciones611 <- 
  create_tmapByFecha(
    df_colera_hotspot,
    mapS.municipios,
    TOTAL_DEFUNCIONES_STR,
    TOTAL_DEFUNCIONES_STR,
    "jenks",
    c(min(df_colera_hotspot$long), min(df_colera_hotspot$lat), max(df_colera_hotspot$long), max(df_colera_hotspot$lat)),
    2
  ) +
  # tm_shape(df_colera_hotspot) + tm_text(CODIGOINE_STR, size = 0.7) +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 25, 50, 100), text.size = 1, position = c("center", "bottom"))

tmap_save(map.invasiones611, filename = paste(COLERA_MAPS_DIR, "tmap.invasiones611.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
tmap_save(map.defunciones611, filename = paste(COLERA_MAPS_DIR, "tmap.defunciones611.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# clean environment -------------------------------------------------------


rm(plot_list.invasiones, plot_list.defunciones, map.invasiones, map.defunciones, map.invasiones611, map.defunciones611, breaksinvasiones, breaksdefunciones)


# neighbour structure ----------------------------------------------------


neighbours <- poly2nb(df_colera_hotspot) # queen neighbours
neighbours

neighbours2 <- poly2nb(df_colera_hotspot, queen = FALSE) # rook neighbours
neighbours2


# global spatial autocorrelation ------------------------------------------


# weights matrix

listw <- nb2listw(neighbours2) 
listw


# global Moran test

globalMoran_test.invasiones <- globalMoran_test(df_colera_hotspot, TOTAL_INVASIONES_STR, listw)
globalMoran_test.defunciones <- globalMoran_test(df_colera_hotspot, TOTAL_DEFUNCIONES_STR, listw)


# local spatial autocorrelation -------------------------------------------


localMoran.invasiones <- localMoran(df_colera_hotspot, TOTAL_INVASIONES_STR, neighbours2, mapS.municipios)
localMoran.defunciones <- localMoran(df_colera_hotspot, TOTAL_DEFUNCIONES_STR, neighbours2, mapS.municipios)


# Getis-Ord approach ------------------------------------------------------


# identify neighbours with queen contiguity (edge/vertex touching)

nb <- poly2nb(df_colera_hotspot, queen = TRUE)
nb


# binary weighting assigns a weight of 1 to all neighbouring features 
# and a weight of 0 to all other features

w_binary <- nb2listw(nb, style = "B")


# calculate spatial lag for "Total_invasiones" and "Total_defunciones"

spatial_lag.i <- lag.listw(w_binary, df_colera_hotspot$Total_invasiones)
spatial_lag.d <- lag.listw(w_binary, df_colera_hotspot$Total_defunciones)


# test for global G statistic of "Total_invasiones" and "Total_defunciones"

globalG.test(df_colera_hotspot$Total_invasiones, w_binary)
globalG.test(df_colera_hotspot$Total_defunciones, w_binary)


# local Gi test -----------------------------------------------------------


# test for local spatial autocorrelation (hotspots) -----------------------


# identify neighbours, create weights, calculate spatial lag

nbs.i <- df_colera_hotspot |> 
  mutate(
    nb = st_contiguity(geometry),                 # neighbours share border/vertex
    wt = st_weights(nb),                          # row-standardized weights
    tes_lag = st_lag(Total_invasiones, nb, wt)    # calculate spatial lag of "Total_invasiones"
  ) 

nbs.d <- df_colera_hotspot |> 
  mutate(
    nb = st_contiguity(geometry),                 
    wt = st_weights(nb),                          
    tes_lag = st_lag(Total_defunciones, nb, wt)     # calculate spatial lag of "Total_defunciones"
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


# TODO: plot looks at Gi values for all locations


# hotspots classification

hot_spots.i <- hotspots_classification(hot_spots.i)
hot_spots.d <- hotspots_classification(hot_spots.d)


# visualize the classification

ggplot() +
  geom_sf(data = mapS.municipios) +
  geom_sf(data = hot_spots.i, aes(fill = classification), color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(fill = "hotspot classification", title = paste0(TOTAL_INVASIONES_STR, " hotspots"))

ggsave(paste(COLERA_MAPS_DIR, paste0("map_hotspot.invasiones_classification.png"), sep = "/"), dpi = 300, limitsize = TRUE)

ggplot() +
  geom_sf(data = mapS.municipios) +
  geom_sf(data = hot_spots.d, aes(fill = classification), color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(fill = "hotspot classification", title = paste0(TOTAL_DEFUNCIONES_STR, " hotspots"))

ggsave(paste(COLERA_MAPS_DIR, paste0("map_hotspot.defunciones_classification.png"), sep = "/"), dpi = 300, limitsize = TRUE)


# TODO: create tmaps


# save results

df_hot_spots.i <- hot_spots.i %>%
  select(
    CODIGOINE, NAMEUNIT, Fecha, Total_invasiones, Total_defunciones, Total_poblacion,
    lat, long, nb, wt, gi, e_gi, var_gi, p_value, p_sim, p_folded_sim, skewness, kurtosis, classification 
  )

df_hot_spots.d <- hot_spots.d %>%
  select(
    CODIGOINE, NAMEUNIT, Fecha, Total_invasiones, Total_defunciones, Total_poblacion,
    lat, long, nb, wt, gi, e_gi, var_gi, p_value, p_sim, p_folded_sim, skewness, kurtosis, classification
  )

df_hot_spots.i$nb <- sapply(df_hot_spots.i$nb, function(x) paste(x, collapse = ", "))
df_hot_spots.i$wt <- sapply(df_hot_spots.i$wt, function(x) paste(x, collapse = ", "))
df_hot_spots.d$nb <- sapply(df_hot_spots.d$nb, function(x) paste(x, collapse = ", "))
df_hot_spots.d$wt <- sapply(df_hot_spots.d$wt, function(x) paste(x, collapse = ", "))

df_hot_spots.i$geometry <- NULL
df_hot_spots.d$geometry <- NULL

df_hot_spots.i$NAMEUNIT <- tolower(iconv(df_hot_spots.i$NAMEUNIT, from = "UTF-8", to = "ASCII//TRANSLIT"))
df_hot_spots.d$NAMEUNIT <- tolower(iconv(df_hot_spots.d$NAMEUNIT, from = "UTF-8", to = "ASCII//TRANSLIT"))

write.csv(df_hot_spots.i, paste(COLERA_DATA_DIR, "colera_hot_spots.invasiones.csv", sep = "/"), row.names = FALSE)
write.csv(df_hot_spots.d, paste(COLERA_DATA_DIR, "colera_hot_spots.defunciones.csv", sep = "/"), row.names = FALSE)
