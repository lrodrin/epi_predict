# https://rpubs.com/heatherleeleary/hotspot_getisOrd_tut
# https://rpubs.com/quarcs-lab/spatial-autocorrelation


library(sf)
library(dplyr)
library(tmap)
library(sfdep)
library(spdep)
library(ggplot2)


# load("colera_data.RData")


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
TASA_INCIDENCIA_STR <- "Tasa_incidencia"
TASA_MORTALIDAD_STR <- "Tasa_mortalidad"
TASA_INCIDENCIA_FACTOR_STR <- "incidencia_factor"
TASA_MORTALIDAD_FACTOR_STR <- "mortalidad_factor"

MONTHS_INT <- c(6, 7, 8, 9, 10, 11)
PLOT_LABELS <- c("low", "mid-low", "mid-high", "high")
PLOT_COLORS_BY_LABELS <- c(
  "low" = "#FEE4D8",
  "mid-low" = "#FCB195",
  "mid-high" = "#FB795A", # EF3C2D
  "high" = "#BB1419"
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
        legend.outside = TRUE,
        frame = FALSE,
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
        legend.outside = TRUE,
        frame = FALSE,
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
      tm_shape(map) + tm_borders()
  ) # TODO: save plot
  
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


# map ---------------------------------------------------------------------


mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7") # remove Canary Islands
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) # remove "Ceuta" and "Melilla"
head(mapS.municipios)


# explore raw data --------------------------------------------------------


df_colera.merged.month$`Codigo Ine` <- as.numeric(df_colera.merged.month$`Codigo Ine`)
rownames(df_colera.merged.month) <- 1:nrow(df_colera.merged.month)
head(df_colera.merged.month)


# add coordinates from df_colera and save as df_colera_hotspot

df_colera_coord <- df_colera[, c(CODIGO_INE_STR, "LAT_POB_new_num", "LNG_POB_new_num")]
df_colera_coord <- distinct(df_colera_coord, .keep_all = TRUE)
df_colera_coord <- na.omit(df_colera_coord)
colnames(df_colera_coord)[2:3] <- c(LAT_STR, LONG_STR)
head(df_colera_coord)

df_colera_hotspot <- merge(df_colera.merged.month, df_colera_coord, by = CODIGO_INE_STR)
df_colera_hotspot <- df_colera_hotspot[, !colnames(df_colera_hotspot) %in% c(PROVINCIA_STR, MUNICIPIO_STR)]
df_colera_hotspot <- df_colera_hotspot %>%
  mutate(
    lat = ifelse(lat < 36.0, 36.0, ifelse(lat > 43.0, 43.0, lat)),
    long = ifelse(long < -10.0, -10.0, ifelse(long > 4.0, 4.0, long))
  )
head(df_colera_hotspot)


# merge mapS.municipios and df_colera_hotspot

df_colera_hotspot <- merge(mapS.municipios, df_colera_hotspot, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(df_colera_hotspot)


# visualize "Tasa_incidencia" and "Tasa_mortalidad" distribution

ggplot(df_colera_hotspot, aes(x = Tasa_incidencia)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = paste0("Distribution of ", TASA_INCIDENCIA_STR), x = TASA_INCIDENCIA_STR, y = "Density") +
  scale_x_continuous(breaks = seq(0, max(df_colera_hotspot$Tasa_incidencia), by = 1)) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggplot(df_colera_hotspot, aes(x = Tasa_mortalidad)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = paste0("Distribution of ", TASA_MORTALIDAD_STR), x = TASA_MORTALIDAD_STR, y = "Density") +
  scale_x_continuous(breaks = seq(0, max(df_colera_hotspot$Tasa_mortalidad), by = 1)) +
  theme_bw() +
  theme(text = element_text(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# factorize "Tasa_incidencia" and "Tasa_mortalidad"

df_colera_hotspot.copy <- df_colera_hotspot

df_colera_hotspot.copy <- factorize(
  df_colera_hotspot.copy,
  TASA_INCIDENCIA_STR,
  TASA_INCIDENCIA_FACTOR_STR,
  c(-1, 0.5, 1, 2, Inf)
)

df_colera_hotspot.copy <- factorize(
  df_colera_hotspot.copy,
  TASA_MORTALIDAD_STR,
  TASA_MORTALIDAD_FACTOR_STR,
  c(-1, 0.5, 1, 2, Inf)
)

plot_observations(df_colera_hotspot.copy, TASA_INCIDENCIA_FACTOR_STR, TASA_INCIDENCIA_STR)
plot_observations(df_colera_hotspot.copy, TASA_MORTALIDAD_FACTOR_STR, TASA_MORTALIDAD_STR)


# for each month

plot_list.incidencia <- plot_observationsByMonth(df_colera_hotspot.copy, TASA_INCIDENCIA_FACTOR_STR, TASA_INCIDENCIA_STR)
plot_list.mortalidad <- plot_observationsByMonth(df_colera_hotspot.copy, TASA_MORTALIDAD_FACTOR_STR, TASA_MORTALIDAD_STR)

for (month in names(plot_list.incidencia)) { # TODO: change type of plots ???
  # print(plot_list.incidencia[[month]])
  # print(plot_list.mortalidad[[month]])
  ggsave(paste0(COLERA_MAPS_DIR, "/map_hotspot.incidenciaXmunicipios_", month, ".png"), plot_list.incidencia[[month]])
  ggsave(paste0(COLERA_MAPS_DIR, "/map_hotspot.mortalidadXmunicipios_", month, ".png"), plot_list.mortalidad[[month]])
  
}


# visualize "Tasa_incidencia" and "Tasa_mortalidad" across neighbourhoods 

map.incidencia_factor <- 
  create_tmap(
    df_colera_hotspot.copy,
    mapS.municipios,
    TASA_INCIDENCIA_FACTOR_STR,
    TASA_INCIDENCIA_STR,
    "cat",
    c(min(df_colera_hotspot.copy$long), min(df_colera_hotspot.copy$lat), max(df_colera_hotspot.copy$long), max(df_colera_hotspot.copy$lat))
  ) 

map.mortalidad_factor <- 
  create_tmap(
    df_colera_hotspot.copy,
    mapS.municipios,
    TASA_MORTALIDAD_FACTOR_STR,
    TASA_MORTALIDAD_STR,
    "cat",
    c(min(df_colera_hotspot.copy$long), min(df_colera_hotspot.copy$lat), max(df_colera_hotspot.copy$long), max(df_colera_hotspot.copy$lat))
  ) 

tmap_save(map.incidencia_factor, filename = paste(COLERA_MAPS_DIR, "tmap.incidencia_factor.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
tmap_save(map.mortalidad_factor, filename = paste(COLERA_MAPS_DIR, "tmap.mortalidad_factor.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# for each month (animation)

map.incidencia_factorByMonth <- 
  create_tmapByFecha(
    df_colera_hotspot.copy,
    mapS.municipios,
    TASA_INCIDENCIA_FACTOR_STR,
    TASA_INCIDENCIA_STR,
    "cat",
    c(min(df_colera_hotspot.copy$long), min(df_colera_hotspot.copy$lat), max(df_colera_hotspot.copy$long), max(df_colera_hotspot.copy$lat)),
    2
  )

map.mortalidad_factorByMonth <- 
  create_tmapByFecha(
    df_colera_hotspot.copy,
    mapS.municipios,
    TASA_MORTALIDAD_FACTOR_STR,
    TASA_MORTALIDAD_STR,
    "cat",
    c(min(df_colera_hotspot.copy$long), min(df_colera_hotspot.copy$lat), max(df_colera_hotspot.copy$long), max(df_colera_hotspot.copy$lat)),
    2
  )

tmap_save(map.incidencia_factorByMonth, filename = paste(COLERA_MAPS_DIR, "tmap.incidencia_factorByMonth.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")
tmap_save(map.mortalidad_factorByMonth, filename = paste(COLERA_MAPS_DIR, "tmap.mortalidad_factorByMonth.png", sep = "/"), width = 20, height = 10, dpi = 300, units = "in")


# clean environment -------------------------------------------------------


rm(df_colera_coord, df_colera_hotspot.copy, plot_list.incidencia, plot_list.mortalidad, map.incidencia_factor, map.incidencia_factorByMonth, map.mortalidad_factor, map.mortalidad_factorByMonth)


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

globalMoran_test.incidencia <- globalMoran_test(df_colera_hotspot, TASA_INCIDENCIA_STR, listw)
globalMoran_test.mortalidad <- globalMoran_test(df_colera_hotspot, TASA_MORTALIDAD_STR, listw)


# local spatial autocorrelation -------------------------------------------


localMoran.incidencia <- localMoran(df_colera_hotspot, TASA_INCIDENCIA_STR, neighbours2, mapS.municipios)
localMoran.mortalidad <- localMoran(df_colera_hotspot, TASA_MORTALIDAD_STR, neighbours2, mapS.municipios)


# Getis-Ord approach ------------------------------------------------------


# identify neighbours with queen contiguity (edge/vertex touching)

nb <- poly2nb(df_colera_hotspot, queen = TRUE)
nb


# binary weighting assigns a weight of 1 to all neighbouring features 
# and a weight of 0 to all other features

w_binary <- nb2listw(nb, style = "B")


# calculate spatial lag for "Tasa_incidencia" and "Tasa_mortalidad"

spatial_lag.i <- lag.listw(w_binary, df_colera_hotspot$Tasa_incidencia)
spatial_lag.m <- lag.listw(w_binary, df_colera_hotspot$Tasa_mortalidad)


# test for global G statistic of "Tasa_incidencia" and "Tasa_mortalidad"

globalG.test(df_colera_hotspot$Tasa_incidencia, w_binary)
globalG.test(df_colera_hotspot$Tasa_mortalidad, w_binary)


# local Gi test -----------------------------------------------------------


# test for local spatial autocorrelation (hotspots) -----------------------


# identify neighbours, create weights, calculate spatial lag

nbs.i <- df_colera_hotspot |> 
  mutate(
    nb = st_contiguity(geometry),                 # neighbours share border/vertex
    wt = st_weights(nb),                          # row-standardized weights
    tes_lag = st_lag(Tasa_incidencia, nb, wt)     # calculate spatial lag of "Tasa_incidencia"
  ) 

nbs.m <- df_colera_hotspot |> 
  mutate(
    nb = st_contiguity(geometry),                 
    wt = st_weights(nb),                          
    tes_lag = st_lag(Tasa_mortalidad, nb, wt)     # calculate spatial lag of "Tasa_mortalidad"
  ) 


# calculate the Gi using local_g_perm

hot_spots.i <- nbs.i |> 
  mutate(
    Gi = local_g_perm(Tasa_incidencia, nb, wt, nsim = 999)
    # nsim = number of Monte Carlo simulations (999 is default)
  ) |> 
  # new "Gi" column itself contains a data frame 
  # can't work with that, need to "unnest" it
  unnest(Gi)

hot_spots.m <- nbs.m |> 
  mutate(
    Gi = local_g_perm(Tasa_mortalidad, nb, wt, nsim = 999)
  ) |> 
  unnest(Gi)


# TODO: plot looks at Gi values for all locations

ggplot() +
  geom_sf(data = mapS.municipios) +
  geom_sf(data = hot_spots.i, aes(fill = gi), color = "black", lwd = 0.15) +
  scale_fill_gradient2() + # makes the value 0 (random) be the middle
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  theme_void()
  
ggplot() +
  geom_sf(data = mapS.municipios) +
  geom_sf(data = hot_spots.m, aes(fill = gi), color = "black", lwd = 0.15) +
  scale_fill_gradient2() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  theme_void()


# hotspots classification

hot_spots.i <- hotspots_classification(hot_spots.i)
hot_spots.m <- hotspots_classification(hot_spots.m)


# visualize the classification

ggplot() +
  geom_sf(data = mapS.municipios) +
  geom_sf(data = hot_spots.i, aes(fill = classification), color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(fill = "hotspot classification", title = paste0(TASA_INCIDENCIA_STR, " hotspots"))

ggsave(paste(COLERA_MAPS_DIR, paste0("map_hotspot.incidencia_classification.png"), sep = "/"), dpi = 300, limitsize = TRUE)

ggplot() +
  geom_sf(data = mapS.municipios) +
  geom_sf(data = hot_spots.m, aes(fill = classification), color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(fill = "hotspot classification", title = paste0(TASA_MORTALIDAD_STR, " hotspots"))

ggsave(paste(COLERA_MAPS_DIR, paste0("map_hotspot.mortalidad_classification.png"), sep = "/"), dpi = 300, limitsize = TRUE)


# save results

df_hot_spots.i <- hot_spots.i %>%
  select(
    CODIGOINE, NAMEUNIT, Fecha, Total_invasiones, Tasa_incidencia, Total_defunciones, Tasa_mortalidad, Total_poblacion,
    lat, long, nb, wt, gi, e_gi, var_gi, p_value, p_sim, p_folded_sim, skewness, kurtosis, classification 
  )

df_hot_spots.m <- hot_spots.m %>%
  select(
    CODIGOINE, NAMEUNIT, Fecha, Total_invasiones, Tasa_incidencia, Total_defunciones, Tasa_mortalidad, Total_poblacion,
    lat, long, nb, wt, gi, e_gi, var_gi, p_value, p_sim, p_folded_sim, skewness, kurtosis, classification
  )

df_hot_spots.i$nb <- sapply(df_hot_spots.i$nb, function(x) paste(x, collapse = ", "))
df_hot_spots.i$wt <- sapply(df_hot_spots.i$wt, function(x) paste(x, collapse = ", "))
df_hot_spots.m$nb <- sapply(df_hot_spots.m$nb, function(x) paste(x, collapse = ", "))
df_hot_spots.m$wt <- sapply(df_hot_spots.m$wt, function(x) paste(x, collapse = ", "))

df_hot_spots.i$geometry <- NULL
df_hot_spots.m$geometry <- NULL

df_hot_spots.i$NAMEUNIT <- tolower(iconv(df_hot_spots.i$NAMEUNIT, from = "UTF-8", to = "ASCII//TRANSLIT"))
df_hot_spots.m$NAMEUNIT <- tolower(iconv(df_hot_spots.m$NAMEUNIT, from = "UTF-8", to = "ASCII//TRANSLIT"))

write.csv(df_hot_spots.i, paste(COLERA_DATA_DIR, "colera_hot_spots.incidencia.csv", sep = "/"), row.names = FALSE)
write.csv(df_hot_spots.m, paste(COLERA_DATA_DIR, "colera_hot_spots.mortalidad.csv", sep = "/"), row.names = FALSE)
