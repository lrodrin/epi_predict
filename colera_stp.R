library(dplyr)
library(lubridate)
library(sf)
library(spdep)
library(ggplot2)
library(tmap)
library(sp)

load("colera_data.RData") # load cholera data


# constants ---------------------------------------------------------------


COLERA_MAPS_DIR <- "colera_maps"
dir.create(COLERA_MAPS_DIR, showWarnings = FALSE)
SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)

PROVINCIAS_STR <- c("zaragoza", "valencia", "granada", "murcia", "teruel", "castellon", "alicante", "navarra", "cuenca", "albacete")
TOTAL_INVASIONES_STR <- paste("Total", INVASIONES_STR, sep = "_")
TOTAL_DEFUNCIONES_STR <- paste("Total", DEFUNCIONES_STR, sep = "_")
COLORS <- c("#ffffff", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
COLORS_SPOTS <- c("#d1e5f0", "#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b")
CLUSTERS <- c("insignificant", "low-low", "low-high", "high-low", "high-high")
CLUSTERS_SPOTS <-  c("insignificant", "very low", "low", "moderate", "high", "very high")


# functions ---------------------------------------------------------------


convert_to_spatial <- function(data, province) {
  #' Convert data frame to spatial data frame.
  #'
  #' This function converts a data frame to a spatial data frame.
  #'
  #' @param data A data frame to be converted.
  #' @param province Name of the province.
  #'
  #' @return Spatial data frame.

  subset_data <- subset(data, Provincia == province) # subset data by province
  spdf_data <- as_Spatial(subset_data) # convert to spatial data frame
  return(spdf_data)
}


compute_weight_matrices <- function(data, province) {
  #' Compute weight matrices.
  #'
  #' This function computes weight matrices for a given spatial data frame.
  #'
  #' @param data A spatial data frame.
  #' @param province Name of the province.

  for(i in sort(unique(data$Fecha))) { # iterate over 6 to 11 (june to november)

    coords <- coordinates(subset(data, Fecha == i)) # get coordinates
    knb <- knn2nb(knearneigh(coords, k = 8, longlat = FALSE)) # create knn neighbours
    assign(paste0("knb_lw.", province, i), nb2listw(knb, style = "B"), envir = .GlobalEnv) # create listw object
  }
}


compute_moran <- function(data, province, cause) {
  #' Compute Moran's I.
  #'
  #' This function computes Moran's I for a given spatial data frame.
  #'
  #' @param data A spatial data frame.
  #' @param province Name of the province.
  #' @param cause Cause of the disease. Invasion or death.

  for(i in sort(unique(data$Fecha))) { # iterate over 6 to 11 (june to november)

    var_names <- paste0("knb_lw.", province, i)
    knb <- get(var_names, envir = globalenv()) # get listw object

    localMI  <- paste0("lmoran_month.", province, i, ".", cause) # create local moran's I
    cm_localMI <- paste0("cm_localMI_month.", province, i, ".", cause) # create combined local moran's I

    assign(localMI, localmoran(data[data$Fecha == i, ][[cause]], knb), envir = .GlobalEnv) # compute local moran's I
    assign(cm_localMI, cbind(data[data$Fecha == i, ], eval(as.name(localMI))), envir = .GlobalEnv) # combine local moran's I with data
  }
}


compute_lisaClusters <- function(data, province, cause) {
  #' Compute LISA clusters.
  #'
  #' This function computes LISA clusters for a given spatial data frame.
  #'
  #' @param data A spatial data frame.
  #' @param province Name of the province.
  #' @param cause Cause of the disease. Invasion or death.

  LisaList <- list() # create list to store LISA clusters
  LisaPlot <- list() # create list to store LISA plots

  for(i in sort(unique(data$Fecha))) { # iterate over 6 to 11 (june to november)

    localMI <- paste0("lmoran_month.", province, i, ".", cause) # get local moran's I
    quadrant <- vector(mode = "numeric", length = nrow(eval(as.name(localMI)))) # create vector to store quadrants

    # compute deviations
    DV <- data[data$Fecha == i, ][[cause]] - mean(data[data$Fecha == i, ][[cause]])
    C_mI <- eval(as.name(localMI))[,1]- mean(eval(as.name(localMI))[,1])
    signif <- 0.05
    quadrant[DV >0 & C_mI>0] <- 4
    quadrant[DV <0 & C_mI<0] <- 1
    quadrant[DV <0 & C_mI>0] <- 2
    quadrant[DV >0 & C_mI<0] <- 3
    quadrant[eval(as.name(localMI))[,5]>signif] <- 0
    
    cm_localMI <- paste0("cm_localMI_month.", province, i, ".", cause) # create combined local moran's I
    
    quadrant_df <- data.frame("quadrant" = quadrant) # create data frame with quadrants
    assign(cm_localMI, cbind(eval(as.name(cm_localMI)), quadrant_df), envir = .GlobalEnv) # combine combined local moran's I with quadrants
    
    LisaList[[i]] <- eval(as.name(cm_localMI))@data # store LISA clusters

    # plot LISA clusters
    LisaPlot[[i]] <- tm_shape(eval(as.name(cm_localMI))) +
      tm_fill(col = "quadrant", style = "cat", palette = COLORS[c(sort(unique(quadrant))) + 1], labels = CLUSTERS[c(sort(unique(quadrant))) + 1]) +
      tm_borders(alpha = 0.5) +
      tm_view(set.zoom.limits = c(11, 17))+
      tm_layout(paste("month", i), legend.outside = TRUE)
  }
  tmap_save(tmap_arrange(LisaPlot[6:11], ncol = 2), paste(COLERA_MAPS_DIR, paste0("lisaClusters_map.", province, ".", cause,".png"), sep = "/")) # save LISA clusters plot
}


compute_localGi <- function(data, province, cause) {
    #' Compute local G_i.
    #'
    #' This function computes local G_i for a given spatial data frame.
    #'
    #' @param data A spatial data frame.
    #' @param province Name of the province.
    #' @param cause Cause of the disease. Invasion or death.

    localGiList <- list() # create list to store local G_i
    localGiPlot <- list() # create list to store local G_i plots

    for(i in sort(unique(data$Fecha))) { # iterate over 6 to 11 (june to november)

      var_names <- paste0("knb_lw.", province, i)
      knb <- get(var_names, envir = globalenv()) # get listw object
      localGI <- paste0("gi_month", i, ".", cause) # create local G_i
      assign(localGI, cbind(data[data$Fecha == i, ], data.frame("gstat_adaptive" = as.matrix(localG(data[data$Fecha == i, ][[cause]], knb)))), envir = .GlobalEnv) # compute local G_i

      categorized_data <- transform(get(localGI), category = cut(gstat_adaptive, breaks = c(-Inf, 0, 1, 2, 3, 4, 5), labels = CLUSTERS_SPOTS,include.lowest = TRUE)) # categorize data
      assign(localGI, cbind(data[data$Fecha == i, ], data.frame("gstat_adaptive" = as.matrix(localG(data[data$Fecha == i, ][[cause]], knb))), categorized_data), envir = .GlobalEnv) # combine local G_i with categorized data

      localGiList[[i]] <- get(localGI) # store local G_i

      # plot local G_i
      localGiPlot[[i]] <- tm_shape(eval(as.name(localGI))) +
        tm_fill(col = "category", style = "pretty", palette = COLORS_SPOTS, title = "") +
        tm_borders(alpha = 0.5) +
        tm_view(set.zoom.limits = c(11, 17))+
        tm_layout(paste("month", i), legend.outside = TRUE)
    }
    tmap_save(tmap_arrange(localGiPlot[6:11], ncol = 2), paste(COLERA_MAPS_DIR, paste0("hotspots_map.", province, ".", cause, ".png"), sep = "/")) # save local G_i plot
}


# main --------------------------------------------------------------------


# load data
df_colera.groupByProvinciaMunicipioFecha <- merge(df_colera.merged.month, df_distances, by = CODIGO_INE_STR)
df_colera.groupByProvinciaMunicipioFecha <- df_colera.groupByProvinciaMunicipioFecha[, c(1, 6:7, 2, 3:5, 8:9, 10:17)] # select columns
head(df_colera.groupByProvinciaMunicipioFecha)

# load shape file
mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7" & CODNUT2 != "ES53")  # remove Canary Islands and Balearic Islands
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001)))  # remove Ceuta and Melilla
head(mapS.municipios)

# merge data frame with shape file
mapS.colera_stp <- merge(mapS.municipios, df_colera.groupByProvinciaMunicipioFecha, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.colera_stp)

for(provincia in PROVINCIAS_STR) {

  assign(paste("spdf_mapS.colera_stp", provincia, sep = "."), convert_to_spatial(mapS.colera_stp, provincia)) # convert to spatial data frame

  var_names <- paste0("spdf_mapS.colera_stp.", provincia)  # get spatial data frame
  spdf_mapS <- get(var_names, envir = globalenv())

  compute_weight_matrices(spdf_mapS, provincia) # compute weight matrices

  # compute moran's I, LISA clusters and local G_i for total invasions and total deaths
  for(cause in c(TOTAL_INVASIONES_STR, TOTAL_DEFUNCIONES_STR)) {
    compute_moran(spdf_mapS, provincia, cause) # compute moran's I
    compute_lisaClusters(spdf_mapS, provincia, cause) # compute LISA clusters
    compute_localGi(spdf_mapS, provincia, cause) # compute local G_i
  }
}

rm(list = ls())
