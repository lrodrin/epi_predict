library(shiny)
library(shinyjs)
library(dplyr)
library(sf)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(tmap)


# constants ---------------------------------------------------------------


CODIGOINE_STR <- "CODIGOINE"
CODIGO_INE_STR <- "Codigo.Ine"
COUNTIES <- c("all", "zaragoza", "valencia", "granada", "murcia", "teruel", "castellon", "alicante", "navarra", "cuenca", "albacete")
MONTHS <- c("all", "6", "7", "8", "9", "10", "11")
VARIABLES <- c("Total_invasiones", "Total_defunciones", "Total_poblacion")


# functions ---------------------------------------------------------------


create_tmap <- function(df_mes, map, var_col, style) {
  #' Create a tmap object
  #'
  #' This function creates a tmap object with the data and the map provided.
  #'
  #' @param df_mes A data frame with the data to be plotted.
  #' @param map A shapefile with the map to be plotted.
  #' @param var_col A character string with the name of the column to be plotted.
  #' @param style A character string with the name of the style to be used.
  #'
  #' @return A tmap object.

  map.tmp <- tm_shape(df_mes, bbox = map) +
    tm_polygons(
      col = var_col,
      border.col = NULL,
      title = var_col,
      palette = "Reds",
      style = style
    ) +
    tm_shape(map) + tm_borders() +
    tm_layout(
      legend.text.size = 1.5,
      inner.margins = c(0, 0, 0, 0),
      panel.label.size = 1.5, panel.label.height = 1.1, 
      panel.label.color = "black", panel.label.bg.color = "gray"
    )
  
  return(map.tmp)
}


filter_data <- function(data, county_selected, month_selected) {
  #' Filter data.
  #'
  #' This function filters the data according to the selected county and month,
  #' or by "Codigo Ine" and "Provincia" and "Municipio" and "Total_poblacion".
  #'
  #' @param data A data frame with the data to be filtered.
  #' @param county_selected A character string with the name of the selected county.
  #' @param month_selected A character string with the name of the selected month.
  #'
  #' @return A data frame with the filtered data.
  
  filtered_data <- data
  
  if (county_selected != "all") { filtered_data <- subset(filtered_data, Provincia == county_selected) } # filter by county
  if (month_selected != "all") { filtered_data <- subset(filtered_data, Fecha == month_selected) }  # filter by month
  else { # filter by "Codigo Ine" and "Provincia" and "Municipio" and "Total_poblacion"
    filtered_data <- filtered_data %>% group_by(`Codigo.Ine`, Provincia, Municipio, Total_poblacion) %>% 
      summarize(Total_invasiones = sum(Total_invasiones), Total_defunciones = sum(Total_defunciones))
  }

  return(filtered_data)
}
