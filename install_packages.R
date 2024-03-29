# package names
packages <-
  c(
    "ggplot2",
    "readxl",
    "dplyr",
    "lubridate",
    "tidyverse",
    "data.table",
    "zoo",
    "forecast",
    "tseries",
    "lwgeom",
    "sf",
    "viridis",
    "foreach",
    "spdep",
    "splancs",
    "raster",
    "reshape2",
    "doParallel",
    "SpatialEpi",
    "plotly",
    "gghighlight",
    "gganimate"
    
  )

# install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# packages loading
invisible(lapply(packages, library, character.only = TRUE))
