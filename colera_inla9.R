library(readxl)
library(sf)
library(dplyr)
library(leaflet)
library(sp)
library(rgdal)
library(viridis)


# load("~/epi_predict/colera_data.RData")


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)

LONG_STR <- "long"
LAT_STR <- "lat"
X_STR <- "x"
Y_STR <- "y"


# data --------------------------------------------------------------------


options(scipen=999)

df_railwaylines <- read_excel(paste(DATA_DIR, "Output.xlsx", sep = "/"))


# format df_railwaylines

df_railwaylines$Orig_Cod_INE <- as.character(df_railwaylines$Orig_Cod_INE)
df_railwaylines$Dest_Cod_INE <- as.character(df_railwaylines$Dest_Cod_INE)

df_railwaylines$Orig_Cod_INE <- substring(df_railwaylines$Dest_Cod_INE, 1, 5)
df_railwaylines$Dest_Cod_INE <- substring(df_railwaylines$Dest_Cod_INE, 1, 5)

df_railwaylines$Orig_Cod_INE <- as.numeric(df_railwaylines$Orig_Cod_INE)
df_railwaylines$Dest_Cod_INE <- as.numeric(df_railwaylines$Dest_Cod_INE)


# merge df_railwaylines with df_colera.merged

df_railwaylines.subset <- df_railwaylines[, c(8, 10, 11, 12, 13)]
colnames(df_railwaylines.subset)[1] <- CODIGO_INE_STR
head(df_railwaylines.subset)

df_colera.merged$`Codigo Ine` <- as.numeric(df_colera.merged$`Codigo Ine`)
rownames(df_colera.merged) <- 1:nrow(df_colera.merged)
head(df_colera.merged)

df_railwaylines.merged <- merge(df_colera.merged, df_railwaylines.subset, by = CODIGO_INE_STR)
head(df_railwaylines.merged)


# add coordinates from df_colera and save as df_colera_inla9

df_colera_coord <- df_colera[, c(CODIGO_INE_STR, "LAT_POB_new_num", "LNG_POB_new_num")]
df_colera_coord <- na.omit(df_colera_coord)
df_colera_coord <- distinct(df_colera_coord, .keep_all = TRUE)
colnames(df_colera_coord)[2:3] <- c(LAT_STR, LONG_STR)
head(df_colera_coord)

df_colera_inla9 <- merge(df_railwaylines.merged, df_colera_coord, by = CODIGO_INE_STR)
head(df_colera_inla9)

# transform long and lat to UTM coordinates

p <- st_as_sf(data.frame(long = df_colera_inla9$long, lat = df_colera_inla9$lat), coords = c(LONG_STR, LAT_STR))
st_crs(p) <- st_crs(4326) # EPSG code  
p <- p %>% st_transform(25830) # ESPG code of Spain which corresponds to UTM zone 30 North
df_colera_inla9[, c(X_STR, Y_STR)] <- st_coordinates(p)

df_colera_inla9 <- na.omit(df_colera_inla9) # remove NA
head(df_colera_inla9)


# data preparation --------------------------------------------------------


# prevalence

df_colera_inla9$prev <-
  ((
    df_colera_inla9$Total_invasiones +  df_colera_inla9$Total_defunciones
  ) / df_colera_inla9$Total_poblacion
  ) * 100

# bins <- seq(min(df_colera_inla9$prev), max(df_colera_inla9$prev), by = 5)
# interval_values <- cut(df_colera_inla9$prev, breaks = bins, right = FALSE)
# interval_counts <- table(interval_values)
# barplot(interval_counts)


# mapping prevalence

# c(0, 1.25, 2.5, 3.75, 5, 10, 15, 20, 25, 65) # all values
pal <- colorBin("viridis", bins = c(0, 1.25, 2.5, 3.75, 5))
leaflet(df_colera_inla9) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~long, lat = ~lat, color = ~ pal(prev)) %>%
  addLegend("bottomright",
            pal = pal, values = ~prev,
            title = "prev."
  ) %>%
  addScaleBar(position = c("bottomleft"))


# environmental covariates


options(scipen=0)
