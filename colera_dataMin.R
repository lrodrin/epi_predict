library(sf)
library(rgdal)
library(viridis)
library(tidyr)
library(gghighlight)
library(plotly)
library(gganimate)
library(zoo)
library(transformr)
library(gifski)


# remove.packages("gganimate")
# remove.packages("transformr")
# install.packages("https://cran.r-project.org/src/contrib/Archive/gganimate/gganimate_1.0.7.tar.gz", repos = NULL, type = "source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/transformr/transformr_0.1.3.tar.gz", repos = NULL, type = "source")


# data --------------------------------------------------------------------


Pob_Arago_PaisValencia_Murcia <- read_excel(paste(DATA_DIR, "Pob_Arago_PaisValencia_Murcia.xlsx", sep = "/"))
Pob_Arago_PaisValencia_Murcia1887 <- Pob_Arago_PaisValencia_Murcia[, c("Codi INE", "Municipi", "1887")]
Pob_Arago_PaisValencia_Murcia1887 <- na.omit(Pob_Arago_PaisValencia_Murcia1887) # remove NA
names(Pob_Arago_PaisValencia_Murcia1887) <- c(CODIGO_INE_STR, MUNICIPIO_STR, "Total_poblacion") # change column names

# group "invasiones" and "defunciones" by "Provincia", "Fecha" and "Codigo Ine"
df_colera_invasiones.groupByProvinciaFechaCodigoINE <- df_colera_invasiones %>%
  group_by(Provincia, `Codigo Ine`, Fecha) %>%
  summarize(Total_invasiones = sum(invasiones)) %>%
  na.omit(df_colera_invasiones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = "day"),
    fill = list(Total_invasiones = 0)
  ) # add missing dates

df_colera_defunciones.groupByProvinciaFechaCodigoINE <- df_colera_defunciones %>%
  group_by(Provincia, `Codigo Ine`, Fecha) %>%
  summarize(Total_defunciones = sum(defunciones)) %>%
  na.omit(df_colera_defunciones) %>%
  complete(
    Fecha = seq.Date(as.Date(START_DATE), as.Date(END_DATE), by = "day"),
    fill = list(Total_defunciones = 0)
  ) # add missing dates

# merge grouped "invasiones" and "defunciones" as df_colera.groupByProvinciaFechaCodigoINE
df_colera.groupByProvinciaFechaCodigoINE <- merge(df_colera_invasiones.groupByProvinciaFechaCodigoINE, df_colera_defunciones.groupByProvinciaFechaCodigoINE)

# merge df_colera.groupByProvinciaFechaCodigoINE with Pob_Arago_PaisValencia_Murcia1887
df_colera.merged <- merge(df_colera.groupByProvinciaFechaCodigoINE, Pob_Arago_PaisValencia_Murcia1887, by = CODIGO_INE_STR)
df_colera.merged <- df_colera.merged[, c(CODIGO_INE_STR, MUNICIPIO_STR, PROVINCIA_STR, "Total_invasiones", "Total_defunciones", "Total_poblacion", FECHA_STR)]

# unique(df_colera.merged$Provincia)

df_colera.merged.CCAAvalencia <- subset(df_colera.merged, Provincia %in% c("castellon", "valencia"))
df_colera.merged.CCAAaragon <- subset(df_colera.merged, Provincia %in% c("huesca", "teruel", "zaragoza"))
df_colera.merged.CCAAmurcia <- subset(df_colera.merged, Provincia == "murcia")

codigosINE.CCAAvalencia <- unique(df_colera.merged.CCAAvalencia$`Codigo Ine`)
codigosINE.CCAAaragon <- unique(df_colera.merged.CCAAaragon$`Codigo Ine`)
codigosINE.CCAAmurcia <- unique(df_colera.merged.CCAAmurcia$`Codigo Ine`)


# map ---------------------------------------------------------------------


# GADM
# mapsfile <- getData(name = "GADM", country = "Spain", level = 4)
# map_municipios <- read_rds("gadm36_ESP_4_sp.rds")

# shapefile
# map <- readOGR("shapes/MUNICIPIOS_IGN.shp", verbose = FALSE)
map_municipios <- st_read("shapes/MUNICIPIOS_IGN.shp", quiet = TRUE)
summary(map_municipios)

map_valencia <- map_municipios[map_municipios$CODIGOINE %in% codigosINE.CCAAvalencia,]
map_aragon <- map_municipios[map_municipios$CODIGOINE %in% codigosINE.CCAAaragon,]
map_murcia <- map_municipios[map_municipios$CODIGOINE %in% codigosINE.CCAAmurcia,]

# map_valencia <- map_municipios[map_municipios$NAME_1 == "Comunidad Valenciana",]
# map_valencia@data

map_valencia$CODIGOINE <- as.numeric(map_valencia$CODIGOINE)
map_aragon$CODIGOINE <- as.numeric(map_aragon$CODIGOINE)
map_murcia$CODIGOINE <- as.numeric(map_murcia$CODIGOINE)

plot(map_valencia)
plot(map_aragon, max.plot = 10)
plot(map_murcia)


# invasiones murcia agg ---------------------------------------------------


head(df_colera.merged.CCAAmurcia)

df_colera_invasiones.merged.CCAAmurcia.agg <- aggregate(
  x = df_colera.merged.CCAAmurcia$Total_invasiones,
  by = list(
    `Codigo Ine` = df_colera.merged.CCAAmurcia$`Codigo Ine`, 
    Total_poblacion = df_colera.merged.CCAAmurcia$Total_poblacion,
    Fecha = df_colera.merged.CCAAmurcia$Fecha),
  FUN = sum
)

names(df_colera_invasiones.merged.CCAAmurcia.agg) <- c(CODIGO_INE_STR, "Total_poblacion", FECHA_STR, "Total_invasiones")
df_colera_invasiones.merged.CCAAmurcia.agg <- df_colera_invasiones.merged.CCAAmurcia.agg[order(df_colera_invasiones.merged.CCAAmurcia.agg$`Codigo Ine`, df_colera_invasiones.merged.CCAAmurcia.agg$Fecha),]
df_colera_invasiones.merged.CCAAmurcia.agg$Fecha <- month(as.POSIXlt(df_colera_invasiones.merged.CCAAmurcia.agg$Fecha, format = "%Y-%m-d%"))
head(df_colera_invasiones.merged.CCAAmurcia.agg)

# adding df_colera_invasiones.merged.CCAAmurcia.agg to map_murcia
df_colera_invasiones.merged.CCAAmurcia.aggw <-
  reshape(
    df_colera_invasiones.merged.CCAAmurcia.agg,
    timevar = FECHA_STR,
    idvar = CODIGO_INE_STR,
    direction = "wide"
  )

df_colera_invasiones.merged.CCAAmurcia.aggw[1:2, ]
map_murcia[1:2, ]

# merge map_murcia with df_colera_invasiones.merged.CCAAmurcia.aggw
map_murcia.merged <- merge(map_murcia, df_colera_invasiones.merged.CCAAmurcia.aggw, by.x = "CODIGOINE", by.y = CODIGO_INE_STR)
map_murcia.merged[1:2, ]

mapsf_murcia.merged <- st_as_sf(map_murcia.merged)
mapsf_murcia.merged <- gather(mapsf_murcia.merged, Fecha, Total_invasiones, paste0("Total_invasiones.", 6:11))
# mapsf_murcia.merged <-
#   gather(mapsf_murcia.merged,
#          Fecha,
#          Total_invasiones,
#          paste0("Total_invasiones.", seq(
#            as.Date('1885-06-18'), as.Date('1885-11-18'), by = 1
#          )))

# plots -------------------------------------------------------------------


# mapsf_murcia.merged$Fecha <- as.Date(substring(mapsf_murcia.merged$Fecha, 18, 30), format = "%Y-%m-%d")
mapsf_murcia.merged$Fecha <- as.integer(substring(mapsf_murcia.merged$Fecha, 18, 19))
mapsf_murcia.merged[1:2, ]

ggplot(mapsf_murcia.merged) + geom_sf(aes(fill = Total_invasiones)) +
  facet_wrap(~Fecha, ncol = 6) +
  ggtitle("total invasiones murcia, 1885") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 100, low = "blue", mid = "white", high = "red"
  )

g <- ggplot(
  df_colera_invasiones.merged.CCAAmurcia.agg,
  aes(
    x = Fecha,
    y = Total_invasiones,
    group = `Codigo Ine`,
    color = `Codigo Ine`
  )
) +
  geom_line() + geom_point(size = 2) + theme_bw()

g
g + theme(legend.position = "none")
g + gghighlight(`Codigo Ine` == "30030")
ggplotly(g)


p <- 
  ggplot(mapsf_murcia.merged) + geom_sf(aes(fill = Total_invasiones)) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 100,
    low = "blue",
    mid = "white",
    high = "red"
  ) +
  transition_time(Fecha) +
  labs(title = "Month: {round(frame_time, 0)}")

animate(p, render = gifski_renderer())
anim_save("murcia.gif")
