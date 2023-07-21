library(readxl)
library(data.table)
library(zoo)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
RAIN_PLOTS_DIR <- "rain_plots"
dir.create(RAIN_PLOTS_DIR, showWarnings = FALSE)
SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)

LLUVIA_STR <- "lluvia"
ANO_STR <- "1885"
NAMEMONTHS_LIST <-
  c("Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Setiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  )

NUMMONTHS_LIST <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
DATE_FORMAT <- "%Y-%m-%d"
CODIGOINE_STR <- "CODIGOINE"


# functions ---------------------------------------------------------------


create_rainTS <- function(df_rain, location) {
  
  df_rain.tmp <- subset(df_rain, localidad == location)
  
  #
  # TODO: create ts
  #
  
  # print(
  ggplot(df_rain.tmp, aes(x = mes, y = lluvia)) + 
    geom_line() + 
    ylab("mm") + 
    ggtitle(paste0(LLUVIA_STR, " mensual ", location, ", ", ANO_STR)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_x_continuous(
      breaks = as.numeric(df_rain.tmp$mes),
      labels = format(df_rain.tmp$mes, "%b")
    )
  # )
  
  ggsave(paste0(RAIN_PLOTS_DIR, "/ts.rain_", location, ".png"), dpi = 300, limitsize = TRUE)
  
}


# main --------------------------------------------------------------------


# read rain
df_rain <-
  read_excel(
    paste(DATA_DIR, "Temperaturas 1885 BBVA Leonardo.xlsx", sep = "/"),
    sheet = LLUVIA_STR,
    range = cell_rows(4:83)
  )

# remove NA
df_rain <- na.omit(df_rain) 

# change format of month's columns from chr to num
df_rain[, 2:13] <- sapply(df_rain[, 2:13], as.numeric)

# create new column "mes"
df_rain.parsed <-
  melt(
    setDT(df_rain[, 1:16]),
    id.vars = c("localidad", "longitud", "latitud", "codigo ine"),
    variable.name = "mes",
    value.name = LLUVIA_STR
  )

# order by "localidad"
df_rain.parsed <- df_rain.parsed[order(df_rain.parsed$localidad),]

# format as character "mes" column
df_rain.parsed$mes <- as.character(df_rain.parsed$mes)

# rename months from name to number
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[1]] <-
  paste(ANO_STR, NUMMONTHS_LIST[1], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[2]] <-
  paste(ANO_STR, NUMMONTHS_LIST[2], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[3]] <-
  paste(ANO_STR, NUMMONTHS_LIST[3], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[4]] <-
  paste(ANO_STR, NUMMONTHS_LIST[4], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[5]] <-
  paste(ANO_STR, NUMMONTHS_LIST[5], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[6]] <-
  paste(ANO_STR, NUMMONTHS_LIST[6], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[7]] <-
  paste(ANO_STR, NUMMONTHS_LIST[7], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[8]] <-
  paste(ANO_STR, NUMMONTHS_LIST[8], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[9]] <-
  paste(ANO_STR, NUMMONTHS_LIST[9], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[10]] <-
  paste(ANO_STR, NUMMONTHS_LIST[10], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[11]] <-
  paste(ANO_STR, NUMMONTHS_LIST[11], sep = "-")
df_rain.parsed$mes[df_rain.parsed$mes == NAMEMONTHS_LIST[12]] <-
  paste(ANO_STR, NUMMONTHS_LIST[12], sep = "-")

# remove NA values
df_rain.parsed <- na.omit(df_rain.parsed)

if(.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "English")
} else {
  Sys.setlocale("LC_TIME", "C")
}

# format as yearmon "mes" column
df_rain.parsed$mes <- as.yearmon(df_rain.parsed$mes) 

# save rain 
write.csv(df_rain.parsed, "rain.csv", fileEncoding = "UTF-8")

# generate all time series of each "localidad"
rain_localidades <- unique(df_rain.parsed$localidad)

for (localidad in rain_localidades) {
  
  create_rainTS(df_rain.parsed, localidad)
  
}

if(.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "Catalan_Spain.1252")
} else {
  Sys.setlocale("LC_TIME", "ca_ES.UTF-8")
}


# map ---------------------------------------------------------------------


map_municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
map_municipios <- subset(map_municipios, CODNUT1 != "ES7")
summary(map_municipios)

df_rain.parsed$mes <- as.Date(df_rain.parsed$mes)
df_rain.parsed$mes <- month(as.POSIXlt(df_rain.parsed$mes, format = DATE_FORMAT))

df_rain.parsed.w <-
  reshape(
    df_rain.parsed,
    timevar = "mes",
    idvar = "codigo ine",
    direction = "wide"
  )

df_rain.parsed.w <- na.omit(df_rain.parsed.w)
colnames(df_rain.parsed.w)[1] <- CODIGOINE_STR
df_rain.parsed.w$CODIGOINE <- as.character(df_rain.parsed.w$CODIGOINE)

print(df_rain.parsed.w[1:2, ])
print(map_municipios[1:2, ])

map_rain <- left_join(map_municipios, df_rain.parsed.w, by = CODIGOINE_STR)
print(map_rain[1:2, ])

mapsf_rain <- st_as_sf(map_rain)
mapsf_rain <- gather(mapsf_rain, mes, lluvia, paste0(LLUVIA_STR, ".", 6:11))
mapsf_rain$mes <- as.integer(substring(mapsf_rain$mes, 8, 9))

ggplot(mapsf_rain) + geom_sf(aes(fill = lluvia)) +
  ggtitle(paste0(LLUVIA_STR, " España", ", ", ANO_STR)) + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(midpoint = 174, low = "blue", mid = "white", high = "red")
