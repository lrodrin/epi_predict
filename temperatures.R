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
TEMPE_PLOTS_DIR <- "tempe_plots"
dir.create(TEMPE_PLOTS_DIR, showWarnings = FALSE)
SHAPES_DATA_DIR <- "shapes"
dir.create(SHAPES_DATA_DIR, showWarnings = FALSE)

TEMPERATURA_STR <- "temperatura"
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


create_tempeTS <- function(df_temperatures, location) {
  
  df_temperatures.tmp <- subset(df_temperatures, localidad == location)
  
  #
  # TODO: create ts
  #
  
  # print(
  ggplot(df_temperatures.tmp, aes(x = mes, y = temperatura)) + 
    geom_line() + 
    ylab("grados (ºC)") + 
    ggtitle(paste0(TEMPERATURA_STR, " mensual ", location, ", ", ANO_STR)) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_x_continuous(
      breaks = as.numeric(df_temperatures.tmp$mes),
      labels = format(df_temperatures.tmp$mes, "%b")
    )
  # )
    
  ggsave(paste0(TEMPE_PLOTS_DIR, "/ts.temperatures_", location, ".png"), dpi = 300, limitsize = TRUE)
  
}


# main --------------------------------------------------------------------


# read temperatures
df_temperatures <-
  read_excel(
    paste(DATA_DIR, "Temperaturas 1885 BBVA Leonardo.xlsx", sep = "/"),
    sheet = TEMPERATURA_STR,
    range = cell_rows(4:83)
  )

# remove NA
df_temperatures <- na.omit(df_temperatures) 

# change format of month's columns from chr to num
df_temperatures[, 2:13] <- sapply(df_temperatures[, 2:13], as.numeric)

# create new column "mes"
df_temperatures.parsed <-
  melt(
    setDT(df_temperatures[, 1:16]),
    id.vars = c("localidad", "longitud", "latitud", "codigo ine"),
    variable.name = "mes",
    value.name = TEMPERATURA_STR
  )

# order by "localidad"
df_temperatures.parsed <- df_temperatures.parsed[order(df_temperatures.parsed$localidad),]

# format as character "mes" column
df_temperatures.parsed$mes <- as.character(df_temperatures.parsed$mes)

# rename months from name to number
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[1]] <-
  paste(ANO_STR, NUMMONTHS_LIST[1], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[2]] <-
  paste(ANO_STR, NUMMONTHS_LIST[2], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[3]] <-
  paste(ANO_STR, NUMMONTHS_LIST[3], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[4]] <-
  paste(ANO_STR, NUMMONTHS_LIST[4], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[5]] <-
  paste(ANO_STR, NUMMONTHS_LIST[5], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[6]] <-
  paste(ANO_STR, NUMMONTHS_LIST[6], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[7]] <-
  paste(ANO_STR, NUMMONTHS_LIST[7], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[8]] <-
  paste(ANO_STR, NUMMONTHS_LIST[8], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[9]] <-
  paste(ANO_STR, NUMMONTHS_LIST[9], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[10]] <-
  paste(ANO_STR, NUMMONTHS_LIST[10], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[11]] <-
  paste(ANO_STR, NUMMONTHS_LIST[11], sep = "-")
df_temperatures.parsed$mes[df_temperatures.parsed$mes == NAMEMONTHS_LIST[12]] <-
  paste(ANO_STR, NUMMONTHS_LIST[12], sep = "-")

# remove NA values
df_temperatures.parsed <- na.omit(df_temperatures.parsed)

if(.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "English")
} else {
  Sys.setlocale("LC_TIME", "C")
}

# format as yearmon "mes" column
df_temperatures.parsed$mes <- as.yearmon(df_temperatures.parsed$mes) 

# save temperatures 
write.csv(df_temperatures.parsed, "temperatures.csv", fileEncoding = "UTF-8")

# generate all time series of each "localidad"
tempe_localidades <- unique(df_temperatures.parsed$localidad)

for (localidad in tempe_localidades) {
  
  create_tempeTS(df_temperatures.parsed, localidad)
  
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
                      
df_temperatures.parsed$mes <- as.Date(df_temperatures.parsed$mes)
df_temperatures.parsed$mes <- month(as.POSIXlt(df_temperatures.parsed$mes, format = DATE_FORMAT))
df_temperatures.parsed.w <-
  reshape(
    df_temperatures.parsed,
    timevar = "mes",
    idvar = "codigo ine",
    direction = "wide"
  )

df_temperatures.parsed.w <- na.omit(df_temperatures.parsed.w)
colnames(df_temperatures.parsed.w)[1] <- CODIGOINE_STR
df_temperatures.parsed.w$CODIGOINE <- as.character(df_temperatures.parsed.w$CODIGOINE)

print(df_temperatures.parsed.w[1:2, ])
print(map_municipios[1:2, ])

map_tempe <- left_join(map_municipios, df_temperatures.parsed.w, by = CODIGOINE_STR)
print(map_tempe[1:2, ])

mapsf_tempe <- st_as_sf(map_tempe)
mapsf_tempe <- gather(mapsf_tempe, mes, temperatura, paste0(TEMPERATURA_STR, ".", 6:11))
mapsf_tempe$mes <- as.integer(substring(mapsf_tempe$mes, 13, 14))

ggplot(mapsf_tempe) + geom_sf(aes(fill = temperatura)) +
  ggtitle(paste0(TEMPERATURA_STR, " España", ", ", ANO_STR)) + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
    ) +
  scale_fill_gradient2(midpoint = 14, low = "blue", mid = "white", high = "red")
