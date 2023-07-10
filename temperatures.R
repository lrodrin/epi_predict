library(readxl)
library(data.table)
library(zoo)


# constants ---------------------------------------------------------------


DATA_DIR <- "data"
dir.create(DATA_DIR, showWarnings = FALSE)
TEMPE_PLOTS_DIR <- "tempe_plots"
dir.create(TEMPE_PLOTS_DIR, showWarnings = FALSE)

TEMPERATURA_STR <- "temperatura"
LOCALIDAD_STR <- "localidad"
MES_STR <- "mes"
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
    ggtitle(paste0("temperatura mensual ", location, ", ", ANO_STR)) +
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
    setDT(df_temperatures[, 1:13]),
    id.vars = c(LOCALIDAD_STR),
    variable.name = MES_STR,
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

# convert NA values to 0
df_temperatures.parsed$temperatura[is.na(df_temperatures.parsed$temperatura)] <- 0 

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
