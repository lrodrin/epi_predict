library(dplyr)
library(lubridate)
library(sf)
library(spdep)
library(rgdal)
library(ggplot2)
library(tmap)

load("colera_data.RData")

df_colera.groupByProvinciaMunicipioFecha <- merge(df_colera.merged.month, df_distances, by = CODIGO_INE_STR)
df_colera.groupByProvinciaMunicipioFecha <- df_colera.groupByProvinciaMunicipioFecha[, c(1, 6:7, 2, 3:5, 8:9, 10:17)]
head(df_colera.groupByProvinciaMunicipioFecha)

mapS.municipios <- st_read(paste(SHAPES_DATA_DIR, "Municipios_IGN.shp", sep = "/"), quiet = TRUE)
mapS.municipios <- subset(mapS.municipios, CODNUT1 != "ES7" & CODNUT2 != "ES53") 
mapS.municipios <- subset(mapS.municipios, !(CODIGOINE %in% c(51001, 52001))) 
head(mapS.municipios)

mapS.colera_stp <- merge(mapS.municipios, df_colera.groupByProvinciaMunicipioFecha, by.x = CODIGOINE_STR, by.y = CODIGO_INE_STR)
head(mapS.colera_stp)

mapS.colera_stp.zaragoza <- subset(mapS.colera_stp, Provincia == "zaragoza")
mapS.colera_stp.valencia <- subset(mapS.colera_stp, Provincia == "valencia")
head(mapS.colera_stp.zaragoza)
head(mapS.colera_stp.valencia)

# Convert to spatialpointdataframe
spdf_mapS.colera_stp.zaragoza <- as_Spatial(mapS.colera_stp.zaragoza)
spdf_mapS.colera_stp.valencia <- as_Spatial(mapS.colera_stp.valencia)

# compute Rook contiguity weight matrix
wm_r.zaragoza <- poly2nb(spdf_mapS.colera_stp.zaragoza, queen = FALSE)
wm_r.valencia <- poly2nb(spdf_mapS.colera_stp.valencia, queen = FALSE)

# compute Queen contiguity weight matrix
wm_q.zaragoza <- poly2nb(spdf_mapS.colera_stp.zaragoza, queen = TRUE)
wm_q.valencia <- poly2nb(spdf_mapS.colera_stp.valencia, queen = TRUE)

# save coordinates to variable
for(i in 6:11) {
  coords <- paste("coords.zaragoza", i, sep = "")
  assign(coords, coordinates(subset(spdf_mapS.colera_stp.zaragoza, Fecha == 6)))
}
for(i in 6:11) {
  coords <- paste("coords.valencia", i, sep = "")
  assign(coords, coordinates(subset(spdf_mapS.colera_stp.valencia, Fecha == 6)))
}

# coords.zaragoza <- coordinates(spdf_mapS.colera_stp.zaragoza)
# coords.valencia <- coordinates(spdf_mapS.colera_stp.valencia)

# par(mfrow=c(1,2))
# plot(spdf_mapS.colera_stp.valencia, border="lightgrey")
# plot(wm_q.valencia, coords.valencia, pch = 19, cex = 0.6, add = TRUE, col= "red", main="Queen Contiguity")
# title(main = "Queen Contiguity")
# plot(spdf_mapS.colera_stp.valencia, border="lightgrey")
# plot(wm_r.valencia, coords.valencia, pch = 19, cex = 0.6, add = TRUE, col = "red", main="Rook Contiguity")
# title(main = "Rook Contiguity")

for(i in 6:11) {
  var_names <- paste("coords.zaragoza", i, sep="")  
  coords <- get(var_names, envir = globalenv())
  knb <- knn2nb(knearneigh(coords, k = 8, longlat = FALSE))
  assign(paste("knb_lw.zaragoza", i, sep = ""), nb2listw(knb, style = "B"))
}
for(i in 6:11) {
  var_names <- paste("coords.valencia", i, sep="")  
  coords <- get(var_names, envir = globalenv())
  knb <- knn2nb(knearneigh(coords, k = 8, longlat = FALSE))
  assign(paste("knb_lw.valencia", i, sep = ""), nb2listw(knb, style = "B"))
}

# knb.zaragoza <- knn2nb(knearneigh(coords.zaragoza, k = 8, longlat = FALSE))
# knb.valencia <- knn2nb(knearneigh(coords.valencia, k = 8, longlat = FALSE))
# knb_lw.zaragoza <- nb2listw(wm_r.zaragoza)
# knb_lw.valencia <- nb2listw(wm_r.valencia)

# plot(spdf_mapS.colera_stp.valencia, border = "lightgrey")
# plot(knb.valencia, coords.valencia, pch = 19, cex = 0.6, add = TRUE, col = "red")
# title(main = "Adaptive Distance Based")

for(i in 6:11) {
  var_names <- paste("knb_lw.zaragoza", i, sep="")  
  knb <- get(var_names, envir = globalenv())
  moranmc <- paste("moranmc_month.zaragoza", i, sep = "")
  assign(moranmc, moran.mc(spdf_mapS.colera_stp.zaragoza[spdf_mapS.colera_stp.zaragoza$Fecha == i, ][["Total_invasiones"]], listw = knb, nsim = 39, zero.policy = TRUE, na.action = na.omit))
}
for(i in 6:11) {
  var_names <- paste("knb_lw.valencia", i, sep="")  
  knb <- get(var_names, envir = globalenv())
  moranmc <- paste("moranmc_month.valencia", i, sep = "")
  assign(moranmc, moran.mc(spdf_mapS.colera_stp.valencia[spdf_mapS.colera_stp.valencia$Fecha == i, ][["Total_invasiones"]], listw = knb, nsim = 39, zero.policy = TRUE, na.action = na.omit))
}

moranmctable.zaragoza <- data.frame(matrix(vector(), ncol = 3))
moranmctable.valencia <- data.frame(matrix(vector(), ncol = 3))
names(moranmctable.zaragoza) <- c("month", "p-value", "statistics")
names(moranmctable.valencia) <- c("month", "p-value", "statistics")

for(i in 6:11){
  var_names <- paste("moranmc_month.zaragoza", i, sep = "")  
  moranmc <- get(var_names, envir = globalenv())
  moranmctable.zaragoza <- rbind(moranmctable.zaragoza, data.frame(i, moranmc$p.value, moranmc$statistic)) 
}
for(i in 6:11){
  var_names <- paste("moranmc_month.valencia", i, sep = "")  
  moranmc <- get(var_names, envir = globalenv())
  moranmctable.valencia <- rbind(moranmctable.valencia, data.frame(i, moranmc$p.value, moranmc$statistic)) 
}

ggplot(moranmctable.zaragoza, aes(x = i, y = moranmc.p.value))+
  geom_line() + xlab("Months (6-11)") + ylab("p-value") + ggtitle("Moran I value over months 6 to 11 - zaragoza")

ggplot(moranmctable.valencia, aes(x = i, y = moranmc.p.value))+
  geom_line() + xlab("Months (6-11)") + ylab("p-value") + ggtitle("Moran I p-value over months 6 to 11 - valencia")

for(i in 6:11) {
  var_names <- paste("knb_lw.zaragoza", i, sep="")  
  knb <- get(var_names, envir = globalenv())
  localMI  <- paste("lmoran_month.zaragoza", i, sep = "")
  cm_localMI <- paste("cm_localMI_month.zaragoza", i, sep="")
  assign(localMI, localmoran(spdf_mapS.colera_stp.zaragoza[spdf_mapS.colera_stp.zaragoza$Fecha == i, ][["Total_invasiones"]], knb))
  assign(cm_localMI, cbind(spdf_mapS.colera_stp.zaragoza[spdf_mapS.colera_stp.zaragoza$Fecha == i, ], eval(as.name(localMI))))
}
for(i in 6:11) {
  var_names <- paste("knb_lw.valencia", i, sep="")  
  knb <- get(var_names, envir = globalenv())
  localMI  <- paste("lmoran_month.valencia", i, sep = "")
  cm_localMI <- paste("cm_localMI_month.valencia", i, sep="")
  assign(localMI, localmoran(spdf_mapS.colera_stp.valencia[spdf_mapS.colera_stp.valencia$Fecha == i, ][["Total_invasiones"]], knb))
  assign(cm_localMI, cbind(spdf_mapS.colera_stp.valencia[spdf_mapS.colera_stp.valencia$Fecha == i, ], eval(as.name(localMI))))
}

# tm_shape(cm_localMI_month.zaragoza6) +
#   tm_fill(col = "Ii", 
#           style = "pretty", 
#           title = "month 6 local moran statistics") +
#   tm_borders(alpha = 0.5)
# 
# tm_shape(cm_localMI_month.zaragoza6) +
#   tm_fill(col = "Pr.z....E.Ii..", 
#           breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
#           palette = "-Blues", 
#           title = "month 6 local Moran's I p-values") +
#   tm_borders(alpha = 0.5)
# 
# moran.plot(subset(mapS.colera_stp.zaragoza, Fecha == 6)$Total_invasiones, 
#            knb_lw.zaragoza6,
#            labels = as.character(subset(mapS.colera_stp.zaragoza, Fecha == 6)$Municipio),
#            xlab = "Total invasiones in Zaragoza month 6", 
#            ylab = "Spatially Lag cholera in Zaragoza")

colors <- c("#ffffff", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
clusters <- c("insignificant", "low-low", "low-high", "high-low", "high-high")

for(i in 6:11) {
  localMI <- paste("lmoran_month.zaragoza", i, sep = "")
  quadrant <- vector(mode = "numeric", length = nrow(eval(as.name(localMI))))
  DV <- spdf_mapS.colera_stp.zaragoza[spdf_mapS.colera_stp.zaragoza$Fecha == i, ][["Total_invasiones"]] - mean(spdf_mapS.colera_stp.zaragoza[spdf_mapS.colera_stp.zaragoza$Fecha == i, ][["Total_invasiones"]])
  
  C_mI <- eval(as.name(localMI))[,1]- mean(eval(as.name(localMI))[,1])
  signif <- 0.05
  quadrant[DV >0 & C_mI>0] <- 4
  quadrant[DV <0 & C_mI<0] <- 1
  quadrant[DV <0 & C_mI>0] <- 2
  quadrant[DV >0 & C_mI<0] <- 3
  quadrant[eval(as.name(localMI))[,5]>signif] <- 0

  cm_localMI <- paste("cm_localMI_month.zaragoza", i, sep="")
  
  quadrant_df <- data.frame("quadrant" = quadrant)
  assign(cm_localMI, cbind(eval(as.name(cm_localMI)), quadrant_df))
  
  LisaPlotName<- paste("lplot_month.zaragoza", i, sep="")
  LisaPlot <- tm_shape(eval(as.name(cm_localMI))) +
    tm_fill(col = "quadrant", style = "cat", palette = colors[c(sort(unique(quadrant)))+1], labels = clusters[c(sort(unique(quadrant)))+1]) +
    tm_borders(alpha=0.5) +
    tm_view(set.zoom.limits = c(11,17))+
    tm_layout(paste("month",i), 
              legend.outside=TRUE)
  assign(LisaPlotName, LisaPlot)
  
}

for(i in 6:11) {
  localMI <- paste("lmoran_month.valencia", i, sep = "")
  quadrant <- vector(mode = "numeric", length = nrow(eval(as.name(localMI))))
  DV <- spdf_mapS.colera_stp.valencia[spdf_mapS.colera_stp.valencia$Fecha == i, ][["Total_invasiones"]] - mean(spdf_mapS.colera_stp.valencia[spdf_mapS.colera_stp.valencia$Fecha == i, ][["Total_invasiones"]])
  
  C_mI <- eval(as.name(localMI))[,1]- mean(eval(as.name(localMI))[,1])
  signif <- 0.05
  quadrant[DV >0 & C_mI>0] <- 4
  quadrant[DV <0 & C_mI<0] <- 1
  quadrant[DV <0 & C_mI>0] <- 2
  quadrant[DV >0 & C_mI<0] <- 3
  quadrant[eval(as.name(localMI))[,5]>signif] <- 0
  
  cm_localMI <- paste("cm_localMI_month.valencia", i, sep="")
  
  quadrant_df <- data.frame("quadrant" = quadrant)
  assign(cm_localMI, cbind(eval(as.name(cm_localMI)), quadrant_df))
  
  LisaPlotName<- paste("lplot_month.valencia", i, sep="")
  LisaPlot <- tm_shape(eval(as.name(cm_localMI))) +
    tm_fill(col = "quadrant", style = "cat", palette = colors[c(sort(unique(quadrant)))+1], labels = clusters[c(sort(unique(quadrant)))+1]) +
    tm_borders(alpha=0.5) +
    tm_view(set.zoom.limits = c(11,17))+
    tm_layout(paste("month",i), 
              legend.outside=TRUE)
  assign(LisaPlotName, LisaPlot)
  
}

tmap_arrange(lplot_month.zaragoza6, lplot_month.zaragoza7, lplot_month.zaragoza8, lplot_month.zaragoza9, lplot_month.zaragoza10, lplot_month.zaragoza11, ncol = 2)
tmap_arrange(lplot_month.valencia6, lplot_month.valencia7, lplot_month.valencia8, lplot_month.valencia9, lplot_month.valencia10, lplot_month.valencia11, ncol = 2)

for(i in 6:11) {
  var_names <- paste("knb_lw.zaragoza", i, sep="")  
  knb <- get(var_names, envir = globalenv())
  localGI <- paste("gi_week",i,sep="")
  assign(localGI, cbind(spdf_mapS.colera_stp.zaragoza[spdf_mapS.colera_stp.zaragoza$Fecha == i, ], 
                        data.frame("gstat_adaptive" = as.matrix(localG(spdf_mapS.colera_stp.zaragoza[spdf_mapS.colera_stp.zaragoza$Fecha == i, ][["Total_invasiones"]], knb)))))
  
  gi <- paste("gimap_month.zaragoza", i, sep="")
  giplot <- tm_shape(eval(as.name(localGI ))) +
    tm_fill(col = "gstat_adaptive",
            style = "pretty",
            palette = "-RdBu",
            title = "local Gi") +
    tm_borders(alpha=0.5) +
    tm_view(set.zoom.limits = c(11,17))+
    tm_layout(paste("month",i),
              legend.outside=TRUE)
  
  assign(gi,  giplot)
}
for(i in 6:11) {
  var_names <- paste("knb_lw.valencia", i, sep="")  
  knb <- get(var_names, envir = globalenv())
  localGI <- paste("gi_week",i,sep="")
  assign(localGI, cbind(spdf_mapS.colera_stp.valencia[spdf_mapS.colera_stp.valencia$Fecha == i, ], 
                        data.frame("gstat_adaptive" = as.matrix(localG(spdf_mapS.colera_stp.valencia[spdf_mapS.colera_stp.valencia$Fecha == i, ][["Total_invasiones"]], knb)))))
  
  gi <- paste("gimap_month.valencia", i, sep="")
  giplot <- tm_shape(eval(as.name(localGI ))) +
    tm_fill(col = "gstat_adaptive",
            style = "pretty",
            palette = "-RdBu",
            title = "local Gi") +
    tm_borders(alpha=0.5) +
    tm_view(set.zoom.limits = c(11,17))+
    tm_layout(paste("month",i),
              legend.outside=TRUE)
  
  assign(gi,  giplot)
}


tmap_arrange(gimap_month.zaragoza6, gimap_month.zaragoza7, gimap_month.zaragoza8, gimap_month.zaragoza9, gimap_month.zaragoza10, gimap_month.zaragoza11, ncol = 2)
tmap_arrange(gimap_month.valencia6, gimap_month.valencia7, gimap_month.valencia8, gimap_month.valencia9, gimap_month.valencia10, gimap_month.valencia11, ncol = 2)
