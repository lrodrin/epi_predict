library(dplyr)
library(ggplot2)
library(dtwclust)
library(cowplot)
library(ggdendro)
library(envalysis)


load("colera_data.RData")


# constants ---------------------------------------------------------------


TS_START <- c("1885", "06", "27")
TS_FREQUENCY <- 1
ALL_PROVINCES <- c("zaragoza", "valencia", "granada", "murcia", "castellon", "teruel", "alicante", "navarra", "albacete", "almeria")


# functions ---------------------------------------------------------------


subsetByProvincia <- function(data, provincia) {
  subset(data, Provincia == provincia)
}


plotByProvincia <- function(data, provincia) {
  ggplot(data, aes(x = Fecha, y = Total_invasiones)) + 
    geom_line() + 
    ylab("Cases") + xlab("") +
    ggtitle(provincia) +
    theme_bw() 
}


fill_missing_values <- function(df, provincia, complete_dates) {
  res <- complete_dates %>%
    left_join(df, by = FECHA_STR) %>%
    mutate(
      Provincia = ifelse(is.na(Provincia), provincia, Provincia),
      Total_invasiones = ifelse(is.na(Total_invasiones), 0, Total_invasiones),
      Total_defunciones = ifelse(is.na(Total_defunciones), 0, Total_defunciones)
    )
  return(res)
}


plot_dendrograms_ts <- function(cluster_dtw_h, ts_list) {
  hcdata <- dendro_data(cluster_dtw_h, type = "rectangle")
  
  # create dendrogram
  dd <- ggplot(segment(hcdata)) +
    geom_segment(aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    )) +
    coord_flip() +
    scale_x_reverse(expand = c(0.2, 0)) +
    theme_dendro() +
    geom_text(
      data = hcdata$labels,
      aes(x, y, label = label),
      hjust = 0,
      angle = 0,
      vjust = -1,
      size = 5
    )
  
  # create time series plots
  ts_plots <- lapply(ts_list, function(ts) {
    ggplot(ts, aes(x = Fecha, y = Total_invasiones)) +
      geom_line() +
      geom_point() +
      ylab("Cases") + xlab(NULL) +
      theme_publish()
  })
  
  # combine time series plots
  pp <- cowplot::plot_grid(plotlist = ts_plots, ncol = 1)
  
  # arrange dendrogram and time series plots
  grid.arrange(pp, dd, ncol = 2, widths = c(4, 2))
}


# main --------------------------------------------------------------------


df_colera.groupByProvinciaFecha <- merge(
  df_colera_invasiones %>% group_by(Provincia, Fecha) %>% summarize(Total_invasiones = sum(invasiones)),
  df_colera_defunciones %>% group_by(Provincia, Fecha) %>% summarize(Total_defunciones = sum(defunciones)),
  by = c(PROVINCIA_STR, FECHA_STR)
)

# subset and plot by "Provincia"
df_by_provincia <- lapply(ALL_PROVINCES, function(prov) subsetByProvincia(df_colera.groupByProvinciaFecha, prov))
plots <- lapply(1:length(ALL_PROVINCES), function(i) plotByProvincia(df_by_provincia[[i]], ALL_PROVINCES[[i]]))

# add missing dates
complete_dates <- data.frame(Fecha = seq(min(df_by_provincia[[1]]$Fecha), max(df_by_provincia[[1]]$Fecha), by = "1 day"))
df_by_provincia <- lapply(seq_along(df_by_provincia), function(i) fill_missing_values(df_by_provincia[[i]], ALL_PROVINCES[[i]], complete_dates))

# create time series
ts_df_all <- lapply(df_by_provincia, function(df_prov) ts(df_prov$Total_invasiones, start = TS_START, frequency = TS_FREQUENCY))
names(ts_df_all) <- ALL_PROVINCES

# plot time series
ggplot(subset(df_colera.groupByProvinciaFecha, Provincia %in% ALL_PROVINCES), aes(x = Fecha, y = Total_invasiones, group = Provincia, colour = Provincia)) + 
  geom_line() +
  scale_color_discrete(name = PROVINCIA_STR) +
  ylab("Cases") + xlab("") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ Provincia, scales = "free_y", ncol = 2) +  
  theme_bw() + theme(legend.position = "none")

# hierarchical clustering with DTW
hcc_all <- tsclust(
  ts_df_all,
  type = "h",
  k = 2,
  distance = "dtw_basic",
  control = hierarchical_control(method = "average"),
  seed = 390,
  preproc = NULL,
  args = tsclust_args(dist = list(window.size = 7))
)

# plot dendrograms and time series
plot(hcc_all)
plot_dendrograms_ts(hcc_all, df_by_provincia)
