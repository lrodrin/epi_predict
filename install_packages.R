# define package names and versions
package_versions <- list(
  ggplot2 = NULL,
  readxl = NULL,
  dplyr = "1.1.2",
  tidyr = "1.3.0",
  lubridate = "1.9.2",
  tidyverse = "2.0.0",
  scales = "1.2.1",
  data.table = "1.14.8",
  zoo = "1.8-12",
  leafpop = "0.1.0",
  INLA = "21.2.23",
  openxlsx = "4.2.5.2",
  Dict = "0.1.0",
  sp = "1.6-0",
  sf = "1.0-12",
  terra = NULL,
  spdep = "1.2-8",
  sfdep = "0.2.3",
  stringi = "1.7.12",
  reshape2 = "1.4.4",
  tmap = "3.3.4"
)

# install packages not yet installed
install_specific_version <- function(package, version) {
  if (!package %in% rownames(installed.packages())) {
    install.packages(package, repos = NULL, type = "source")
  }
  if (!is.null(version) && packageVersion(package) != version) {
    install.packages(package, repos = NULL, type = "source", version = version)
  }
}

# install specific versions or latest versions
invisible(lapply(names(package_versions), function(pkg) install_specific_version(pkg, package_versions[[pkg]])))

# load packages
invisible(lapply(names(package_versions), function(pkg) {
  if (!is.null(package_versions[[pkg]])) {
    library(pkg, character.only = TRUE)
  }
}))
