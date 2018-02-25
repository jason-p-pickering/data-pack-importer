options("repos"="http://cran.rstudio.com")

install.packages("devtools")
library(devtools)
install_github(repo="jason-p-pickering/datim-validation")
install_github(repo="jason-p-pickering/data-pack-importer", branch="master")
install_github(repo="tkunstek/openxlsx")
update.packages(ask=FALSE)
