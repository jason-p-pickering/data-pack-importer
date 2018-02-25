library(feather)
library(dplyr)

source_dir<-"/home/jason/consultancy/datim/datapack/"
setwd(source_dir)
s_17<-readRDS(file="distrSiteFY17.rda")%>%
  mutate(year = 2017,
         mode = "s")

col_names<-names(s_17)

s_18<-readRDS(file="/home/jason/consultancy/datim/datapack/distrSiteFY18.rda") %>%
  mutate(year = 2018,
         mode = "s")

c_17<-readRDS(file="/home/jason/consultancy/datim/datapack/distrClusterFY17.rda") %>%
  mutate(year = 2017,
         mode = "c") %>% `names<-`(.,col_names)
c_18<-readRDS(file="/home/jason/consultancy/datim/datapack/distrClusterFY18.rda") %>%
  mutate(year = 2017,
         mode = "c") %>% `names<-`(.,col_names)

write_feather(pct_distr1,"/home/jason/consultancy/datim/datapack/pct_distr.feather")
