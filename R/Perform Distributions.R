#1) Enter presets####
    #Change this to suit your needs
        loadSecrets("/Users/scott/.secrets/datim.json")
    #Enter 2017 if FY17 Results chosen as base, 2018 if FY18 Targets, 0000 if chose not to distribute
        distribution.Method <- 2017
    #Enter file path for non-HTS Data Pack here:
        DataPack.Normal <- "/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Data Pack Files/zambia_datapack_import_test/zambia_dp_import.json"
    #Enter file path for HTS Data Pack here:
        DataPack.HTS <- "/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Data Pack Files/zambia_datapack_import_test/zambia_dp_import_hts.json"
    #Enter folder where Distribution Source Files are stored:
        sourceFolder="/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Distribution Source Files/"


        
#2) Load Data Pack####
dp_import <- fromJSON(DataPack.Normal)
dp_import_hts <- fromJSON(DataPack.HTS)

OU <- dp_import$wb_info$ou_name
OUuid <- dp_import$wb_info$ou_uid
has.Cluster <- case_when(OUuid %in% unique(clusterMap$operatingUnitUID)~1,TRUE~0)

#Combine HTS & non-HTS Data Packs
DataPack <- as_data_frame(dp_import$data) %>%
    rbind(as_data_frame(dp_import_hts$data)) %>%
    mutate(Value=as.numeric(value)) %>%
    select(-value)




#3) Distribute from Cluster to PSNU ####

#Handle _Military differently

#If Clustered, distribute to PSNU level
if(has.Cluster==1){
    psnuLevelTargets<-distributeCluster(DataPack,distribution.Method)
}

#Combine with IMPATT data for import


#Prepare PSNU-level Targets for DATIM import
psnuLevelTargets<-psnuLevelTargets %>%
    mutate(Value=trunc(Value+0.5))



#4) Distribute from Cluster/PSNU to Site ####
#Only distribute if user chose FY17 Results or FY18 Targets as base. If chose not to distribute to site, ths will pass over.
if(distribution.Method %in% c(2017,2018)) {
    siteLevelTargets <- distributeSite(DataPack,distribution.Method)
}

#Prepare Site-level Targets for DATIM import
siteLevelTargets<-siteLevelTargets %>%
    mutate(Value=trunc(Value+0.5))


#5) Export to Site-level Review File


GitHub<-(paste0("/Users/scott/Documents/GitHub"))
repo<-("/COP18-Target-Setting")

GoogleDrive <-("/Users/scott/Google Drive/PEPFAR/COP Targets/Development")

library(RCurl)
library(sqldf)
library(stringr)
library(reshape2)
library(dplyr)
library(tidyr)
library(httr)
library(jsonlite)
library(anytime)
library(foreign)


#install.packages("githubinstall")
#library(githubinstall)
#githubinstall("datimvalidation")

library(datimvalidation)