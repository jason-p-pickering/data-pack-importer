#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#I. Set up distribution source files####
#////////////////////////////////////////////////////
#Paste file path to site-level export here:
    siteExportPath="/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Distribution Source Files/COP18DistributionSource_20180214_2.csv"
#Where to save distribution source files:
    sourceFolder="/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Distribution Source Files/"
    
#Distribution Sources
    siteExport<- read.csv(file=siteExportPath,stringsAsFactors=FALSE,header=TRUE)
    saveRDS(siteExport,paste0(sourceFolder,"siteExport.rda"))

#@sjackson: Document this function.  
    
distrSource <- function(df,FY=NULL) {
      
      clusterMap<-datapackimporter::clusters
      rCOP18deMap<-datapackimporter::rCOP18deMap
      
      ds <- df %>%
        dplyr::filter(FiscalYear==FY) %>%
        dplyr::filter(Value !=0) %>%
        dplyr::filter(!is.na(Value)) %>%
        dplyr::filter(PSNUuid != "") %>%
        #Remove "Sensitive Site" data
        dplyr::filter(!orgUnit %in% datapackimporter::sites_exclude) %>%
        #Create COPuid for easy re-mapping
        dplyr::mutate(pd_2017_2018_S=paste(dataElement,categoryOptionCombo,sep=".",collapse=NULL)) %>%
        #Pull in FY2019 PSNU-level dataElement and categoryOptionCombo codes
        #TODO: Redothis with inner_join
        merge(rCOP18deMap[rCOP18deMap$FiscalYear==FY,c("pd_2017_2018_S","pd_2019_P","FiscalYear")],by=c("pd_2017_2018_S")) %>%
        dplyr::select(-FiscalYear.y,-FiscalYear.x,-dataElement,-categoryOptionCombo) %>%
        #Constrain to only those data values entered against dataElements mappable to FY19
        filter(!is.na(pd_2019_P)) %>%
        #Create new dataElement and categoryOptionCombo codes
        mutate(dataElement=str_extract(pd_2019_P,"^(.)+(?=(\\.))"),
               categoryOptionCombo=str_extract(pd_2019_P,"(?<=(\\.))(.)+$")) %>%
        select(-pd_2019_P,-pd_2017_2018_S) %>%
        select(PSNUuid,attributeOptionCombo,dataElement,categoryOptionCombo,orgUnit,Value)
      
      return(ds)
    }
    
#Create cluster level distribution from site level file. 
clusterDistribution <- function(df) {	
    	
    clusterMap<-datapackimporter::clusters	
    	
    clusterDistr <- df %>%	
                      #Sum up to PSNU level	
                          select(-orgUnit) %>%	
                          group_by(PSNUuid,attributeOptionCombo,dataElement,categoryOptionCombo) %>%	
                          summarise(psnuValue=sum(Value)) %>%	
                      #Merge in Cluster groupings	
                          ungroup() %>%	
                          left_join(clusterMap[,c("psnuuid","cluster_psnuuid")], by=c("PSNUuid"="psnuuid")) %>%	
                      #Filter to include only Clustered locations	
                          filter(!is.na(cluster_psnuuid)) %>%	
                      #Summarize by Cluster	
                          group_by(cluster_psnuuid,attributeOptionCombo,dataElement,categoryOptionCombo) %>%	
                          mutate(psnuPct = psnuValue/sum(psnuValue)) %>%	
                          ungroup() %>%	
                      #Create id to join with Data Pack dataset	
                          mutate(whereWhoWhatHuh=paste(cluster_psnuuid,attributeOptionCombo,dataElement,categoryOptionCombo,sep=".")) %>%	
                          select(whereWhoWhatHuh,PSNUuid,psnuPct)	
                return(clusterDistr)	
}

    #' @export
    #' @title siteDistribution()
    #'
    #' @description Creates site-level percentages against which Data Pack values can be multiplied for distribution purposes
    #' @param df Name of distribution source file.
    #' @return Returns a dataframe ready for use in distributing site level targets.
    #'
    
    siteDistribution<-function(df) {
      
      clusterMap<-datapackimporter::clusters
      
      siteDistr <- df %>%
        #Remove all dedupe data
        filter(!attributeOptionCombo %in% c("YGT1o7UxfFu","X8hrDf6bLDC")) %>%
        #Pull in ClusterUIDs
        left_join(clusterMap[,c("psnuuid","cluster_psnuuid")], by=c("PSNUuid"="psnuuid")) %>%
        mutate(dpOrgUnit=case_when(is.na(cluster_psnuuid)~PSNUuid
                                   ,TRUE~cluster_psnuuid)) %>%
        select(dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo,orgUnit,Value) %>%
        #Form percentage distributions across site within PSNU/Cluster x IM x DE.COC combination
        group_by(dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo) %>%
        mutate(sitePct = (Value)/sum(Value)) %>%
        #Create id to join with Data Pack dataset
        mutate(whereWhoWhatHuh=paste(dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo,sep=".")) %>%
        #Reorder columns; drop Value, keep percentages
        ungroup() %>%
        select(whereWhoWhatHuh,orgUnit,sitePct)
      
      return(siteDistr)
      
    }
    
    
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#II. PREPARE SITE-LEVEL DISTRIBUTION MATRICES####
#////////////////////////////////////////////////////
#No Dedupes
#Mapped to Cluster/PSNU -- not pure PSNU
siteDistribution(distrSourceFY17)%>%
        saveRDS(paste0(sourceFolder,"distrSiteFY17.rda"))
    
siteDistribution(distrSourceFY18)%>%
    saveRDS(paste0(sourceFolder,"distrSiteFY18.rda"))


    
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#III. PREPARE CLUSTER-PSNU DISTRIBUTION MATRICES####
#////////////////////////////////////////////////////
#Contains Dedupes
#Includes clustered org units only
clusterDistribution(distrSourceFY17) %>%
    saveRDS(paste0(sourceFolder,"distrClusterFY17.rda"))

clusterDistribution(distrSourceFY18) %>%
    saveRDS(paste0(sourceFolder,"distrClusterFY18.rda"))

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
