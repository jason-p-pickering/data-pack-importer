#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#I. Set up dependencies####
#////////////////////////////////////////////////////
#Paste file path to site-level export here:
    siteExportPath="/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Distribution Source Files/COP18DistributionSource_20180214_2.csv"
#Where to save distribution source files:
    outFolder="/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Distribution Source Files"
    setwd(outFolder)

    
siteExport<- read.csv(file=siteExportPath,stringsAsFactors=FALSE,header=TRUE) %>%
    saveRDS("./siteExport.rda")

pacman::p_load(dplyr,tidyr)

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#II. Set up functions####
#////////////////////////////////////////////////////
    

#' @title distrSource()
#'
#' @description Performs row and column filtering on export from PDH to arrive at dataset ready for use in Data Pack Importer.
#' @param df Reference to PDH export.
#' @param FY 4 digit numeric value indicating Fiscal Year to filter and perform operations against.
#' @return Returns a dataframe ready for use in preparing source files.
#'  
    
distrSource <- function(df,FY=NULL) {
      
      rCOP18deMap<-datapackimporter::rCOP18deMap
      
      .dots=ifelse(is.null(FY),"is.integer(FiscalYear)","FiscalYear==FY")
      
      ds <- df %>%
        dplyr::filter_(.dots=.dots) %>%
        dplyr::filter(Value !=0 & !is.na(Value) & PSNUuid != "") %>%
        #Remove "Sensitive Site" data
            dplyr::filter(!orgUnit %in% datapackimporter::sites_exclude) %>%
        #Create COPuid for easy re-mapping
            dplyr::mutate(pd_2017_2018_S=paste(dataElement,categoryOptionCombo,sep=".",collapse=NULL)) %>%
            dplyr::select(-dataElement,-categoryOptionCombo) %>%
        #Pull in FY2019 PSNU-level dataElement and categoryOptionCombo codes
            dplyr::left_join(rCOP18deMap[,c("pd_2017_2018_S","pd_2019_P","FiscalYear")],by=c("pd_2017_2018_S","FiscalYear")) %>%
        #Constrain to only those data values entered against dataElements mappable to FY19
            dplyr::filter(!is.na(pd_2019_P)) %>%
        #Create new dataElement and categoryOptionCombo codes
            tidyr::separate(pd_2019_P,into=c("dataElement","categoryOptionCombo"),sep="\\.",extra="warn",fill="warn",remove=T) %>%
            dplyr::select(uidlevel3,PSNUuid,attributeOptionCombo,dataElement,categoryOptionCombo,orgUnit,Value)
      
      return(ds)
    }
    

    
#' @title clusterDistribution()
#'
#' @description Creates PSNU-level percentages against which Data Pack values can be multiplied for distribution purposes.
#' @param df Reference to PDH export
#' @param FY 4 digit numeric value indicating Fiscal Year to filter and perform operations against.
#' @return Returns a dataframe ready for use in distributing from Cluster to PSNU level.
#'  

clusterDistribution <- function(df, FY=NULL) {	
    	
    clusterMap<-datapackimporter::clusters
    
    clusterDistr <- distrSource(df,FY) %>%
        #Remove all dedupe data
            dplyr::filter(!attributeOptionCombo %in% c("YGT1o7UxfFu","X8hrDf6bLDC")) %>%
        #Sum up to PSNU level	
            dplyr::select(-orgUnit) %>%	
            dplyr::group_by(uidlevel3,PSNUuid,attributeOptionCombo,dataElement,categoryOptionCombo) %>%	
            dplyr::summarise(psnuValue=sum(Value)) %>%
            dplyr::ungroup() %>%
      #Merge in Cluster groupings
            dplyr::left_join(clusterMap[,c("psnuuid","cluster_psnuuid")], by=c("PSNUuid"="psnuuid")) %>%
      #Create org unit level resembling Data Pack (Clustered mixed with nonclustered/PSNU)
            dplyr::mutate(dpOrgUnit=case_when(is.na(cluster_psnuuid)~PSNUuid
                                           ,TRUE~cluster_psnuuid)) %>%
            dplyr::select(-cluster_psnuuid) %>%
      #Summarize by Cluster
            dplyr::group_by(uidlevel3,dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo) %>%	
            dplyr::mutate(psnuPct = psnuValue/sum(psnuValue)) %>%	
            dplyr::ungroup() %>%	
      #Create id to join with Data Pack dataset	
            dplyr::mutate(whereWhoWhatHuh=paste(dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo,sep=".")) %>%	
            dplyr::select(uidlevel3,whereWhoWhatHuh,PSNUuid,psnuPct)
    
    return(clusterDistr)	
}
    


#' @export
#' @title siteDistribution()
#'
#' @description Creates site-level percentages against which Data Pack values can be multiplied for distribution purposes
#' @param df Name of distribution source file.
#' @param FY 4 digit numeric value indicating Fiscal Year to filter and perform operations against.
#' @return Returns a dataframe ready for use in distributing site level targets.
#'

siteDistribution<-function(df,FY) {
  
  clusterMap<-datapackimporter::clusters
  
  siteDistr <- distrSource(df,FY) %>%
    #Remove all dedupe data
        dplyr::filter(!attributeOptionCombo %in% c("YGT1o7UxfFu","X8hrDf6bLDC")) %>%
    #Pull in ClusterUIDs
        dplyr::left_join(clusterMap[,c("psnuuid","cluster_psnuuid")], by=c("PSNUuid"="psnuuid")) %>%
        dplyr::mutate(dpOrgUnit=case_when(is.na(cluster_psnuuid)~PSNUuid
                                   ,TRUE~cluster_psnuuid)) %>%
        dplyr::select(uidlevel3,dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo,orgUnit,Value) %>%
    #Form percentage distributions across site within PSNU/Cluster x IM x DE.COC combination
        dplyr::group_by(uidlevel3,dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo) %>%
        dplyr::mutate(sitePct = (Value)/sum(Value)) %>%
        dplyr::ungroup() %>%
    #Create id to join with Data Pack dataset
        dplyr::mutate(whereWhoWhatHuh=paste(dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo,sep=".")) %>%
    #Reorder columns; drop Value, keep percentages
        dplyr::select(uidlevel3,whereWhoWhatHuh,orgUnit,sitePct)
  
  return(siteDistr)
}
    
    
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#III. PREPARE SITE-LEVEL DISTRIBUTION MATRICES####
#////////////////////////////////////////////////////
#No Dedupes
#Mapped to Cluster/PSNU -- not pure PSNU
siteDistribution(siteExport,2017)%>%
    saveRDS("./distrSiteFY17.rda")
    
siteDistribution(siteExport,2018)%>%
    saveRDS("./distrSiteFY18.rda")


    
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#IV. PREPARE CLUSTER-PSNU DISTRIBUTION MATRICES####
#////////////////////////////////////////////////////
#No Dedupes
#Maps from mixed clustered level (PSNU for nonclustered, Cluster for clustered, similar to as in Data Pack) to PSNU level
#Expected to see many percents as 100% for all nonclustered locales.
clusterDistribution(siteExport,2017) %>%
    saveRDS("./distrClusterFY17.rda")

clusterDistribution(siteExport,2018) %>%
    saveRDS("./distrClusterFY18.rda")



# library(feather)
# library(dplyr)
# 
# source_dir<-"/home/jason/consultancy/datim/datapack/"
# setwd(source_dir)
# s_17<-readRDS(file="distrSiteFY17.rda")%>%
#   mutate(year = 2017,
#          mode = "s")
# 
# col_names<-names(s_17)
# 
# s_18<-readRDS(file="/home/jason/consultancy/datim/datapack/distrSiteFY18.rda") %>%
#   mutate(year = 2018,
#          mode = "s")
# 
# c_17<-readRDS(file="/home/jason/consultancy/datim/datapack/distrClusterFY17.rda") %>%
#   mutate(year = 2017,
#          mode = "c") %>% `names<-`(.,col_names)
# c_18<-readRDS(file="/home/jason/consultancy/datim/datapack/distrClusterFY18.rda") %>%
#   mutate(year = 2017,
#          mode = "c") %>% `names<-`(.,col_names)
# 
# write_feather(pct_distr1,"/home/jason/consultancy/datim/datapack/pct_distr.feather")
