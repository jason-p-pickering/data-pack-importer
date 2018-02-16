
#' @export
#' @title distrSource()
#'
#' @description Converts FY17/18 dataElements to FY19 PSNU level elements within DATIM extract
#' @param df Name of dataframe storing DATIM extract.
#' @param FY FiscalYear for which source distribution should be pulled.
#' @return Returns a dataframe ready for use in preparing source files.
#'
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

#' @export
#' @title clusterDistribution()
#'
#' @description Creates PSNU-level percentages against which Data Pack values can be multiplied for distribution purposes
#' @param df Name of distribution source file.
#' @return Returns a dataframe ready for use in distributing site level targets.
#'
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
#' @title distributeCluster()
#'
#' @description Distributes clustered Data Pack data to PSNU level for DATIM import
#' @param df Name of dataframe storing Data Pack data.
#' @param dm Distribution Method selection, whether based on FY17, FY18, or not distributed
#' @return Returns a dataframe structured for DATIM import.
#'

distributeCluster <- function(df,dm) {
    
    if(dm==2017) Pcts<-readRDS(paste0(sourceFolder,"distrClusterFY17.rda"))
    if(dm==2018) Pcts<-readRDS(paste0(sourceFolder,"distrClusterFY18.rda"))
    if(dm==0000) Pcts<-readRDS(paste0(sourceFolder,"distrClusterFY18.rda"))
    
    clusterMap<-datapackimporter::clusters
    
    #Prepare Cluster Averages
    clusterAvgs <- clusterMap %>%
        select(cluster_psnuuid,psnuuid) %>%
        group_by(cluster_psnuuid) %>%
        mutate(num=1,
               den=n()) %>%
        mutate(avg=num/den) %>%
        select(-num,-den)
    
    ds <- df %>%
        filter(orgunit %in% unique(clusterMap$cluster_psnuuid)) %>%
        mutate(whereWhoWhatHuh=paste(orgunit,attributeoptioncombo,dataelement,categoryoptioncombo,sep=".")) %>%
        left_join(Pcts,by=c("whereWhoWhatHuh")) %>%
        #Where there is no history at PSNU level, simply distribute evenly among all underlying PSNUs
            left_join(clusterAvgs,by=c("orgunit"="cluster_psnuuid")) %>%
            mutate(Value=case_when(is.na(psnuPct)~Value*avg
                                  ,TRUE~Value*psnuPct)) %>%
        mutate(orgunit=PSNUuid) %>%
        select(dataelement,period,orgunit,categoryoptioncombo,attributeoptioncombo,Value) %>%
        rbind(df[!df$orgunit %in% unique(clusterMap$cluster_psnuuid),])
    
    return(ds)
}




#' @export
#' @title distributeSite()
#'
#' @description Distributes Data Pack data (both Clustered and non-Clustered) to Site level for DATIM import
#' @param df Name of dataframe storing Data Pack data.
#' @param dm Distribution Method selection, whether based on FY17, FY18, or not distributed
#' @return Returns a dataframe structured for DATIM import.
#'

distributeSite <- function(df,dm) {
    
    if(dm==2017) Pcts<-distrSiteFY17
    if(dm==2018) Pcts<-distrSiteFY18
    if(dm==0000) stop("User selected not to distribute to site!")
    
    ds <- df %>%
        #Create id to link to percent distributions
            mutate(whereWhoWhatHuh=paste(orgunit,attributeoptioncombo,dataelement,categoryoptioncombo,sep=".")) %>%
        #Pull in distribution percentages
            left_join(Pcts,by=c("whereWhoWhatHuh")) %>%
            mutate(Value=Value*sitePct) %>%
            mutate(PSNUuid=orgunit
                   ,orgunit=orgUnit) %>%
        select(dataelement,period,PSNUuid,orgunit,categoryoptioncombo,attributeoptioncombo,Value)
    
    return(ds)
}


