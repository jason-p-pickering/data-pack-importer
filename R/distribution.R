#' @export
#' @title distributeCluster(data)
#'
#' @description Distributes clustered Data Pack data to PSNU level for DATIM import
#' @param d Data object returned from ImportSheets function. 
#' @return Returns an object structured for allocation to the import into the PNSU dataset.
#'

distributeCluster <- function(d) {
  
  cluster_psnuuid<-NULL
  psnuuid<-NULL
  n<-NULL
  num<-NULL
  den<-NULL
  orgUnit<-NULL
  attributeoptioncombo<-NULL
  dataelement<-NULL
  categoryoptioncombo<-NULL
  orgunit<-NULL
  value<-NULL
  PSNUuid<-NULL
  period<-NULL
  n<-NULL
  
  distros_path=d$wb_info$support_files_path
    #Default distribution is 2018 if not otherwise specified
    if (d$wb_info$distribution_method == 2017) {
      file_name = "distrClusterFY17.rda"
    } else {
      file_name = "distrClusterFY18.rda"
    }
  
    file_path = paste0(distros_path, file_name)
    
    if (!file.exists(file_path)) {
      stop(paste("Distribution file could not be found. Please check it exists at",file_path))
    }
    
    Pcts<-readRDS( file = file_path )
    clusterMap<-datapackimporter::clusters
    militaryUnits<-datapackimporter::militaryUnits
    
    #Prepare Cluster Averages
    clusterAvgs <- clusterMap %>%
        dplyr::select(cluster_psnuuid,psnuuid) %>%
        dplyr::group_by(cluster_psnuuid) %>%
        dplyr::mutate(num=1,
               den=n()) %>%
        dplyr::mutate(avg=num/den) %>%
        dplyr::select(-num,-den)
    
    ds <- d$data %>%
        dplyr::filter(orgunit %in% unique(clusterMap$cluster_psnuuid)) %>%
        dplyr::mutate(whereWhoWhatHuh=paste(orgunit,attributeoptioncombo,dataelement,categoryoptioncombo,sep=".")) %>%
        dplyr::left_join(Pcts,by=c("whereWhoWhatHuh")) %>%
        #Where there is no history at PSNU level, simply distribute evenly among all underlying PSNUs
        dplyr::left_join(clusterAvgs,by=c("orgunit"="cluster_psnuuid")) %>%
        dplyr::mutate(value=dplyr::case_when(is.na(psnuPct)~value*avg ,TRUE~value*psnuPct)) %>%
        dplyr::mutate(value = round(value)) %>%
        dplyr::filter(value != "0") 
        dplyr::mutate(orgunit=PSNUuid) %>%
        dplyr::select(dataelement,period,PSNUuid,orgunit,categoryoptioncombo,attributeoptioncombo,value) %>%
        dplyr::bind_rows(d$data[!d$data$orgunit %in% unique(clusterMap$cluster_psnuuid),])
    
        # ds <- DataPack %>%
        # filter(orgunit %in% unique(clusterMap$cluster_psnuuid)
        #        & !orgunit %in% militaryUnits$orgUnit) %>%
        # mutate(whereWhoWhatHuh=paste(orgunit,attributeoptioncombo,dataelement,categoryoptioncombo,sep=".")) %>%
        # left_join(Pcts,by=c("whereWhoWhatHuh")) %>%
        # #Where there is no history at PSNU level, simply distribute evenly among all underlying PSNUs
        #     left_join(clusterAvgs,by=c("orgunit"="cluster_psnuuid")) %>%
        #     mutate(Value=case_when(is.na(psnuPct)~Value*avg
        #                           ,TRUE~Value*psnuPct)) %>%
        # mutate(orgunit=PSNUuid) %>%
        # select(dataelement,period,orgunit,categoryoptioncombo,attributeoptioncombo,Value) %>%
        # rbind(DataPack[!DataPack$orgunit %in% unique(clusterMap$cluster_psnuuid),],DataPack[orgunit %in% militaryUnits$orgUnit,])
        
    
    return(list(wb_info=d$wb_info,
                follow_on_mechs=d$follow_on_mechs,
                data=ds))
}


#' @export
#' @title distributeSite(d)
#'
#' @description Distributes Data Pack data (both Clustered and non-Clustered) to Site level for DATIM import
#' @param d Data object returned from ImportSheets function. 
#' @return Returns an object structured for allocation to the site level tool. 
#'

distributeSite <- function(d) {
  
  supportType<-NULL
  pd_2019_S<-NULL
  pd_2019_P<-NULL
  DataPackCode<-NULL
  dataelement<-NULL
  categoryoptioncombo<-NULL
  orgunit<-NULL
  value<-NULL
  period<-NULL
  sitePct<-NULL
  orgUnit<-NULL
  wb_info<-NULL
  ou_name<-NULL
  psnu_name<-NULL
  organisationunituid<-NULL
  name<-NULL
  
  #Not sure what to do with this. I think we should exlcude them. 
  militaryUnits<-datapackimporter::militaryUnits
  #Default distribution is 2018 if not otherwise specified
  if (d$wb_info$distribution_method == 2017) {
    file_name = "distrSiteFY17.rda"
  } else if (d$wb_info$distribution_method ==  2018) {
    file_name = "distrSiteFY18.rda"
  } else
  {
    stop("Distribution year must either 2017 or 2018! ")
  }
  
  file_path = paste0(d$wb_info$support_files_path, file_name)
  
  if (!file.exists(file_path)) {
    stop(paste("Distribution file could not be found. Please check it exists at",file_path))
  }
  
  Pcts<-readRDS( file = file_path )
  de_map<-datapackimporter::rCOP18deMapT %>%
    dplyr::select(supportType,pd_2019_S,pd_2019_P,DataPackCode) %>%
    na.omit() %>%
    dplyr::distinct()
  
    ds <- d$data %>%
        #Create id to link to percent distributions
        dplyr::mutate(whereWhoWhatHuh=paste(orgunit,attributeoptioncombo,dataelement,categoryoptioncombo,sep=".")) %>%
        #Pull in distribution percentages, keeping all data
        dplyr::left_join(Pcts,by=c("whereWhoWhatHuh")) %>%
       #Do we need to round or what here?
        dplyr::mutate(value= trunc( ( as.numeric(value) * sitePct  ) + 0.5 )) %>%
      #Don't we have to remap back to the Site level data elements from the PSNU data elements?
        dplyr::select(dataelement,period,orgunit=orgUnit,categoryoptioncombo,attributeoptioncombo,value) %>%
        dplyr::mutate(pd_2019_P=paste0(`dataelement`,".",`categoryoptioncombo`)) %>%
        dplyr::left_join(de_map,by=c("pd_2019_P")) %>%
        dplyr::select(orgunit,attributeoptioncombo,supportType,DataPackCode,value) 
        # dplyr::left_join(mechs,by=c("attributeoptioncombo")) %>%
        # dplyr::left_join(ous_with_psnus,by=c("orgunit"="organisationunituid")) %>%
        # dplyr::mutate(site = paste(psnu_name,">",name,"(",orgunit,")")) %>%
        # dplyr::select(site,mechanism,type=supportType,dp_code=DataPackCode,value)
    
    
    file_path = paste0(d$wb_info$support_files_path, "mechanisms_by_ou.csv")
    
    mechanisms<-utils::read.csv(file_path,stringsAsFactors = FALSE) %>% 
      dplyr::select(mechanism,attributeoptioncombo=uid,ou) %>%
      dplyr::filter( ou == d$wb_info$ou_name) %>% 
      dplyr::filter( attributeoptioncombo %in% unique(ds$attributeoptioncombo)) %>%
      dplyr::arrange(mechanism)
    
    sites<-readRDS(paste0(d$wb_info$support_files_path,"ous_with_psnus.rds")) %>%
      dplyr::filter(ou_name == d$wb_info$ou_name) %>%
      dplyr::filter(!(psnu_name =="" | is.na(psnu_name))) %>%
      dplyr::select(organisationunituid,name,psnu_name)
    
    return(list(wb_info=d$wb_info,
                mechanisms=mechanisms,
                sites=sites,
                data=ds))
}
