#' @export
#' @title round_trunc(x)
#'
#' @description Rounds values by the rule of half away from zero -- consistent for both positive and negative numbers.
#' @param x Value to be rounded
#' @return Returns a vector of integers with length=length(x)
#'

round_trunc<- function(x){trunc(abs(x)+0.5)*sign(x)}



#' @export
#' @title distributeCluster(data)
#'
#' @description Distributes clustered Data Pack data to PSNU level for DATIM import
#' @param d Data object returned from ImportSheets function. 
#' @return Returns an object structured for allocation to the import into the PNSU dataset.
#'

distributeCluster <- function(d) {
  
  if(d$wb_info$is_clustered) {
      
      #Note: All of these null assignments are for 
      #package checks, which provide warnings if these
      #impiled variables used in dplyr are not initialized.
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
      
      file_path = paste0(distros_path
                         ,ifelse(stringr::str_detect(distros_path,"\\/$"),"","/")
                         , file_name)
      
      if (!file.exists(file_path)) {
        stop(paste("Distribution file could not be found. Please check it exists at",file_path))
      }
      
      Pcts<-readRDS( file = file_path )
      clusterMap<-datapackimporter::clusters
      militaryUnits<-datapackimporter::militaryUnits
      ou_uid<-d$wb_info$ou_uid
      
      #Prepare Cluster Averages
      clusterAvgs <- clusterMap %>%
        dplyr::select(cluster_psnuuid,psnuuid) %>%
        dplyr::group_by(cluster_psnuuid) %>%
        dplyr::mutate(num=1,
                      den=n()) %>%
        dplyr::mutate(avg=num/den) %>%
        dplyr::select(-num,-den)
        
        #At this point, data may still contain both clustered and nonclustered data within a
        # "Clustered" OU, and likely will contain some _Military data
        
        d$data <- d$data %>%
            #Pull _Military units out separately. Will bind back in at end. These need no manipulation
                dplyr::filter(!orgunit %in% militaryUnits)
            #Create join key
                dplyr::mutate(whereWhoWhatHuh=paste(orgunit,attributeoptioncombo,dataelement,categoryoptioncombo,sep=".")) %>%
            #Join with Percentage distribution file (For non-clustered units, will pull in a series of 100%'s)
                dplyr::left_join(Pcts[Pcts$uidlevel3==ou_uid,],by=c("whereWhoWhatHuh")) %>%
            #Where there is no history at PSNU level, simply distribute evenly among all underlying PSNUs
                dplyr::left_join(clusterAvgs,by=c("orgunit"="cluster_psnuuid")) %>%
                dplyr::mutate(value=dplyr::case_when(is.na(psnuPct)~value*avg,TRUE~value*psnuPct)) %>%
                dplyr::mutate(orgunit=PSNUuid) %>%
            #Round to integer values per MER requirements
                dplyr::mutate(value = round(value)) %>%
            #Remove zero value targets
                dplyr::filter(value != "0")  %>%
            #Bind _Military units back in
                    #@sjackson - make sure column orders/types conform
                dplyr::bind_rows(d$data[!d$data$orgunit %in% militaryUnits,])
  } 
  return(d)
}


#' @export
#' @title distributeSite(d)
#'
#' @description Distributes Data Pack data (both Clustered and non-Clustered) to Site level for DATIM import
#' @param d Data object returned from ImportSheets function. 
#' @return Returns an object structured for allocation to the site level tool. 
#'

distributeSite <- function(d) {
  
  #Note: All of these null assignments are for 
  #package checks, which provide warnings if these
  #impiled variables used in dplyr are not initialized.
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
  mil_data<-d$data %>% 
    dplyr::filter(orgunit %in% datapackimporter::militaryUnits)
  
  #Default distribution is 2018 if not otherwise specified
      if (d$wb_info$distribution_method == 2017) {
        file_name = "distrSiteFY17.rda"
      } else if (d$wb_info$distribution_method ==  2018) {
        file_name = "distrSiteFY18.rda"
      } else
      {
        stop("Distribution year must either 2017 or 2018! ")
      }
  
  file_path = paste0(d$wb_info$support_files_path
                     ,ifelse(stringr::str_detect(d$wb_info$support_files_path,"\\/$"),"","/")
                     , file_name)
  
  if (!file.exists(file_path)) {
    stop(paste("Distribution file could not be found. Please check it exists at",file_path))
  }
  
  Pcts<-readRDS( file = file_path ) %>%
      dplyr::filter(uidlevel3==d$wb_info$ou_uid)
      
  de_map<-datapackimporter::rCOP18deMapT %>%
    dplyr::select(supportType,pd_2019_S,pd_2019_P,DataPackCode) %>%
    na.omit() %>%
    dplyr::distinct()
  
    ds <- d$data %>%
        #Create id to link to percent distributions
        dplyr::mutate(whereWhoWhatHuh=paste(orgunit,attributeoptioncombo,dataelement,categoryoptioncombo,sep=".")) %>%
        #Pull in distribution percentages, keeping all data
        dplyr::left_join(Pcts,by=c("whereWhoWhatHuh")) %>%
        dplyr::mutate(value = dplyr::case_when(!is.na(sitePct)~round_trunc(as.numeric(value) * sitePct)
                                               #Where no past behavior (sitePct is NA), keep values at PSNU/Cluster level
                                               #for manual distribution in Site tool
                                               ,TRUE~round_trunc(as.numeric(value)))) %>%
      #Reattach the military data after distribution
        dplyr::bind_rows(mil_data) %>%
        dplyr::mutate(orgunit=dplyr::case_when(!is.na(orgUnit)~orgUnit,TRUE~orgunit)) %>%
        dplyr::select(dataelement,period,orgunit,categoryoptioncombo,attributeoptioncombo,value) %>%
        dplyr::mutate(pd_2019_P=paste0(`dataelement`,".",`categoryoptioncombo`)) %>%
        dplyr::left_join(de_map,by=c("pd_2019_P")) %>%
        dplyr::select(orgunit,attributeoptioncombo,supportType,DataPackCode,value) 
        # dplyr::left_join(mechs,by=c("attributeoptioncombo")) %>%
        # dplyr::left_join(ous_with_psnus,by=c("orgunit"="organisationunituid")) %>%
        # dplyr::mutate(site = paste(psnu_name,">",name,"(",orgunit,")")) %>%
        # dplyr::select(site,mechanism,type=supportType,dp_code=DataPackCode,value)
    
    
    mechanisms<-readRDS(paste0(d$wb_info$support_files_path
                               ,ifelse(stringr::str_detect(d$wb_info$support_files_path,"\\/$"),"","/")
                               , "mech_list.rda")) %>% 
      dplyr::select(mechanism,attributeoptioncombo=uid,ou) %>%
      dplyr::filter( ou == d$wb_info$ou_name) %>% 
        #Only allow data entry in Site level tool against Mechanisms already seen in Disagg Tool
      dplyr::filter( attributeoptioncombo %in% unique(ds$attributeoptioncombo)) %>%
      dplyr::arrange(mechanism)
    
    sites<-readRDS(paste0(d$wb_info$support_files_path
                          ,ifelse(stringr::str_detect(d$wb_info$support_files_path,"\\/$"),"","/")
                          ,"ous_list.rda")) %>%
      dplyr::filter(ou_uid == d$wb_info$ou_uid) %>%
      dplyr::select(organisationunituid=DataPackSiteUID,name=DataPackSiteID,siteType)
    
    schemas<-
      if ( d$wb_info$wb_type == "NORMAL") {
        schemas <- datapackimporter::main_site_schema
      } else if ( d$wb_info$wb_type == "HTS") {
        schemas <- datapackimporter::hts_site_schema
      } else{
        stop("Unknown PSNU worbook type!")
      }
    #Alter the workbook info
    d$wb_info$wb_type<-ifelse(d$wb_info$wb_type=="NORMAL","NORMAL_SITE","HTS_SITE")
    return(list(wb_info=d$wb_info,
                mechanisms=mechanisms,
                schemas=schemas,
                sites=sites,
                sums=d$sums,
                data=ds))
}
