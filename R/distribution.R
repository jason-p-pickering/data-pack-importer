#' @export
#' @title distributeCluster()
#'
#' @description Distributes clustered Data Pack data to PSNU level for DATIM import
#' @param df Name of dataframe storing Data Pack data.
#' @param distribution_year Distribution Method selection, whether based on FY17. Otherwise, FY18.
#' @return Returns a dataframe structured for DATIM import.
#'

distributeCluster <- function(df,distribution_year) {
    
  distros_path=getOption("datapack_distros")
    if (is.null(distros_path) |
        is.na(distros_path) |
        !file.exists(distros_path)) {
      stop("Could not access path to the distributions.")
    }
    #Default distribution is 2018 if not otherwise specified
    if (distribution_year == 2017) {
      file_name = "distrClusterFY17.rda"
    } else {
      file_name = "distrClusterFY18.rda"
    }
    
    file_path = paste0(getOption("datapack_distros"), file_name)
    
    if (!file.exists(file_path)) {
      stop(paste("Distribution file could not be found. Please check it exists at",file_path))
    }
    
    Pcts<-readRDS(file = file_path )
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
#' @param distribution_year Distribution Method selection, whether based on FY17 or FY18. 
#' @return Returns a dataframe structured for DATIM import.
#'

distributeSite <- function(df,distribution_year) {
    
  
  #Default distribution is 2018 if not otherwise specified
  if (distribution_year == 2017) {
    file_name = "distrSiteFY17.rda"
  } else if (distribution_year ==  2018) {
    file_name = "distrSiteFY18.rda"
  } else
  {
    stop("Distribution year must either 2017 or 2018! ")
  }
  
  file_path = paste0(getOption("datapack_distros"), file_name)
  
  if (!file.exists(file_path)) {
    stop(paste("Distribution file could not be found. Please check it exists at",file_path))
  }
  
  Pcts<-readRDS(file = file_path )

    ds <- df %>%
        #Create id to link to percent distributions
            mutate(whereWhoWhatHuh=paste(orgunit,attributeoptioncombo,dataelement,categoryoptioncombo,sep=".")) %>%
        #Pull in distribution percentages
        #@Sjackson: What is the point of a left join here? Anything which is NA will have to be dropped
        #And cannot be imported. 
            left_join(Pcts,by=c("whereWhoWhatHuh")) %>%
       #Do we need to round or what here?
            mutate(value=as.character(floor(as.numeric(value)*sitePct))) %>%
            filter(value != "0") %>% 
      #Don't we have to remap back to the Site level data elements from the PSNU data elements?
        select(dataelement,period,orgunit=orgUnit,categoryoptioncombo,attributeoptioncombo,value)
    return(ds)
}


