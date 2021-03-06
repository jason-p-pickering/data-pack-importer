#' @export
#' @title round_trunc(x)
#'
#' @description Rounds values by the rule of half away from zero -- consistent for both positive and negative numbers.
#' @param x Value to be rounded
#' @return Returns a vector of integers with length=length(x)
#'

round_trunc <- function(x) {
  trunc(abs(x) + 0.5) * sign(x)
}


get_percentage_distribution <- function(d,type) {
  # Default distribution is 2018 if not otherwise specified
  if (d$wb_info$distribution_method == 2017 & type=="site") {
    file_name <- "distrSiteFY17.rda"
  } else if (d$wb_info$distribution_method == 2018 & type=="site") {
    file_name <- "distrSiteFY18.rda"
  } else if (d$wb_info$distribution_method == 2017 & type=="cluster") {
    file_name <- "distrClusterFY17.rda"
  } else if (d$wb_info$distribution_method == 2018 & type=="cluster") {
    file_name <- "distrClusterFY18.rda"
  }
  else {
    stop("Distribution year must either 2017 or 2018! ")
  }
  
  file_path <- paste0(d$wb_info$support_files_path, file_name)
  
  if (!file.exists(file_path)) {
    stop(paste("Distribution file could not be found. Please check it exists at", file_path))
  }
  
  if (!is.null(d$follow_on_mechs)) {
    
    followOns <- d$follow_on_mechs %>%
      dplyr::left_join(mechs, by = c("closingCode" = "code")) %>%
      dplyr::select(closingCode, closingUID = uid, followOnCode) %>%
      dplyr::left_join(mechs, by = c("followOnCode" = "code")) %>%
      dplyr::select(closingCode, closingUID, followOnCode, followOnUID = uid)
    
    Pcts <- readRDS(file = file_path) %>%
      dplyr::filter(uidlevel3 == d$wb_info$ou_uid) %>%
      # Map follow-on mechs
      dplyr::mutate(
        attributeoptioncombo = stringr::str_extract(whereWhoWhatHuh, "(?<=(^\\w{11}\\.))\\w{11}")
      ) %>%
      dplyr::left_join(
        dplyr::select(followOns, closingUID, followOnUID),
        by = c("attributeoptioncombo" = "closingUID")
      ) %>%
      dplyr::mutate(
        whereWhoWhatHuh = dplyr::case_when(
          !is.na(followOnUID) ~ stringr::str_replace(whereWhoWhatHuh, attributeoptioncombo, followOnUID),
          TRUE ~ whereWhoWhatHuh
      )) %>% 
      dplyr::select(-attributeoptioncombo, -followOnUID)
    
  } else {
    Pcts <- readRDS(file = file_path) %>%
      dplyr::filter(uidlevel3 == d$wb_info$ou_uid)
  }
  
  return(Pcts)
  
}

#' @export
#' @importFrom dplyr n
#' @title distributeCluster(data)
#'
#' @description Distributes clustered Data Pack data to PSNU level for DATIM import
#' @param d Data object returned from ImportSheets function.
#' @param Pcts Percentage object for this data set. Setting this allows one to override the default 
#' percentage allocation with a custom one. 
#' @return Returns an object structured for allocation to the import into the PNSU dataset.
#'

distributeCluster <- function(d,Pcts=NULL) {
  if (d$wb_info$is_clustered) {
    
    if (is.null(Pcts)) {
      Pcts <- get_percentage_distribution(d, "cluster")
    }
    
    cluster_map <- datapackimporter::clusters %>%
      dplyr::filter(operatingUnitUID == d$wb_info$ou_uid) %>%
      dplyr::select(cluster_psnuuid, psnuuid) %>%
      dplyr::group_by(cluster_psnuuid) %>%
      dplyr::mutate(avg = 1 / n())
    
    military_units <- datapackimporter::militaryUnits
    
    # Create join key
    d_all <- d$data %>%
      dplyr::mutate(
        whereWhoWhatHuh = paste(
          orgunit,
          attributeoptioncombo,
          dataelement,
          categoryoptioncombo,
          sep = "."
        )
      )
    
    #We leave military and data already at the PSNU level alone
    d_mil_psnu <- d_all %>%
      dplyr::filter(orgunit %in% military_units |
                      !(orgunit %in% unique(cluster_map$cluster_psnuuid))) %>%
      dplyr::select(dataelement,
                    period,
                    orgunit,
                    categoryoptioncombo,
                    attributeoptioncombo,
                    value)
    #Filter cluster data
    d_clust <- d_all %>%
      dplyr::filter(!(orgunit %in% military_units)) %>%
      dplyr::filter(orgunit %in% unique(cluster_map$cluster_psnuuid))
    #Check to be sure we have either Military, clusters or PSNU data
    if (NROW(d_mil_psnu) + NROW(d_clust) != NROW(d_all)) {
      warning("Mil,PSNU and cluster data are not congruent with parsed data")
    }
    #Join in the percentages
    d_clust <-
      d_clust %>% dplyr::left_join(Pcts, by = c("whereWhoWhatHuh"))
    #Clusters to PSNU with history
    d_clust_hist <- d_clust %>%
      dplyr::filter(!is.na(psnuPct)) %>%
      dplyr::mutate(value = as.character(as.numeric(value) * psnuPct)) %>%
      dplyr::select(dataelement,
                    period,
                    orgunit = PSNUuid,
                    categoryoptioncombo,
                    attributeoptioncombo,
                    value)
    
    #Clusters to PSNU with no history
    d_clust_nohist <- d_clust %>%
      dplyr::filter(is.na(psnuPct)) %>%
      dplyr::select(-uidlevel3, -PSNUuid, -psnuPct) %>%
      dplyr::rename(cluster_psnuuid = orgunit) %>%
      dplyr::inner_join(cluster_map, by = "cluster_psnuuid") %>%
      dplyr::mutate(value = as.character(as.numeric(value) * avg)) %>%
      dplyr::select(dataelement,
                    period,
                    orgunit = psnuuid,
                    categoryoptioncombo,
                    attributeoptioncombo,
                    value)
    
    #Bind everything back together
    d_all_new <-
      dplyr::bind_rows(d_mil_psnu, d_clust_hist, d_clust_nohist)
    
    #Check and be sure we are within 5% of the original
    d_all_new_sum <- d_all_new  %>%
      dplyr::group_by(dataelement,
                      period,
                      categoryoptioncombo,
                      attributeoptioncombo) %>%
      dplyr::summarise(value = sum(as.numeric(value)))
    d_sum <- d$data %>%
      dplyr::group_by(dataelement,
                      period,
                      categoryoptioncombo,
                      attributeoptioncombo) %>%
      dplyr::summarise(value_start = sum(as.numeric(value))) %>%
      dplyr::full_join(
        d_all_new_sum,
        by = c(
          "dataelement",
          "period",
          "categoryoptioncombo",
          "attributeoptioncombo"
        )
      ) %>%
      dplyr::mutate(diff = (value_start - value) / value_start * 100) %>%
      #Difference greater than 5%
      dplyr::filter(diff > 5)
    if (NROW(d_sum) > 0) {
      warning("Cluster to PSNU allocation did not go well.")
      print(d_sum)
    }
    
    # Round to integer values and filter zeros per MER requirements
    d$data <- d_all_new %>%
      dplyr::mutate(value = as.character(round_trunc(as.numeric(value)))) %>%
      dplyr::filter(value != "0") %>%
      dplyr::select(dataelement,
                    period,
                    orgunit,
                    categoryoptioncombo,
                    attributeoptioncombo,
                    value)
    
    #There must be no duplicates at this point
    if (sum(duplicated(d$data[, 1:5])) > 0) {
      stop("Duplicates detected in cluster to PSNU export file!")
    }
  }
  return(d)
}


#' @export
#' @title distributeSite(d)
#'
#' @description Distributes Data Pack data (both Clustered and non-Clustered) to Site level for DATIM import
#' @param d Data object returned from ImportSheets function.
#' @param Pcts Percentage object used to distribute from PSNU to site. Setting this allows for the default
#' distribution method to be overridden. 
#' @return Returns an object structured for allocation to the site level tool.
#'

distributeSite <- function(d, Pcts=NULL ) {

  if (is.null(Pcts)) {
    Pcts <- get_percentage_distribution(d, "site")
  }
  
  de_map <- datapackimporter::rCOP18deMapT %>%
    dplyr::select(supportType, pd_2019_S, pd_2019_P, DataPackCode) %>%
    na.omit() %>%
    dplyr::distinct()

  ds <- d$data %>%
    # Create id to link to percent distributions
    dplyr::mutate(whereWhoWhatHuh = paste(orgunit, attributeoptioncombo, dataelement, categoryoptioncombo, sep = ".")) %>%
    # Remove IMPATT data
    dplyr::filter(!dataelement %in% c("rORzrY9rpQ1", "r4zbW3owX9n")) %>%
    # Pull in distribution percentages, keeping all data
    dplyr::left_join(Pcts, by = c("whereWhoWhatHuh")) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        !is.na(sitePct)~round_trunc(as.numeric(value) * sitePct)
        # Where no past behavior (sitePct is NA), keep values at PSNU/Cluster level
        # for manual distribution in Site tool
        , TRUE~round_trunc(as.numeric(value))
      ),
      # Denote where data was not distributed with a 0
      distributed = dplyr::case_when(is.na(sitePct)~0, TRUE~1)
    ) %>%
    dplyr::mutate(orgunit = dplyr::case_when(!is.na(orgUnit)~orgUnit, TRUE~orgunit)) %>%
    dplyr::select(distributed, dataelement, period, orgunit, categoryoptioncombo, attributeoptioncombo, value) %>%
    dplyr::mutate(pd_2019_P = paste0(`dataelement`, ".", `categoryoptioncombo`)) %>%
    dplyr::left_join(de_map, by = c("pd_2019_P")) %>%
    dplyr::select(distributed, orgunit, attributeoptioncombo, supportType, DataPackCode, value)
  # dplyr::left_join(mechs,by=c("attributeoptioncombo")) %>%
  # dplyr::left_join(ous_with_psnus,by=c("orgunit"="organisationunituid")) %>%
  # dplyr::mutate(site = paste(psnu_name,">",name,"(",orgunit,")")) %>%
  # dplyr::select(site,mechanism,type=supportType,dp_code=DataPackCode,value)


  mechanisms <- datapackimporter::mechs %>%
    dplyr::select(mechanism, attributeoptioncombo = uid, ou) %>%
    dplyr::filter(ou == d$wb_info$ou_name) %>%
    # Only allow data entry in Site level tool against Mechanisms already seen in Disagg Tool
    dplyr::filter(attributeoptioncombo %in% unique(ds$attributeoptioncombo)) %>%
    dplyr::arrange(mechanism)

  sites <- readRDS(paste0(d$wb_info$support_files_path, "ous_list.rda")) %>%
    dplyr::filter(ou_uid == d$wb_info$ou_uid) %>%
    dplyr::select(organisationunituid = DataPackSiteUID, name = DataPackSiteID, siteType, distributed)

  schemas <-
    if (d$wb_info$wb_type == "NORMAL") {
      schemas <- datapackimporter::main_site_schema
    } else if (d$wb_info$wb_type == "HTS") {
      schemas <- datapackimporter::hts_site_schema
    } else {
      stop("Unknown PSNU worbook type!")
    }
  # Alter the workbook info
  d$wb_info$wb_type <- ifelse(d$wb_info$wb_type == "NORMAL", "NORMAL_SITE", "HTS_SITE")
  return(list(
    wb_info = d$wb_info,
    mechanisms = mechanisms,
    schemas = schemas,
    sites = sites,
    sums = d$sums,
    data = ds
  ))
}