write_datim_export_file<-function(d) {
if (d$wb_info$wb_type %in% c("HTS", "NORMAL")) {
  file_prefix <- "/psnu_import_"
  d <- distributeCluster(d)
} else if (d$wb_info$wb_type %in% c("HTS_SITE", "NORMAL_SITE")) {
  file_prefix <- "/site_import_"
}

export_data <- d$data %>%
  dplyr::select(
    dataelement,
    period,
    orgunit,
    categoryoptioncombo,
    attributeoptioncombo,
    value
  ) %>%
  na.omit()

output_file_path <- paste0(
  dirname(d$wb_info$wb_path),
  file_prefix,
  d$wb_info$wb_type,
  "_",
  d$wb_info$ou_name,
  "_",
  format(Sys.time(), "%Y%m%d%H%M%S"),
  ".csv"
)

utils::write.table(
  export_data,
  file = output_file_path,
  quote = TRUE,
  sep = ",",
  row.names = FALSE,
  col.names = TRUE
)
print(paste0("Successfully saved output to ", output_file_path))

}

#' @export
#' @title prepare_export_to_datim(d)
#'
#' @description Prepares a PSNU or Site level object for import to DATIM.
#' @param d Data object from ImportSheets or parse_site_level_tool
#' @return Object with aggregatedHTS data, where applicable. In cases of clustered data, the data will be distributed to PSNU level.
#'

prepare_export_to_datim <- function(d) {
  
  if (d$wb_info$wb_type %in% c("HTS", "NORMAL") & d$wb_info$is_clustered  ) {
    d <- distributeCluster(d)
  } 
  
  d$data <- d$data %>%
    dplyr::mutate(value = as.character(round_trunc(as.numeric(value)))) %>%
    dplyr::filter(!(value < 1))
  
  #We need to be sure we ONLY have HTS data here. 
  
  hts_codes <- datapackimporter::rCOP18deMapT %>%
    dplyr::filter( indicator == "HTS_TST" & !is.na(DataPackCode) & !is.na(pd_2019_P)) %>%
    tidyr::separate(pd_2019_S,into=c("pd_2019_S_de","pd_2019_S_coc"),sep="\\.",remove=T) %>%
    tidyr::separate(pd_2019_P,into=c("pd_2019_P_de","pd_2019_P_coc"),sep="\\.",remove=T) %>%
    dplyr::select(pd_2019_S_de, pd_2019_P_de, supportType, Modality) %>%
    dplyr::distinct()
  
    hts_des <- c(hts_codes[, "pd_2019_S_de"], hts_codes[, "pd_2019_P_de"])
  #Only HTS data should be here
  d$data <- d$data %>% dplyr::filter(dataelement %in% hts_des)

  #Compute HTS_TST Numerator values from HTS Modalities
  if (d$wb_info$wb_type %in% c("HTS_SITE","HTS")) {
    
    hts_numerator_codes <- hts_codes %>% 
      dplyr::filter( Modality !="") 
    
    d_hts <- d$data %>%
      dplyr::filter(dataelement %in% dplyr::case_when(d$wb_info$wb_type=="HTS_SITE" ~ hts_numerator_codes $pd_2019_S_de,
                                                      d$wb_info$wb_type=="HTS" ~ hts_numerator_codes$pd_2019_P_de)) %>%
      dplyr::inner_join(hts_codes,by=c("dataelement" =
                                        dplyr::case_when(d$wb_info$wb_type=="HTS_SITE" ~ "pd_2019_S_de",
                                                         d$wb_info$wb_type=="HTS" ~ "pd_2019_P_de"))) %>%
      dplyr::mutate(value = as.numeric(value),
                    categoryoptioncombo = "HllvX50cXC0",
                    dataelement = dplyr::case_when(supportType == "DSD" & d$wb_info$wb_type == "HTS_SITE" ~ "NUdcIMK4Peq",
                                                   supportType == "DSD" & d$wb_info$wb_type == "HTS" ~ "bCdPl0retrn",
                                                   supportType == "TA" & d$wb_info$wb_type == "HTS_SITE" ~ "zmAsEta7AiO",
                                                   supportType == "TA" & d$wb_info$wb_type == "HTS" ~ "D131hA9xpEx")) %>%
      dplyr::select(-supportType) %>%
      dplyr::group_by(dataelement,period,orgunit,categoryoptioncombo,attributeoptioncombo) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_all(as.character)
    
    d$data <- dplyr::bind_rows(d$data,d_hts)
  }
  
  #Check any duplicates
  if ( any(duplicated(d$data[,1:5])) ) {
    stop("Duplicates detected in DATIM export object")
  }
  write_datim_export_file(d)
  return(d)
}
