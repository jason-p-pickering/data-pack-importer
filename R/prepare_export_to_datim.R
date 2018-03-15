#' @export
#' @title prepare_export_to_datim(d)
#'
#' @description Prepares a PSNU or Site level object for import to DATIM.
#' @param d Data object from ImportSheets or parse_site_level_tool
#'

prepare_export_to_datim <- function(d) {
  if (d$wb_info$wb_type %in% c("HTS", "NORMAL")) {
    file_prefix <- "/psnu_import_"
    d <- distributeCluster(d)
  } else if (d$wb_info$wb_type %in% c("HTS_SITE", "NORMAL_SITE")) {
    file_prefix <- "/site_import_"
  }
  
  d$data <- d$data %>%
    dplyr::mutate(value = as.character(round_trunc(as.numeric(value)))) %>%
    dplyr::filter(!(value < 1))
  
  #Compute HTS_TST Numerator values from HTS Tools
  if (d$wb_info$wb_type %in% c("HTS_SITE","HTS")) {
    
    hts_codes <- datapackimporter::rCOP18deMapT %>%
      dplyr::filter(indicator=="HTS_TST" & !is.na(DataPackCode) & !is.na(pd_2019_P) & Modality !="") %>%
      tidyr::separate(pd_2019_S,into=c("pd_2019_S_de","pd_2019_S_coc"),sep="\\.",remove=T) %>%
      tidyr::separate(pd_2019_P,into=c("pd_2019_P_de","pd_2019_P_coc"),sep="\\.",remove=T) %>%
      dplyr::select(pd_2019_S_de, pd_2019_P_de, supportType) %>%
      dplyr::distinct()
    
    d_hts <- d$data %>%
      dplyr::filter(dataelement %in% dplyr::case_when(d$wb_info$wb_type=="HTS_SITE" ~ hts_codes$pd_2019_S_de,
                                                      d$wb_info$wb_type=="HTS" ~ hts_codes$pd_2019_P_de)) %>%
      dplyr::left_join(hts_codes,by=c("dataelement" =
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
