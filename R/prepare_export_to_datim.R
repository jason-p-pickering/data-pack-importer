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

  export_data <- d$data %>%
    dplyr::mutate(value = as.character(round_trunc(as.numeric(value)))) %>%
    dplyr::filter(!(value < 1)) %>%
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