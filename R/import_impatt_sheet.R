
#' @title ValidateImpattSheet(d,wb_info)
#' @description Validates the impatt sheet for completeness.
#' @param d A parsed data frame with IMPATT data
#' @param wb_info Workbook info for the workbook
#' @return NULL
ValidateImpattSheet <- function(d, wb_info) {
  psnus <- datapackimporter::psnus[[wb_info$ou_uid]]
  psnus_missing <- !(psnus$id %in% d$psnuuid)
  if (any(psnus_missing)) {
    msg <-
      paste(
        "The following PNSUs were missing from the IMPATT table:",
        paste(psnus$name[psnus_missing], sep = "", collapse = ",")
      )
    warning(msg)
  }
}


#' @title import_impatt_sheet(wb_info,schema)
#'
#' @param wb_info Workbook info object
#' @param schema Schema for a single sheet
#'
#' @return A DHIS2 compatible import data frame
#'
#'
import_impatt_sheet<-function(wb_info,schema) {
  
  cell_range <- readxl::cell_limits(
    c(schema$row, schema$start_col),
    c(NA, schema$end_col)
  )
  from <- c("snu_priotization_fy19", "plhiv_fy19")
  # IMPATT.PRIORITY_SNU (SUBNAT), IMPATT.PLHIV (SUBNAT, Age/Sex)
  to <- c("r4zbW3owX9n", "rORzrY9rpQ1")
  # https://www.datim.org/api/optionSets/mvbwbgbJgXr.json?fields=options[code,name]
  d <- readxl::read_excel(wb_info$wb_path, sheet = schema$sheet_name, range = cell_range, col_types = "text")
  
  if (NROW(d) ==  0) {
    warning("Nothing found in the IMPATT sheet!")
    return( empty_dhis_tibble() )
  }
  
 ValidateImpattSheet(d, wb_info)

  #Duplicate check
  dups<- d %>%
    dplyr::group_by(psnu) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::filter(n>1)%>%
    dplyr::pull(psnu)
  
  if (length(dups) > 0) {
    warning("Duplicate PSNUs were found in the IMPATT sheet! ",paste(dups,sep="",collapse=","))
  }
  
  d<- d %>%
    dplyr::filter(snu_priotization_fy19 != "Mil") %>%
    dplyr::mutate(
      .,
      snu_priotization_fy19 = plyr::mapvalues(
        snu_priotization_fy19,
        datapackimporter::impatt$options$dp_code,
        datapackimporter::impatt$options$code,
        warn_missing = FALSE
      )
    ) %>%
    tidyr::gather(variable, value, -c(1:2)) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::mutate(
      ., dataelement = plyr::mapvalues(variable, from, to, warn_missing = FALSE),
      orgunit = psnuuid,
      period = "2018Oct",
      attributeoptioncombo = "HllvX50cXC0",
      categoryoptioncombo = "HllvX50cXC0",
      value = as.character(value)
    ) %>%
    dplyr::select(., dataelement, period, orgunit, categoryoptioncombo, attributeoptioncombo, value)
  
  return(d)
}