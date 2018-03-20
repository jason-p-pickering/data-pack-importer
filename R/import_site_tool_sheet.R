#' get_site_tool_duplicates(d,sheet_name)
#'
#' @param d Parsed data object from a site level tool sheet
#' @param sheet_name Name of the sheet
#'
#' @return NULL
get_site_tool_duplicates <- function(d,sheet_name) {
  
  site_dupes<-d %>% dplyr::select(Site,mech_code,DataPackCode) %>%
    dplyr::group_by(Site,mech_code,DataPackCode) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::filter(n>1) %>%
    dplyr::select(Site,mech_code) %>%
    dplyr::distinct()
  
  if (NROW(site_dupes) > 0) {
    msg <-
      paste0("Duplicate rows were found in sheet ", sheet_name,": ",
             paste(
               paste0(site_dupes$Site, ":", site_dupes$mech_code),
               sep = "",
               collapse = ";"
             ))
    warning(msg)
  }
}

#' import_site_tool
#'
#' @param wb_info Workbook info object
#' @param schema Schema for a single sheet
#'
#' @return A DHIS2 compatible import data frame
#'
#'
import_site_tool_sheet<-function(wb_info, schema) {
  
  cell_range <- readxl::cell_limits(
    c(schema$row, schema$start_col),
    c(NA, schema$end_col)
  )

  d <- readxl::read_excel(wb_info$wb_path, sheet = schema$sheet_name, range = cell_range, col_types = "text") 
  
  if (NROW(d) == 0) { return(empty_dhis_tibble()) }
  
  de_map <- datapackimporter::rCOP18deMapT %>%
    dplyr::select(supportType, pd_2019_S, pd_2019_P, DataPackCode) %>%
    na.omit() %>%
    dplyr::distinct()
  
  mechs <- datapackimporter::mechs
  
  d<-d %>%
    dplyr::select(-Inactive) %>%
    tidyr::gather(variable, value, -c(1:3, convert = FALSE)) %>%
    # Remove anyting which is not-numeric
    dplyr::filter(!is.na(suppressWarnings(as.numeric(value)))) %>%
    # Remove anything which is close to zero
    dplyr::filter(round_trunc(as.numeric(value)) != "0") %>%
    dplyr::filter(!(value == "NA")) %>%
    # Special handling for dedupe which is coerced to 0 and 1
    # Dedupe should always be dropped.
    dplyr::filter(., !(Mechanism %in% c("0", "00000", "1", "00001")))

  check_missing_field(d, schema, field = "Type")
  check_missing_field(d, schema, field="Mechanism")
  check_missing_field(d, schema, field="Site")
  
  unallocated <- dplyr::filter(d, grepl("NOT YET DISTRIBUTED", Site)) %>%
    dplyr::pull(Site) %>%
    unique() %>%
    stringr::str_replace(., " > NOT YET DISTRIBUTED", "")
  
  if (length(unallocated) > 0) {
    msg <-
      paste0(
        "There are unallocated values in sheet ",
        schema$sheet_name,
        ":",
        paste(unallocated, sep = "", collapse = ",")
      )
    warning(msg)
  }
  
  d <- d %>%
    dplyr::mutate(
      mech_code = stringi::stri_extract_first_regex(Mechanism, "^[0-9]{4,5}(?=\\s-)"),
      orgunit = stringi::stri_extract_first_regex(Site, "(?<=\\()([A-Za-z][A-Za-z0-9]{10})(?=\\)$)"),
      DataPackCode = paste0(variable, "_", tolower(Type)),
      period = "2018Oct"
    )
  
  #Data checks
  get_site_tool_duplicates(d,sheet_name = schema$sheet_name)
  check_mechs_by_code(d = d, wb_info = wb_info, sheet_name = schema$sheet_name)
  check_negative_numbers(d, schema)

  #DHIS2 form
  d <- d %>%
    dplyr::inner_join(mechs, by = c("mech_code" = "code")) %>%
    dplyr::inner_join(de_map, by = "DataPackCode") %>%
    tidyr::separate(., pd_2019_S, c("dataelement", "categoryoptioncombo")) %>%
    dplyr::select(dataelement, period, orgunit, categoryoptioncombo, attributeoptioncombo = uid, value)
  
  check_any_missing<-function(x) {
    any(!complete.cases(x))
  }
  
  if (check_any_missing(d) & NROW(d) > 0 ) {
    warning(paste("Duplicates found in sheet",schema$sheet_name))
  }
  
  return(d)
}
