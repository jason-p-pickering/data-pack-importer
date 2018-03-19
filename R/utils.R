get_distribution_method <- function(distribution_method=NA) {
  distribution_methods <- c(2017, 2018)
  if (is.na(distribution_method) | !any(distribution_method %in% distribution_methods)) {
    # Distribution method
    promptText <- paste0("Please enter the distribution method (2017 or 2018):")
    print(promptText)
    distribution_method <- utils::select.list(distribution_methods, multiple = FALSE)
  }
  
  return(distribution_method)
}

#' @title check_mechs_by_code(d,sheet_name,mechanisms)
#'
#' @param d Parsed data from a single import sheet
#' @param wb_info Workbook info
#' @param sheet_name Name of the sheet
#'
#' @return TRUE if no invalid mechanisms were found, otherwise issues a warning and returns FALSE.
#'
check_mechs_by_code <- function(d, wb_info, sheet_name ) {
  
  mechs_wanted <- readRDS(paste0(wb_info$support_files_path, "mech_list.rda")) %>%
    dplyr::filter(ou == wb_info$ou_name) %>%
    dplyr::pull(code)
  
  invalid_mechs <- unique(d$mech_code)[!(unique(d$mech_code) %in% mechs_wanted)]
  
  if (length(invalid_mechs) > 0) {
    msg <- paste0(
      "The following mechanisms in sheet ", sheet_name, " were invalid:",
      paste(invalid_mechs, sep = "", collapse = ";")
    )
    warning(msg)
    return(FALSE)
  }
  return(TRUE)
}

check_negative_numbers <- function(d, schema) {
  
  has_negative_numbers <- as.numeric(d$value) < 0
  
  if (any(has_negative_numbers)) {
   msg<-paste0("Negative values were found in sheet ", schema$sheet_name )
   warning(msg)
  } else {
    return(NULL)
  }
}


#' @title empty_datim_tibble
#'
#' @return An empty tibble with DE, perod, orgunit, category option combo, attribute option combo, value
#'
empty_dhis_tibble<-function()  { 
  
  tibble::tibble(
    "dataelement" = character(),
    "period" = character(),
    "orgunit" = character(),
    "categoryoptioncombo" = character(),
    "attributeoptioncombo" = character(),
    "value" = character()
  ) 
}


divide_evenly<-function(x,n) {
  if (n == 0 ) {
    stop("N must be greater than 0")
  }
  if (n == 1 ) {
    return(x)  }
  a<-rep(x %/% n,n)
  b <- 1:n <= x %% n
  sample(a + b,n)
}