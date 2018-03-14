#' empty_datim_tibble
#'
#' @description 
#' @return An empty tibble with DE, perod, orgunit, category option combo, attribute option combo, value
#'
#' @examples
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
