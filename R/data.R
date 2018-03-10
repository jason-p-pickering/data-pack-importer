#' @docType data
#' @title hts_schema
#' @usage data(hts_schema)
#'
#' @format Returns a list which defines the schema for the HTS Disagg tool
#'
#' HTS Schemas of the Data Pack Excel sheets
"hts_schema"

#' @docType data
#' @title main_schema
#' @usage data(main_schema) 
#'
#' @return Returns a list which defines the schema for the  Disagg tool
#'
"main_schema"



#' @docType data
#' @title mechs
#' @usage data(mechs) 
#'
#' @format Returns a two column data frame with mechanisms and codes
#'
"mechs"


#' @docType data
#' @title impatt
#' @usage data(impatt)
#'
#' @format Returns a three column data frame with code,name and Data Pack code (dp_code)
#'
"impatt"


#' @docType data
#' @title support_files_md5
#' @usage data(support_files_md5)
#'
#' @format  Object used for validation of support files
#'
"support_files_md5"


#' @docType data
#' @title rCOP18deMapT
#' @usage data(rCOP18deMapT)
#'
#' @format Object used for mapping indicators and disaggs across time, including from FY19 PSNU level to FY19 Site level. Does not translate new indicators/disaggs
"rCOP18deMapT"


#' @docType data
#' @title clusters
#' @usage data(clusters)
#'
#' @format Object used dealing with distribution of values from clusters to PSNUs
"clusters"

#' @docType data
#' @title sites_exclude
#' @usage data(sites_exclude)
#'
#' @format Vector of sites to exlcude
"sites_exclude"


#' @docType data
#' @title psnus
#' @usage data(psnus)
#'
#' @format  PSNU UIDs and names
"psnus"


#' @docType data
#' @title militaryUnits
#' @usage data(militaryUnits)
#'
#' @format PSNU UIDs and names
"militaryUnits"


#' @docType data
#' @title hts_site_schema
#' @usage data(hts_site_schema)
#'
#' @format Schema of the HTS site level tool
"hts_site_schema"


#' @docType data
#' @title main_site_schema
#' @usage data(main_site_schema)
#'
#' @format Schema of the Normal site level tool
"main_site_schema"