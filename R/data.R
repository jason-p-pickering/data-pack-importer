#' @export
#' @title hts_schema
#'
#' @description Returns the HTS Schema
#' @return Returns a list which defines the schema for the HTS Disagg tool
#'
#'HTS Schemas of the Data Pack Excel sheets
"hts_schema"

#' @export
#' @title main_schema
#'
#' @description Normal Schemas of the Data Pack Excel sheets
#' @return Returns a list which defines the schema for the  Disagg tool
#'
"main_schema"

#' @export
#' @title mechs
#'
#' @description List of mechanisms and codes
#' @return Returns a two column data frame with mechanisms and codes 
#'
"mechs"

#' @export
#' @title impatt
#'
#' @description List of option codes for PSNU prioritization
#' @return Returns a three column data frame with code,name and Data Pack code (dp_code)
#'
"impatt"

#' @export
#' @title des
#'
#' @description List of DataPack codes and DATIM DE.COC combinations
#' @return Returns a two column data frame with codes and combis
#'
"des"


#' @export
#' @title COPdataElements
#'
#' @description Object used for further processing and matching of data pack codes
#' @return Returns an object with the following columns
"COPdataElements"