#' @export
#' @title ValidateSheet(schemas,sheet_name)
#'
#' @description Validates the layout of a single sheet based on its schema definition.
#' @param schemas Schemas of this workbook.
#' @param sheet_name Name of the sheet to be validated.
#' @param wb_info Info about the workbook.  
#' @return Returns a boolean value TRUE if the sheet is valid, otherwise, FALSE.
#'
ValidateSheet <- function(schemas,sheet_name,wb_info) {
  schema<-rlist::list.find(schemas$schema,sheet_name==sheet_name)[[1]]
  cell_range = readxl::cell_limits(c(schema$row, schema$start_col),
                                   c(schema$row, schema$end_col))
  all( names(
    readxl::read_excel(wb_info$wb_path, sheet = schema$sheet_name, range = cell_range)
  ) == unlist(schema$fields,use.names = FALSE))
  
}

#' @export
#' @title ValidateSheets(schemas,sheets)
#'
#' @description Validates all of the sheets
#' @param schemas Schemas for this workbook
#' @param sheets Names of sheets
#' @return Returns a boolean value TRUE if the sheet is valid, otherwise, FALSE.
#'
ValidateSheets<-function(schemas,sheets,wb_info) {
  
  vapply(sheets,function(x) { ValidateSheet(schemas = schemas,sheet_name = x,wb_info=wb_info) }, FUN.VALUE=logical(1) ) 
}


#' @export
#' @title ValidateImpattSheet(d,wb_info)
#' @description Validates the impatt sheet for completeness.
#' @param d A parsed data frame with IMPATT data
#' 
ValidateImpattSheet <- function(d, wb_info) {
  psnus <- datapackimporter::psnus[[wb_info$ou_uid]]
  psnus_missing <- !(psnus$id %in% d$psnuuid)
  if (any(psnus_missing)) {
    msg <-
      paste(
        "The following PNSUs were missing from the IMPATT table:",
        paste(psnus$name[psnus_missing], sep = "", collapse = ",")
      )
    return(msg)
  } else
  {
    return(NULL)
  }
}

#' @export
#' @title GetWorkbookInfo(wb_path)
#'
#' @description Provides information about the workbook
#' @param wb_path The absolute file path to the workbook.
#' @return Returns a list consiting of :
#'  \itemize{
#'    \item wb_path: Full path to the disagg tool 
#'    \item timestamp: Timestamp of when this script was run
#'    \item wb_type: Should either be HTS or NORMAL
#'    \item ou_name: Name of the operating unit
#'    \item ou_uid: UID of the operating unit }
#' 
#'
GetWorkbookInfo<-function(wb_path) {
  if (!file.exists(wb_path)) {stop("Workbook could not be read!")}
  #Distribution method
  promptText<-paste0("Please enter the distribution method (2017 or 2018):")
  distribution_methods<-c(2017,2018)
  print(promptText)
  distribution_method<-utils::select.list(distribution_methods,multiple=FALSE)
  wb_type<-names(readxl::read_excel(wb_path, sheet = "Home", range = "O3"))
  if ( wb_type == "normal") {
    wb_type = "NORMAL"
  } else if (wb_type == "hts") {
    wb_type = "HTS"
  } else
  {
    stop("Unknown workbook type. Must be 'normal' or 'hts'!")
  }
  ou_uid<-names(readxl::read_excel(wb_path, sheet = "Home", range = "O4"))
  ou_name<-names(readxl::read_excel(wb_path, sheet = "Home", range = "O1"))
  return(list(
    wb_path = tools::file_path_as_absolute(wb_path),
    timestamp = Sys.time(),
    wb_type=wb_type,
    ou_name=ou_name,
    ou_uid=ou_uid,
    is_clustered=ou_name %in% datapackimporter::clusters$operatingunit,
    distribution_method = distribution_method))
  }

#' @export
#' @title ValidateWorkbook(wb_path)
#'
#' @description Validates the layout of all relevant sheets in a data pack workbook
#' @param wb_path  The absolute file path to the workbook.
#' @return Returns a boolean value TRUE if the the workbook is valid, otherwise FALSE
#'
#'
#'
#'
ValidateWorkbook <- function(wb_path) {
  wb_info<-GetWorkbookInfo(wb_path)
  if (wb_info$wb_type == "HTS") { schemas <- datapackimporter::hts_schema }
  if (wb_info$wb_type == "NORMAL") { schemas <-datapackimporter::main_schema }
  all_sheets <- readxl::excel_sheets(path = wb_info$wb_path)
  expected <- unlist(sapply(schemas$schema, `[`, c('sheet_name')),use.names = FALSE)
  all_there <- expected %in% all_sheets
  #Validate against expected tables
  if ( !all(all_there) ) {
    stop(paste0("Some tables appear to be missing!:",paste(expected[!(all_there)],sep="",collapse=",")))
  }
  sheets<-all_sheets[all_sheets %in% expected]
  validation_results<-ValidateSheets(schemas,sheets,wb_info)
  if (any(!(validation_results))) {
    invalid_sheets <-
      paste(names(validation_results)[!validation_results], sep = "", collapse = ",")
    msg <- paste0("The following sheets were invalid:", invalid_sheets)
    stop(msg)
  } else {
    return(TRUE)
  }
}




#' @export
#' @importFrom stats complete.cases
#' @title ImportSheet(wb_path,schema)
#'
#' @description Imports a single sheet from a workbook.
#' @param wb_info  Workbook info object.
#' @param schema Schema of the sheet
#' @return Returns a data frame with the following columns. 
#' Will return an empty data frame if the the sheet is blank.
#' 
#'  \itemize{
#'   \item datalement: UID of the data elememnt
#'   \item period: ISO string of the period
#'   \item orgunit: UID of the organisation unit
#'   \item categoryoptioncombo: UID of the category option combo
#'   \item attributeoptioncombo: UID of the mechanism
#'   \item value: Value as a string.
#' }
#'


ImportSheet <- function(wb_info, schema) {

  cell_range = readxl::cell_limits(c(schema$row, schema$start_col),
                                   c(NA, schema$end_col))
  mechs<-datapackimporter::mechs
  des<-datapackimporter::des
  if ( schema$method == "standard") {
  d <-
    readxl::read_excel(wb_info$wb_path, sheet = schema$sheet_name, range = cell_range) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::gather(variable, value, -c(1:7),convert =FALSE) %>% 
    dplyr::filter(.,  value != "0" ) %>% 
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(.,orgunit= psnuuid,mechid,type,variable,value) %>%
    dplyr::mutate(.,
           attributeoptioncombo = 
             plyr::mapvalues(mechid,
                             mechs$code,
                             mechs$uid,
                             warn_missing = FALSE),
           code = paste0(variable,"_",tolower(type)),
           period = "2018Oct",
           value = as.character(value)) %>%
    dplyr::inner_join(.,des,by="code") %>%
    tidyr::separate(.,combi,c("dataelement","categoryoptioncombo")) %>%
    dplyr::select(.,dataelement,period,orgunit,categoryoptioncombo,attributeoptioncombo,value)
  } else if (schema$method == "impatt"){
    from<-c("snu_priotization_fy19","plhiv_fy19")
    #IMPATT.PRIORITY_SNU (SUBNAT), IMPATT.PLHIV (SUBNAT, Age/Sex)
    to<-c("r4zbW3owX9n","Rom79qVjNVb")
    #https://www.datim.org/api/optionSets/mvbwbgbJgXr.json?fields=options[code,name]
    d<-readxl::read_excel(wb_info$wb_path, sheet = schema$sheet_name, range = cell_range)
    msg<-ValidateImpattSheet(d,wb_info)
    if ( !is.null(msg) ) {
      warning(msg)
    }
    d <- d %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(.,
                    snu_priotization_fy19 =  plyr::mapvalues(snu_priotization_fy19,
                                              datapackimporter::impatt$options$dp_code,
                                              datapackimporter::impatt$options$code,
                                              warn_missing = FALSE)) %>% 
      tidyr::gather(variable, value, -c(1:2)) %>%
      dplyr::filter(complete.cases(.)) %>% 
      dplyr::mutate(., dataelement = plyr::mapvalues(variable,from,to,warn_missing = FALSE),
                    orgunit = psnuuid,
                    period = "2018Oct",
                    attributeoptioncombo = "HllvX50cXC0",
                    categoryoptioncombo = "HllvX50cXC0",
                    value = as.character(value) ) %>%
    dplyr::select(.,dataelement,period,orgunit,categoryoptioncombo,attributeoptioncombo,value)
      
  } else {
      d<- tibble::tibble(
        "dataelement" = character(),
        "period" = character(),
        "orgunit" = character(),
        "categoryoptioncombo" = character(),
        "attributeoptioncombo" = character(),
        "value" = character()
      )
    }
  
  return(d)
  
}

#' @export
#' @title ImportFollowOnMechs(wb_path)
#'
#' @description Imports the follow on mechs sheet.
#' @param wb_info  Workbook info object.
#' @return A data  frame with three columns Closing Out, Follow On, Notes. 
#' If this sheet is blank, returns NULL.

ImportFollowOnMechs<-function(wb_info) {
  if (wb_info$wb_type == "NORMAL") { schemas <-datapackimporter::main_schema } else {
    stop("Only Normal Disagg tools with follow on mechs are supported!")
  }
  sheet_to_import = "Follow on Mech List"
  schema<-rlist::list.find(schemas$schema,sheet_name==sheet_to_import)[[1]]
  cell_range = readxl::cell_limits(c(schema$row, schema$start_col),
                                   c(NA, schema$end_col))
  d<-readxl::read_excel(wb_path, sheet = schema$sheet_name, range = cell_range)
  if (!is.null(d) & nrow(d) > 0) {
    return(d)
  } else {
    return(NULL)
  }
}



#' @export
#' @title ImportSheets(wb_path)
#'
#' @description Imports all sheets from the workbook
#' @param wb_path  The absolute file path to the workbook.
#' @return Returns a list of data frames: 
#' #'  \itemize{
#'            \item wb_info: Workbook Info
#'            \item data: Standard d2importer data frame
#'            \item follow_on_mechs: Data frame of follow on mechs.
#'            }

#'
ImportSheets <- function(wb_path) {
  wb_info = GetWorkbookInfo(wb_path)
  if (wb_info$wb_type == "HTS") { schemas <- datapackimporter::hts_schema }
  if (wb_info$wb_type == "NORMAL") { schemas <-datapackimporter::main_schema }
  sheets<-unlist(sapply(schemas$schema, `[`, c('sheet_name')),use.names = FALSE)
  df <- tibble::tibble(
    "dataelement" = character(),
    "period" = character(),
    "orgunit" = character(),
    "categoryoptioncombo" = character(),
    "attributeoptioncombo" = character(),
    "value" = character()
  )
  actual_sheets<-readxl::excel_sheets(wb_info$wb_path)
  sheets_to_import<-actual_sheets[actual_sheets %in% sheets]
  
  for (i in 1:length(sheets_to_import)) {
    
    schema<-rlist::list.find(schemas$schema,sheet_name==sheets_to_import[i])[[1]]
    d <- ImportSheet(wb_info, schema)
    df <- dplyr::bind_rows(df, d)
  }
  
  
  #Import the follow on mechs
  if (wb_info$wb_type == "NORMAL") {
  follow_on_mechs<-ImportFollowOnMechs(wb_info)
  } else {
    follow_on_mechs<-NULL
  }
  
  return ( list(wb_info = wb_info,
    follow_on_mechs=follow_on_mechs,
              data = df) )
}