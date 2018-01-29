#' @export
#' @title ValidateSheet(wb_path,sheet_name,wb_type)
#'
#' @description Validates the layout of a single sheet based on its schema definition.
#' @param wb_path The absolute file path to the workbook.
#' @param sheet_name The name of the specific sheet to be validated within the workbook.
#' @param schema Specific schema for the sheet 
#' @return Returns a boolean value TRUE if the sheet is valid, otherwise, FALSE.
#'
ValidateSheet <- function(wb_path, sheet_name,schema) {

  cell_range = readxl::cell_limits(c(schema$row, schema$start_col),
                                   c(schema$row, schema$end_col))
  all( names(
    readxl::read_excel(wb_path, sheet = sheet_name, range = cell_range)
  ) == unlist(schema$fields,use.names = FALSE))
  
}

#' @export
#' @title GetWorkbookType(wb_path)
#'
#' @description Determines the workbook type
#' @param wb_path The absolute file path to the workbook.
#' @return Returns a character value of either NORMAL or HTS. Exits on anything else.
#'
GetWorkbookType<-function(wb_path) {
  foo<-names(readxl::read_excel(wb_path, sheet = "Home", range = "O3"))
  if (foo == "normal") {
    return("NORMAL")
  } else if (foo == "hts") {
    return("HTS")
  } else
  {
    stop("Unknown workbook type. Must be 'normal' or 'hts'!")
  }
  
  }

#' @export
#' @title ValidateWorkbook(wb_path,wb_type)
#'
#' @description Validates the layout of all relevant sheets in a data pack workbook
#' @param wb_path  The absolute file path to the workbook.
#' @param wb_type Should be either NORMAL or HTS
#' @return Returns a boolean value TRUE if the the workbook is valid, otherwise false
#'
#'
#'
ValidateWorkbook <- function(wb_path) {
  wb_type = GetWorkbookType(wb_path)
  if (wb_type == "HTS") { schemas <- datapackimporter::hts_schema }
  if (wb_type == "NORMAL") { schemas <-datapackimporter::main_schema }
  all_tables <- readxl::excel_sheets(path = wb_path)
  expected <- unlist(sapply(schemas$schema, `[`, c('sheet')),use.names = FALSE)
  all_there <- expected %in% all_tables
  #Validate against expected tables
  if ( !all(all_there) ) {
    warning(paste0("Some tables appear to be missing!:",paste(expected[!(all_there)],sep="",collapse=",")))
  }
  tables<-all_tables[all_tables %in% expected]
  result <-
    data.frame(sheet_name = tables,
               valid = FALSE,
               stringsAsFactors = FALSE)
  for (i in 1:nrow(result)) {
    sheet_name<-as.character(result$sheet_name[i])
    schema<-rlist::list.find(schemas$schema,sheet==sheet_name)[[1]]
    result$valid[i] <-
      ValidateSheet(wb_path, sheet_name,schema)
  }
  
  if (any(!(result$valid))) {
    invalid_sheets <-
      paste(result$sheet_name[!result$valid], sep = "", collapse = ",")
    msg <- paste0("The following sheets were invalid:", invalid_sheets)
    warning(msg)
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @export
#' @title ImportSheet(wb_path,sheet_name,wb_type)
#'
#' @description Imports a single sheet from a workbook.
#' @param wb_path  The absolute file path to the workbook.
#' @param sheet_name Name of the sheet to import. 
#' @param wb_type Should be either NORMAL or HTS.
#' @return Returns a data frame with the following columns:
#' #' \itemize{psnuuid,mechid,type,variable,value
#'   \item psnuid: Name of the PSNU or cluster
#'   \item mechid: Mechanism number
#' }
#'


ImportSheet <- function(wb_path, sheet_name,wb_type="NORMAL") {
  if (wb_type == "HTS") { schemas <- datapackimporter::hts_schema }
  if (wb_type == "NORMAL") { schemas <-datapackimporter::main_schema }
  schema <- schema<-rlist::list.find(schemas$schema,sheet==sheet_name)[[1]]
  cell_range = readxl::cell_limits(c(schema$row, schema$start_col),
                                   c(NA, schema$end_col))
  mechs<-datapackimporter::mechs
  des<-datapackimporter::des
  if ( schema$method == "standard") {
  d <-
    readxl::read_excel(wb_path, sheet = sheet_name, range = cell_range) %>%
    tidyr::gather(variable, value, -c(1:7)) %>% 
    dplyr::filter(.,  value != 0) %>% 
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(.,orgunit= psnuuid,mechid,type,variable,value) %>%
    dplyr::mutate(.,
           attributeoptioncombo = 
             plyr::mapvalues(mechid,
                             mechs$code,
                             mechs$uid,
                             warn_missing = FALSE),
           code = paste0(variable,"_",tolower(type)),
           period = "2018Oct") %>%
    dplyr::inner_join(.,des,by="code") %>%
    tidyr::separate(.,combi,c("dataelement","categoryoptioncombo")) %>%
    dplyr::select(.,dataelement,period,orgunit,categoryoptioncombo,attributeoptioncombo,value)
  } else {
      d<- tibble::tibble(
        "dataelement" = character(),
        "period" = character(),
        "orgunit" = character(),
        "categoryoptioncombo" = numeric(),
        "attributeoptioncombo" = character(),
        "value" = numeric()
      )
    }
  
  return(d)
  
}


#' @export
#' @title ImportSheets(wb_path,wb_type)
#'
#' @description Imports a single sheet from a workbook.
#' @param wb_path  The absolute file path to the workbook.
#' @return Returns a boolean value TRUE if the the workbook is valid, otherwise false
#'
ImportSheets <- function(wb_path) {
  wb_type = GetWorkbookType(wb_path)
  if (wb_type == "HTS") { schemas <- datapackimporter::hts_schema }
  if (wb_type == "NORMAL") { schemas <-datapackimporter::main_schema }
  sheets<-unlist(sapply(schemas$schema, `[`, c('sheet')),use.names = FALSE)
  df <- tibble::tibble(
    "dataelement" = character(),
    "period" = character(),
    "orgunit" = character(),
    "categoryoptioncombo" = character(),
    "attributeoptioncombo" = character(),
    "value" = numeric()
  )
  actual_sheets<-readxl::excel_sheets(wb_path)
  sheets_to_import<-actual_sheets[actual_sheets %in% sheets]
  
  for (i in 1:length(sheets_to_import)) {
    d <- ImportSheet(wb_path, sheets_to_import[i])
    df <- dplyr::bind_rows(df, d)
  }
return(df)
}