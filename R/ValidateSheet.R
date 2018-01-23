#' @export
#' @title ValidateSheet(wb_path,sheet_name,wb_type)
#'
#' @description Validates the layout of a single sheet based on its schema definition.
#' @param wb_path The absolute file path to the workbook.
#' @param sheet_name The name of the specific sheet to be validated within the workbook.
#' @param wb_type Should be either NORMAL or HTS
#' @return Returns a boolean value TRUE if the sheet is valid, otherwise, FALSE.
#'
ValidateSheet <- function(wb_path, sheet_name,wb_type="NORMAL") {
  if (wb_type == "HTS") { schemas <- datapackimporter::hts_schema }
  if (wb_type == "NORMAL") { schemas <-datapackimporter::main_schema }
  schema<-rlist::list.find(schemas$schema,sheet==sheet_name)[[1]]

  cell_range = readxl::cell_limits(c(schema$row, schema$start_col),
                                   c(schema$row, schema$end_col))
  Reduce("&", names(
    readxl::read_excel(wb_path, sheet = sheet_name, range = cell_range)
  ) == unlist(schema$fields,use.names = FALSE))
  
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
ValidateWorkbook <- function(wb_path,wb_type="NORMAL") {
  if (wb_type == "HTS") { schemas <- datapackimporter::hts_schema }
  if (wb_type == "NORMAL") { schemas <-datapackimporter::main_schema }
 all_tables <- readxl::excel_sheets(path = wb_path)
  tables <-
    all_tables[sapply(all_tables, function(x)
      grepl("Targets", x))]
  #Validate against expected tables
  if (!identical(tables, unlist(sapply(schemas$schema, `[`, c('sheet')),use.names = FALSE))) {
    warning("Some tables appear to be missing!")
  }
  
  result <-
    data.frame(sheet_name = tables,
               valid = FALSE,
               stringsAsFactors = FALSE)
  for (i in 1:nrow(result)) {
    result$valid <-
      ValidateSheet(wb_path, as.character(result$sheet_name[i]))
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
#' #' \itemize{
#'   \item First item
#'   \item Second item
#' }
#'


ImportSheet <- function(wb_path, sheet_name,wb_type="NORMAL") {
  if (wb_type == "HTS") { schemas <- datapackimporter::hts_schema }
  if (wb_type == "NORMAL") { schemas <-datapackimporter::main_schema }
  schema <- schema<-rlist::list.find(schemas$schema,sheet==sheet_name)[[1]]
  cell_range = readxl::cell_limits(c(schema$row, schema$start_col),
                                   c(NA, schema$end_col))
  d <-
    readxl::read_excel(wb_path, sheet = sheet_name, range = cell_range) %>%
    tidyr::gather(variable, value, -c(1:7)) %>% 
    dplyr::filter(., value != 0) %>% 
    dplyr::filter(., !is.na(value))
  #MOre remapping here
  return(d)
  
}


#' @export
#' @title ImportSheets(wb_path,wb_type)
#'
#' @description Imports a single sheet from a workbook.
#' @param wb_path  The absolute file path to the workbook.
#' @param wb_type Should be either NORMAL or HTS.
#' @return Returns a boolean value TRUE if the the workbook is valid, otherwise false
#'
ImportSheets <- function(wb_path,wb_type="NORMAL") {
  if (wb_type == "HTS") { schemas <- datapackimporter::hts_schema }
  if (wb_type == "NORMAL") { schemas <-datapackimporter::main_schema }
  sheets<-unlist(sapply(schemas$schema, `[`, c('sheet')),use.names = FALSE)
  df <- tibble::tibble(
    "psnu" = character(),
    "psnuuid" = character(),
    "priority" = character(),
    "mechid" = numeric(),
    "mechname" = character(),
    "type" = character(),
    "psnu_type"=character(),
    "variable" = character(),
    "value" = numeric()
  )
  actual_sheets<-readxl::excel_sheets(wb_path)
  sheets_to_import<-actual_sheets[actual_sheets %in% sheets]
  for (i in 1:length(sheets_to_import)) {
    d <- ImportSheet(wb_path, sheets_to_import[i])
    df <- dplyr::bind_rows(df, d)
  }
  
  return( dplyr::select(df,psnuuid,mechid,type,variable,value) )
}