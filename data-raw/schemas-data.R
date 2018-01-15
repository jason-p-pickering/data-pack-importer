#' @export
#' @title ValidateSheet(sheet_path,sheet_name)
#'
#' @description Validates the layout of a single sheet based on its schema definition.  
#' 
#' @return Returns a boolean value TRUE if the sheet is valid, otherwise, FALSE.
#'
ValidateSheet <- function(sheet_path, sheet_name) {
  schemas <- LoadSheetsSchema()
  fields <- schemas[sheet_name, "fields"][[1]]
  cell_range <-
    paste0(schemas[sheet_name, "start"], ":", schemas[sheet_name, "end"])
  Reduce("&", names(read_excel(
    sheet_path, sheet = sheet_name, range = cell_range
  )) == fields)
  
}
