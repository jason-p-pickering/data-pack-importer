#' @export
#' @title ValidateSheet(sheet_path,sheet_name)
#'
#' @description Validates the layout of a single sheet based on its schema definition.
#'
#' @return Returns a boolean value TRUE if the sheet is valid, otherwise, FALSE.
#'
ValidateSheet <- function(sheet_path, sheet_name) {
  schema <- datapackimporter::schemas[sheet_name, ]
  
  cell_range = readxl::cell_limits(c(schema$row, schema$start_col),
                                   c(schema$row, schema$end_col))
  Reduce("&", names(
    readxl::read_excel(sheet_path, sheet = sheet_name, range = cell_range)
  ) == schemas[sheet_name, "fields"][[1]])
  
}

#' @export
#' @title ValidateWorkbook(sheet_path)
#'
#' @description Validates the layout of all relevant sheets in a data pack workbook
#'
#' @return Returns a boolean value TRUE if the the workbook is valid, otherwise false
#'
#'
#'
ValidateWorkbook <- function(sheet_path) {
  schemas <- datapackimporter::schemas
  all_tables <- readxl::excel_sheets(path = sheet_path)
  tables <-
    all_tables[sapply(all_tables, function(x)
      grepl("Targets", x))]
  #Validate against expected tables
  if (!identical(tables, schemas$sheet)) {
    warning("Some tables appear to be missing!")
  }
  
  result <-
    data.frame(sheet_name = tables,
               valid = FALSE,
               stringsAsFactors = FALSE)
  for (i in 1:nrow(result)) {
    result$valid <-
      ValidateSheet(sheet_path, as.character(result$sheet_name[i]))
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


ImportSheet <- function(sheet_path, sheet_name) {
  schema <- datapackimporter::schemas[sheet_name, ]
  cell_range = readxl::cell_limits(c(schema$row, schema$start_col),
                                   c(NA, schema$end_col))
  d <-
    readxl::read_excel(sheet_path, sheet = sheet_name, range = cell_range) %>%
    dplyr::gather(variable, value, -c(1:6)) %>% dplyr::filter(., value != 0) %>% dplyr::filter(., !is.na(value))
  return(d)
  
}

ImportSheets <- function(sheet_path) {
  schemas <- datapackimporter::schemas
  sheets <- schemas$sheet
  df <- tibble::tibble(
    "psnu" = character(),
    "psnuuid" = character(),
    "priority" = character(),
    "mechid" = numeric(),
    "mechname" = character(),
    "type" = character(),
    "variable" = character(),
    "value" = numeric()
  )
  actual_sheets<-readxl::excel_sheets(sheet_path)
  sheets_to_import<-actual_sheets[actual_sheets %in% sheets]
  for (i in 1:length(sheets_to_import)) {
    d <- ImportSheet(sheet_path, sheets_to_import[i])
    df <- dplyr::bind_rows(df, d)
  }
  
  return(df)
}