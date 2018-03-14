#' @export
#' @title ImportFollowOnMechs(wb_info)
#'
#' @description Imports the follow on mechs sheet.
#' @param wb_info  Workbook info object.
#' @return A data  frame with three columns Closing Out, Follow On, Notes.
#' If this sheet is blank, returns NULL.

ImportFollowOnMechs <- function(wb_info) {
  sheet_name <- NULL
  if (wb_info$wb_type == "NORMAL") {
    schemas <- datapackimporter::main_schema
  } else {
    stop("Only Normal Disagg tools with follow on mechs are supported!")
  }
  sheet_to_import <- "Follow on Mech List"
  schema <- rlist::list.find(schemas$schema, sheet_name == sheet_to_import)[[1]]
  cell_range <- readxl::cell_limits(
    c(schema$row, schema$start_col),
    c(NA, schema$end_col)
  )
  d <- readxl::read_excel(wb_info$wb_path, sheet = sheet_to_import, range = cell_range, col_types = "text")
  if (!is.null(d) & nrow(d) > 0) {
    return(d)
  } else {
    return(NULL)
  }
}


