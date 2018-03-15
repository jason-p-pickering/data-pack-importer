#' @export
#' @title ValidateSheet(d,this_sheet)
#'
#' @description Validates the layout of a single sheet based on its schema definition.
#' @param d Info about the workbook.
#' @param this_sheet A particular sheet to validate.
#' @return Returns a boolean value TRUE if the sheet is valid, otherwise, FALSE.
#'
ValidateSheet <- function(d, this_sheet) {
  schema <- rlist::list.find(d$schemas$schema, sheet_name == this_sheet)
  if (length(schema) != 1) {
    stop("Could not find the exact schema for this sheet!")
  } else {
    schema <- schema[[1]]
  }
  cell_range <- readxl::cell_limits(
    c(schema$row, schema$start_col),
    c(schema$row, schema$end_col)
  )
  fields_got <- tryCatch(
    {
        names(
          readxl::read_excel(
            d$wb_info$wb_path,
            sheet = this_sheet,
            range = cell_range,
            col_types = "text"
          ))
    },
    error = function(err) {
      msg<-paste0("Could not parse sheet ", this_sheet,". File may be corrupt!")
      stop(msg)
    },
    finally = {}
  )
  
  fields_want <- unlist(schema$fields, use.names = FALSE)
  all_good <- all(fields_want == fields_got)

  if (!all_good) {
    fields_compare <- data.frame(wanted = fields_want, got = fields_got, stringsAsFactors = FALSE) %>%
      dplyr::mutate(ok = fields_want == fields_got) %>%
      dplyr::filter(!ok)
    warning(paste0("Some fields did not match for ", this_sheet))
    print(fields_compare)
    return(FALSE)
  }

  return(TRUE)
}

#' @export
#' @title ValidateSheets(d)
#'
#' @description Validates all of the sheets
#' @param d Parsed data pack object with workbook info
#' @return Returns a boolean named vector of sheets and their validation status.
#'
ValidateSheets <- function(d) {
  sheets <- unlist(sapply(d$schemas$schema, `[`, c("sheet_name")), use.names = FALSE)
  vapply(sheets, function(x) {
    ValidateSheet(d, x)
  }, FUN.VALUE = logical(1))
}

#' @export
#' @title GetWorkbookInfo(wb_path,distribution_method,support_files_path)
#'
#' @description Provides information about the workbook
#' @param wb_path The absolute file path to the workbook.
#' @param distribution_method The distribution method to use.
#' @param support_files_path Path to the support files directory.
#' @return Returns a list consiting of :
#'  \itemize{
#'    \item wb_path: Full path to the disagg tool
#'    \item timestamp: Timestamp of when this script was run
#'    \item wb_type: Should either be HTS or NORMAL
#'    \item ou_name: Name of the operating unit
#'    \item ou_uid: UID of the operating unit }
#'
#'
GetWorkbookInfo <- function(wb_path, distribution_method=NA, support_files_path=NA) {
  if (!file.exists(wb_path)) {
    stop("Workbook could not be read!")
  }

  if (is.na(support_files_path)) {
    # Supporting files directory
    support_files_path <- readline("Please provide the path to DataPack Support Files:")
  }

  if (!stringr::str_detect(support_files_path, "\\/$")) {
    stop("support_files_path must include a final slash!")
  }

  if (!dir.exists(support_files_path)) {
    stop("Could not access support files directory!")
  }


  wb_type <- names(readxl::read_excel(wb_path, sheet = "Home", range = "O3"))

  if (wb_type == "normal") {
    wb_type <- "NORMAL"
    distribution_method <- get_distribution_method(distribution_method)
    schemas <- datapackimporter::main_schema
  } else if (wb_type == "hts") {
    wb_type <- "HTS"
    distribution_method <- get_distribution_method(distribution_method)
    schemas <- datapackimporter::hts_schema
  } else if (wb_type == "NORMAL_SITE") {
    distribution_method <- names(readxl::read_excel(wb_path, sheet = "Home", range = "O5"))
    schemas <- datapackimporter::main_site_schema
  } else if (wb_type == "HTS_SITE") {
    distribution_method <- names(readxl::read_excel(wb_path, sheet = "Home", range = "O5"))
    schemas <- datapackimporter::hts_site_schema
  } else {
    stop("Unknown DataPack type!")
  }

  ou_uid <- names(readxl::read_excel(wb_path, sheet = "Home", range = "O4"))
  ou_name <- names(readxl::read_excel(wb_path, sheet = "Home", range = "O1"))
  return(list(
    wb_info = list(
      wb_path = tools::file_path_as_absolute(wb_path),
      timestamp = Sys.time(),
      wb_type = wb_type,
      ou_name = ou_name,
      ou_uid = ou_uid,
      is_clustered = ou_name %in% datapackimporter::clusters$operatingunit,
      distribution_method = distribution_method,
      support_files_path = support_files_path
    ),
    schemas = schemas
  ))
}

#' @export
#' @title ValidateWorkbook(wb_path)
#'
#' @description Validates the layout of all relevant sheets in a data pack workbook
#' @param wb_path  The absolute file path to the workbook.
#' @param distribution_method The distribution method to use.
#' @param support_files_path Path to the support files directory.
#' @return Returns an object with information about the workbook, if the file is valid.
#' Otherwise, the function will produce an error.
#'
#'
#'
#'
ValidateWorkbook <- function(wb_path, distribution_method=NA, support_files_path=NA) {
  d <- NULL
  d <-
    GetWorkbookInfo(
      wb_path,
      distribution_method = distribution_method,
      support_files_path = support_files_path
    )


  all_sheets <- readxl::excel_sheets(path = d$wb_info$wb_path)
  expected_sheets <- unlist(sapply(d$schemas$schema, `[`, c("sheet_name")), use.names = FALSE)
  all_there <- expected_sheets %in% all_sheets
  # Validate against expected tables
  if (!all(all_there)) {
    stop(paste0("Some tables appear to be missing!:", paste(expected_sheets[!(all_there)], sep = "", collapse = ",")))
  }
  validation_results <- ValidateSheets(d)
  if (any(!(validation_results))) {
    invalid_sheets <-
      paste(names(validation_results)[!validation_results], sep = "", collapse = ",")
    msg <- paste0("The following sheets were invalid:", invalid_sheets)
    stop(msg)
  } else {
    return(d)
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
  
  if (schema$method == "standard") {
    d<-import_disagg_tool_sheet(wb_info,schema)
  } else if (schema$method == "impatt") {
    
    d<-import_impatt_sheet(wb_info,schema)
    
  } else if (schema$method == "site_tool") {
    
    d<-import_site_tool_sheet(wb_info,schema) 
    
  } else {
    #TODO: Error out here instead
    d <- empty_dhis_tibble()
  }

  return(d)
}


#' @export
#' @title ImportSheets(wb_path,distr)
#'
#' @description Imports all sheets from the workbook
#' @param wb_path  The absolute file path to the workbook.
#' @param distribution_method The distribution method to use.
#' @param support_files_path Path to the support files directory.
#' @return Returns a list of data frames:
#' #'  \itemize{
#'            \item wb_info: Workbook Info
#'            \item data: Standard d2importer data frame
#'            \item follow_on_mechs: Data frame of follow on mechs.
#'            }
#'
ImportSheets <- function(wb_path=NA, distribution_method=NA, support_files_path=NA) {
  d <-
    ValidateWorkbook(wb_path, distribution_method, support_files_path)

  sheets <-
    unlist(sapply(d$schemas$schema, `[`, c("sheet_name")), use.names = FALSE)
  df <- tibble::tibble(
    "dataelement" = character(),
    "period" = character(),
    "orgunit" = character(),
    "categoryoptioncombo" = character(),
    "attributeoptioncombo" = character(),
    "value" = character()
  )
  actual_sheets <- readxl::excel_sheets(d$wb_info$wb_path)
  sheets_to_import <- actual_sheets[actual_sheets %in% sheets]

  sheet_name <- NULL

  for (i in 1:length(sheets_to_import)) {
    schema <- rlist::list.find(d$schemas$schema, sheet_name == sheets_to_import[i])[[1]]
    df_parsed <- ImportSheet(d$wb_info, schema)
    df <- dplyr::bind_rows(df, df_parsed)
  }
  
  d$data<-df
  
  # Calculate sums
  if (d$wb_info$wb_type %in% c("HTS", "NORMAL")) {
    df_codes <- unique(datapackimporter::rCOP18deMapT[, c("pd_2019_P", "DataPackCode")]) %>%
      na.omit() %>%
      dplyr::distinct()

    # Generate the sums
    sums <- d$data %>%
      dplyr::mutate(
        value = as.numeric(value),
        pd_2019_P = paste0(dataelement, ".", categoryoptioncombo)
      ) %>%
      dplyr::left_join(df_codes, by = c("pd_2019_P")) %>%
      dplyr::mutate(match_code = gsub("_dsd$", "", DataPackCode)) %>%
      dplyr::mutate(match_code = gsub("_ta$", "", match_code)) %>%
      dplyr::select(match_code, value) %>%
      dplyr::group_by(match_code) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE))

    # Pad with zeros

    df_zeros <- df_codes %>%
      dplyr::mutate(match_code = gsub("_dsd$", "", DataPackCode)) %>%
      dplyr::mutate(match_code = gsub("_ta$", "", match_code)) %>%
      na.omit() %>%
      dplyr::distinct() %>%
      dplyr::select(match_code) %>%
      dplyr::mutate(value = 0)

    sums <- sums %>%
      dplyr::bind_rows(df_zeros) %>%
      dplyr::group_by(match_code) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::mutate(value = round_trunc(value))
    d$sums<-sums
  }

  # Import the follow on mechs
  if (d$wb_info$wb_type == "NORMAL") {
    d <- ImportFollowOnMechs(d) } 
  
  return(d)
}
