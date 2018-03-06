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

  fields_got <- names(readxl::read_excel(d$wb_info$wb_path, sheet = this_sheet, range = cell_range,col_types = "text"))
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
#' @title ValidateImpattSheet(d,wb_info)
#' @description Validates the impatt sheet for completeness.
#' @param d A parsed data frame with IMPATT data
#' @param wb_info Workbook info for the workbook
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
  } else {
    return(NULL)
  }
}

get_distribution_method <- function(distribution_method=NA) {
  distribution_methods <- c(2017, 2018)
  if (is.na(distribution_method) | !any(distribution_method %in% distribution_methods)) {
    # Distribution method
    promptText <- paste0("Please enter the distribution method (2017 or 2018):")
    print(promptText)
    distribution_method <- utils::select.list(distribution_methods, multiple = FALSE)
  }

  return(distribution_method)
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


check_invalid_mechs_by_code <- function(d, sheet_name) {
  mechs_wanted <- datapackimporter::mechs$code
  #Check for invalid mechanisms
  invalid_mechs<-unique(d$mech_code)[!(unique(d$mech_code) %in% mechs_wanted)] 
  if (length(invalid_mechs) >0 ) {
    msg<-paste0("The following mechanisms in sheet ", sheet_name, " were invalid:", 
                paste(invalid_mechs,sep="",collapse=";"))
    warning(msg)
    return(FALSE)
  }
  return(TRUE)
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
    cell_range <- readxl::cell_limits(
      c(schema$row, schema$start_col),
      c(NA, schema$end_col)
    )
    mechs <- datapackimporter::mechs
    
    des <- datapackimporter::rCOP18deMapT %>%
      dplyr::select(code = DataPackCode, combi = pd_2019_P) %>%
      dplyr::filter(., complete.cases(.)) %>%
      dplyr::distinct()
    
    d <-
      readxl::read_excel(wb_info$wb_path, sheet = schema$sheet_name, range = cell_range,col_types = "text") %>%
      dplyr::mutate_all(as.character) %>%
      tidyr::gather(variable, value, -c(1:7), convert = FALSE) %>%
      dplyr::filter(., value != "0") %>%
      dplyr::filter(!is.na(value)) %>%
      #Special handling for dedupe which is coerced to 0 and 1
      dplyr::filter( . ,!(mechid %in% c("0","00000","1","00001"))) %>%
      dplyr::select( . , orgunit = psnuuid, mech_code=mechid, type, variable, value)
    
      check_invalid_mechs_by_code( d = d, sheet_name=schema$sheet_name )
      
      d<-d %>% dplyr::mutate(
        .,
        attributeoptioncombo =
          plyr::mapvalues(
            .$mech_code,
            mechs$code,
            mechs$uid,
            warn_missing = FALSE
          ),
        code = paste0(variable, "_", tolower(type)),
        period = "2018Oct",
        value = as.character(value)
      ) %>%
      dplyr::inner_join(., des, by = "code") %>%
      tidyr::separate(., combi, c("dataelement", "categoryoptioncombo")) %>%
      dplyr::select(., dataelement, period, orgunit, categoryoptioncombo, attributeoptioncombo, value) %>%
      # Filter out all dedupe data
      dplyr::filter(!attributeoptioncombo %in% c("YGT1o7UxfFu", "X8hrDf6bLDC"))
  } else if (schema$method == "impatt") {
    cell_range <- readxl::cell_limits(
      c(schema$row, schema$start_col),
      c(NA, schema$end_col)
    )
    from <- c("snu_priotization_fy19", "plhiv_fy19")
    # IMPATT.PRIORITY_SNU (SUBNAT), IMPATT.PLHIV (SUBNAT, Age/Sex)
    to <- c("r4zbW3owX9n", "rORzrY9rpQ1")
    # https://www.datim.org/api/optionSets/mvbwbgbJgXr.json?fields=options[code,name]
    d <- readxl::read_excel(wb_info$wb_path, sheet = schema$sheet_name, range = cell_range, col_types = "text")
    msg <- ValidateImpattSheet(d, wb_info)
    if (!is.null(msg)) {
      warning(msg)
    }
    d <- d %>%
      dplyr::filter(snu_priotization_fy19 != "Mil") %>%
      dplyr::mutate(
        .,
        snu_priotization_fy19 = plyr::mapvalues(
          snu_priotization_fy19,
          datapackimporter::impatt$options$dp_code,
          datapackimporter::impatt$options$code,
          warn_missing = FALSE
        )
      ) %>%
      tidyr::gather(variable, value, -c(1:2)) %>%
      dplyr::filter(complete.cases(.)) %>%
      dplyr::mutate(
        ., dataelement = plyr::mapvalues(variable, from, to, warn_missing = FALSE),
        orgunit = psnuuid,
        period = "2018Oct",
        attributeoptioncombo = "HllvX50cXC0",
        categoryoptioncombo = "HllvX50cXC0",
        value = as.character(value)
      ) %>%
      dplyr::select(., dataelement, period, orgunit, categoryoptioncombo, attributeoptioncombo, value)
  } else if (schema$method == "site_tool") {
    cell_range <- readxl::cell_limits(
      c(schema$row, schema$start_col),
      c(NA, schema$end_col)
    )
    de_map <- datapackimporter::rCOP18deMapT %>%
      dplyr::select(supportType, pd_2019_S, pd_2019_P, DataPackCode) %>%
      na.omit() %>%
      dplyr::distinct()

    mechs <- datapackimporter::mechs

    d <- readxl::read_excel(wb_info$wb_path, sheet = schema$sheet_name, range = cell_range,col_types = "text") %>%
      dplyr::select(-Inactive) %>%
      tidyr::gather(variable, value, -c(1:3, convert = FALSE)) %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::filter(!(value == "NA")) %>%
      #Special handling for dedupe which is coerced to 0 and 1
      #Dedupe should always be dropped. 
      dplyr::filter( . ,!(Mechanism %in% c("0","00000","1","00001"))) 

    unallocated <- dplyr::filter(d, grepl("NOT YET DISTRIBUTED", Site)) %>%
      dplyr::pull(Site) %>%
      unique() %>%
      stringr::str_replace(., " > NOT YET DISTRIBUTED", "")

    if (length(unallocated) > 0) {
      msg <-
        paste0(
          "There are unallocated values in sheet ",
          schema$sheet_name,
          ":",
          paste(unallocated, sep = "", collapse = ",")
        )
      warning(msg)
    }

    negative_values <- d %>% dplyr::filter(as.numeric(value) < 0)

    if (NROW(negative_values) > 0) {
      msg <- paste0("Negative values were found in ", schema$sheet_name, "! Aborting!")
      stop(msg)
    }

    d <- d %>%
      dplyr::mutate(
        mech_code = stringi::stri_extract_first_regex(Mechanism, "^[0-9]+"),
        orgunit = stringi::stri_extract_first_regex(Site, "(?<=\\()([A-Za-z][A-Za-z0-9]{10})(?=\\))"),
        DataPackCode = paste0(variable, "_", tolower(Type)),
        period = "2018Oct"
      ) 
    
      mechs_are_valid<-check_invalid_mechs_by_code(d=d,sheet_name = schema$sheet_name) 
      
      d<-d %>%
      dplyr::left_join(mechs, by = c("mech_code" = "code")) %>%
      dplyr::left_join(de_map, by = "DataPackCode") %>%
      tidyr::separate(., pd_2019_S, c("dataelement", "categoryoptioncombo")) %>%
      dplyr::select(dataelement, period, orgunit, categoryoptioncombo, attributeoptioncombo = uid, value)
  
      }

  else {
    d <- tibble::tibble(
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
  d <- readxl::read_excel(wb_info$wb_path, sheet = sheet_to_import, range = cell_range,col_types = "text")
  if (!is.null(d) & nrow(d) > 0) {
    return(d)
  } else {
    return(NULL)
  }
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

  has_negative_numbers <- as.numeric(df$value) < 0
  if (any(has_negative_numbers)) {
    foo <- df[has_negative_numbers, ]
    warning("Negative values were found in the data!")
    print(foo)
  }

  # Calculate sums
  if (d$wb_info$wb_type %in% c("HTS", "NORMAL")) {
    df_codes <- unique(datapackimporter::rCOP18deMapT[, c("pd_2019_P", "DataPackCode")]) %>%
      na.omit() %>%
      dplyr::distinct()

    # Generate the sums
    sums <- df %>%
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
  } else {
    sums <- NULL
  }


  # Import the follow on mechs
  if (d$wb_info$wb_type == "NORMAL") {
    follow_on_mechs <- ImportFollowOnMechs(d$wb_info)
  } else {
    follow_on_mechs <- NULL
  }
  return(list(
    wb_info = d$wb_info,
    schemas = d$schemas,
    follow_on_mechs = follow_on_mechs,
    sums = sums,
    data = df
  ))
}