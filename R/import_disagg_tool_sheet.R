check_psnu_duplicates<-function(d,sheet_name,wb_info) {
  
  any_dups<- d %>% 
    dplyr::select(code,psnu, mech_code) %>%
    dplyr::group_by(code,psnu , mech_code) %>%
    dplyr::summarise(n=n()) %>%
    dplyr::filter(n>1) %>%
    dplyr::ungroup()%>%
    dplyr::select(psnu,mech_code) %>%
    dplyr::distinct() %>%
    dplyr::arrange(psnu,mech_code) %>%
    dplyr::mutate(row_id = paste(psnu,":",mech_code)) %>%
    dplyr::pull(row_id)
  
  if (NROW(any_dups)>0) {
    
    warning(paste0(
      "Duplicate rows found sheet ",
      sheet_name,
      " in rows: ",
      paste(any_dups, sep = "", collapse = ",")
    ))
  }
  
}



#' @title import_disagg_tool_sheet(wb_info,schema)
#'
#' @param wb_info Workbook info object
#' @param schema Schema for a single sheet
#'
#' @return A DHIS2 compatible import data frame
#'
#'
import_disagg_tool_sheet <- function(wb_info, schema) {
  cell_range <- readxl::cell_limits(
    c(schema$row, schema$start_col),
    c(NA, schema$end_col)
  )

  d <- readxl::read_excel(
    wb_info$wb_path,
    sheet = schema$sheet_name,
    range = cell_range,
    col_types = "text"
  )

  if (NROW(d) == 0) {
    return(empty_dhis_tibble())
  }

  mechs <- datapackimporter::mechs

  des <- datapackimporter::rCOP18deMapT %>%
    dplyr::select(code = DataPackCode, combi = pd_2019_P) %>%
    dplyr::filter(., complete.cases(.)) %>%
    dplyr::distinct() %>%
    tidyr::separate(
      col = combi,
      into = c("dataelement", "categoryoptioncombo")
    )

  d <- d %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::gather(variable, value, -c(1:7), convert = FALSE) %>%
    dplyr::select(-psnu_type) %>%
    # Special handling for dedupe which is coerced to 0 and 1
    # Dedupe should always be dropped.
    dplyr::filter(!(mechid %in% c("0", "00000", "1", "00001"))) %>%
    dplyr::filter(!(value == "NA")) %>%
    # Remove anyting which is not-numeric
    dplyr::filter(!is.na(suppressWarnings(as.numeric(value)))) %>%
    # Remove anything which is close to zero
    dplyr::filter(round_trunc(as.numeric(value)) != "0") %>%
    dplyr::mutate(
      code = paste0(variable, "_", tolower(type)),
      period = "2018Oct"
    ) %>%
    # Filter out data elements we are not interested in
    dplyr::inner_join(des, by = "code") %>%
    dplyr::select(
      psnu,
      orgunit = psnuuid,
      mech_code = mechid,
      period,
      dataelement,
      categoryoptioncombo,
      code,
      value
    )
  # Perform checks before transformation to DHIS2 UIDs to make identification easier
  mech_check <-
    check_mechs_by_code(d = d, wb_info, sheet_name = schema$sheet_name)
  dup_check <- check_psnu_duplicates(d, schema$sheet_name, wb_info)

  na_orgunits <- is.na(d$orgunit)
  if (any(na_orgunits)) {
    na_rows <- paste((which(na_orgunits) + schema$row), sep = "", collapse = ",")
    warning(paste("Organisation units with NULL UIDs were found in sheet", schema$sheet_name, "in rows ", na_rows))
  }

  d <- d %>%
    dplyr::mutate(
      .,
      attributeoptioncombo =
        plyr::mapvalues(
          .$mech_code,
          mechs$code,
          mechs$uid,
          warn_missing = FALSE
        )
    ) %>%
    # Filter out all dedupe data once again
    dplyr::filter(!attributeoptioncombo %in% c("YGT1o7UxfFu", "X8hrDf6bLDC")) %>%
    dplyr::select(
      dataelement,
      period,
      orgunit,
      categoryoptioncombo,
      attributeoptioncombo,
      value
    )
  # At this point, there should be no significant negative numbers
  check_negative_numbers(d, schema$sheet_name)

  return(d)
}