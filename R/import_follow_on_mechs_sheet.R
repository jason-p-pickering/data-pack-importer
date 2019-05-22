
validate_follow_on_mechs_sheet<-function(d_follow_on,d) {
  
  is_valid<-TRUE
  messages<-NULL
  #Missing rows
  if ( any( is.na(d_follow_on[,c("closingCode","followOnCode")]) ) ) {
    msg<-"Blank mechanisms were found in the Follow-on mechs sheet!"
    is_valid<-FALSE
  }
  
  #Follow on mech and closing mech are the same
  
  if (any(d_follow_on$closingCode[!is.na(d_follow_on$closingCode)] == d_follow_on$followOnCode[!is.na(d_follow_on$followOnCode)])) {
    warning("Follow on and closing mechs cannot be the same!")
    is_valid<-FALSE
  }
  
  #Duplicated closing mechanisms
  if (any(duplicated(d_follow_on$closingCode)) ) {
    dups<-d_follow_on$closingCode[duplicated(d_follow_on$closingCode)]
    msg <-
      paste0(
        "Duplicated closing mechanisms were found in the Follow-on Mechs sheet! ",
        paste(dups, sep = "", collapse = ","))
    warning(msg)
    is_valid<-FALSE
  }
  #Duplicated follow on mechs
  if (any(duplicated(d_follow_on$followOnCode)) ) {
    dups<-d_follow_on$followOnCode[duplicated(d_follow_on$followOnCode)]
    msg <-
      paste0(
        "Duplicated follow-on mechanisms were found in the Follow-on Mechs sheet! ",
        paste(dups, sep = "", collapse = ","))
    warning(msg)
    is_valid<-FALSE
  }
  #Non-existent closing mechs
  mechs_wanted <- readRDS(paste0(d$wb_info$support_files_path, "mech_list.rda")) %>%
    dplyr::filter(ou == d$wb_info$ou_name) %>%
    dplyr::pull(code)
  
  closing_got<-unique(d_follow_on$closingCode)
  closing_not_valid_mechs<-closing_got[!(closing_got %in% mechs_wanted)]
  
  if (length(closing_not_valid_mechs)>0) {
    msg<-paste0(
      "Invalid closing mechs were found in the Follow-on Mechs sheet! ",
      paste(closing_not_valid_mechs,sep="",collapse=","))
    warning(msg)
    is_valid<-FALSE
  }
  
  follow_on_got<-unique(d_follow_on$followOnCode)
  follow_on_not_valid_mechs<-follow_on_got[!(follow_on_got %in% mechs_wanted)]
  
  if (length(follow_on_not_valid_mechs)>0) {
    msg<-paste0(
      "Invalid follow-on mechs were found in the Follow-on Mechs sheet! ",
      paste(follow_on_not_valid_mechs,sep="",collapse=","))
    warning(msg)
    is_valid<-FALSE
  }
  
  return(is_valid)
  
}

#' @export
#' @title ImportFollowOnMechs(d)
#'
#' @description Imports the follow on mechs sheet.
#' @param d  Object from import sheets
#' @return An data object with a follow on mechs sheet object. NULL if blank. 
#' If this sheet is blank, returns NULL.

ImportFollowOnMechs <- function(d) {
  sheet_name <- NULL
  if (d$wb_info$wb_type != "NORMAL") {
    stop("Only Normal Disagg tools with follow on mechs are supported!")
  }
  
  sheet_to_import <- "Follow on Mech List"
  schema <- rlist::list.find(d$schemas$schema, sheet_name == sheet_to_import)[[1]]

  cell_range <- readxl::cell_limits(
    c(schema$row, schema$start_col),
    c(NA, schema$end_col)
  )

  d_follow_on <-
    readxl::read_excel(
      d$wb_info$wb_path,
      sheet = sheet_to_import,
      range = cell_range,
      col_types = "text"
    )
  names(d_follow_on)<-c("closingCode","followOnCode","notes")
  #Bail early if there is nothing here  
  if (NROW(d_follow_on) == 0) {  return(d) }
  
  is_valid<-validate_follow_on_mechs_sheet(d_follow_on,d)
  
  if (is_valid) {
    d$follow_on_mechs<-d_follow_on
  } 
  
  return(d)
  
}


