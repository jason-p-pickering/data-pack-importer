#' @export
#' @title write_site_level_sheet(wb,schema,df)
#'
#' @description Validates the layout of all relevant sheets in a data pack workbook
#' @param wb Workbook object
#' @param schema Schema object for this sheet
#' @param d Data frame object 

write_site_level_sheet <- function(wb,schema,d) {
  
  #Is this always true??
  fields <- unlist(schema$fields)[-c(1:4)]
  #Create the styling for the main data table
  s <- openxlsx::createStyle(numFmt = "#,##0;-#,##0;;")
  #Create the OU level summary
  
  sums <- d$sums %>%
    dplyr::filter(match_code %in% fields) %>%
    dplyr::mutate(match_code = factor(match_code, levels = fields)) %>%
    tidyr::spread(match_code, value, drop = FALSE)
  
  if (NROW(sums) == 1) {
    
    openxlsx::writeData(
      wb,
      sheet = schema$sheet_name,
      sums,
      xy = c(5, 4),
      colNames = F,
      keepNA = F
    )
    
    #Style both of the sums and formula rows and columns
    openxlsx::addStyle(
      wb,
      schema$sheet_name,
      style = s,
      rows = 4:5,
      cols = 5:(length(fields) + 5),
      gridExpand = TRUE
    )
    
    #Subtotal fomulas
    subtotal_formula_columns <-
      seq(from = 0,
          to = (length(fields) - 1),
          by = 1) + 5
    subtotal_formula_column_letters <-
      openxlsx::int2col(subtotal_formula_columns)
    subtotal_formulas <-
      paste0('=SUBTOTAL(109,INDIRECT($B$1&"["&',
             subtotal_formula_column_letters,
             '6&"]"))')
    
    #Conditional formatting
    #Create the conditional formatting
    cond_format_formula <- paste0(
      'OR(',
      subtotal_formula_column_letters,
      '5<(0.95*',
      subtotal_formula_column_letters,
      '4),',
      subtotal_formula_column_letters,
      '5>(1.05*',
      subtotal_formula_column_letters,
      '4))'
    )
    
    negStyle <-
      openxlsx::createStyle(fontColour = "#000000", bgFill = "#FFFFFF")
    posStyle <-
      openxlsx::createStyle(fontColour = "#000000", bgFill = "#ffc000")
    
    for (i in 1:(length(subtotal_formulas))) {
      openxlsx::writeFormula(wb, schema$sheet_name, subtotal_formulas[i], xy = c(i + 4, 5))
      openxlsx::conditionalFormatting(
        wb,
        sheet = schema$sheet_name,
        cols = i + 4,
        rows = 5,
        rule = cond_format_formula[i],
        style = posStyle
        
      )
    }
  } else if (NROW(sums) > 1) {
    stop("Unhandled exception in writing column sums to the sheet!")
  } else {
    return(NA)
  }
  
  #Filter  out this indicator
  df_indicator<- d$data_prepared %>% 
    dplyr::filter(match_code %in% fields)
  if(NROW(df_indicator) == 0){
  df_indicator<-data.frame(Site=d$sites$name[1],
  Mechanism=d$mechanisms$mechanism[1],
  Type="DSD",
  match_code=fields,
  value=NA)}
  # 
  if (NROW(df_indicator) > 0){


    #Spread the data, being sure not to drop any levels
    df_indicator<-df_indicator %>%
      dplyr::mutate(match_code=factor(match_code,levels = fields)) %>%
      tidyr::spread(match_code,value,drop=FALSE)

    df_indicator<-df_indicator[rowSums(is.na(df_indicator[,-c(1:3)]))<length(fields),]
    
    #Dont error even if the table does not exist
    foo <- tryCatch( {openxlsx::removeTable(wb,schema$sheet_name,schema$sheet_name)},
                     error = function(err) {},
                     finally = {} )  
  
    #Write the main data table
    openxlsx::writeDataTable(
      wb,
      sheet = schema$sheet_name,
      df_indicator,
      xy = c(2, 6),
      colNames = TRUE,
      keepNA = FALSE,
      tableName = schema$sheet_name
    )
    #Style the data table
    openxlsx::addStyle(
      wb,
      schema$sheet_name,
      style = s,
      rows = 7:(NROW(df_indicator) + 100),
      cols = 5:(length(fields) + 5),
      gridExpand = TRUE
    )
    
    formula_cell_numbers<- ( 1:NROW(df_indicator) ) + 6
    
    inactiveFormula <-
      paste0(
        'IF(AND(',
        schema$sheet_name,
        '!$B',formula_cell_numbers,
        '<>"",INDEX(site_table[Inactive],MATCH(',
        schema$sheet_name,
        '!$B',formula_cell_numbers,
        ',site_table[siteID],0)+1)=1),"!!","")')
     
    openxlsx::writeFormula(wb,schema$sheet_name,inactiveFormula,xy=c(1,7))
    openxlsx::dataValidation(wb,schema$sheet_name,cols=2,rows=(NROW(df_indicator)*2),"list",value="site_list")
    openxlsx::dataValidation(wb,schema$sheet_name,cols=3,rows=(NROW(df_indicator)*2),"list",value="mech_list")
    openxlsx::dataValidation(wb,schema$sheet_name,cols=4,rows=(NROW(df_indicator)*2),"list",value="DSDTA")
  }
  
  
}

#' @export
#' @title export_site_level_tool(d)
#'
#' @description Validates the layout of all relevant sheets in a data pack workbook
#' @param d Object returned from the site level distribution function

export_site_level_tool <- function(d) {
  
  if (d$wb_info$wb_type == "NORMAL_SITE") {
    template_name = "SiteLevelReview_TEMPLATE.xlsx"
  } else if (d$wb_info$wb_type == "HTS_SITE") {
    template_name = "SiteLevelReview_HTS_TEMPLATE.xlsx"
  }
  
  template_path <- paste0(d$wb_info$support_files_path , template_name)
  
  output_file_path <- paste0(
    dirname(d$wb_info$wb_path),
    "/SiteLevelReview_",
    d$wb_info$wb_type,
    "_",
    d$wb_info$ou_name,
    "_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    ".xlsx"
  )

  wb <- openxlsx::loadWorkbook(file = template_path)
  sheets<-openxlsx::getSheetNames(template_path)
  openxlsx::sheetVisibility(wb)[which(sheets =="Mechs")]<-"veryHidden"
  
  #Fill in the Homepage details
  
  #OU Hidden
  openxlsx::writeData(
    wb,
    "Home",
    d$wb_info$ou_name,
    xy = c(15, 1),
    colNames = F,
    keepNA = F
  )
  #OU Name Upper case
  openxlsx::writeFormula(
    wb,
    "Home",
    x="UPPER(O1)",
    d$wb_info$ou_name,
    xy=c(15,2)
  )
  
  #Workbook Type
  openxlsx::writeData(
    wb,
    "Home",
    d$wb_info$wb_type ,
    xy = c(15, 3),
    colNames = F,
    keepNA = F
  )

  
  #OU UID
  openxlsx::writeData(
    wb,
    "Home",
    d$wb_info$ou_uid,
    xy = c(15, 4),
    colNames = F,
    keepNA = F
  )
  #Distribution method
  openxlsx::writeData(
    wb,
    "Home",
    d$wb_info$distribution_method,
    xy = c(15, 5),
    colNames = F,
    keepNA = F
  )
  #Generation timestamp
  openxlsx::writeData(
    wb,
    "Home",
    paste("Generated on:",Sys.time(), "by", rlist::list.extract(as.list(Sys.info()),"user")),
    xy = c(15, 6),
    colNames = F,
    keepNA = F
  )
  #Package version
  openxlsx::writeData(
    wb,
    "Home",
    as.character(packageVersion("datapackimporter")),
    xy = c(15, 7),
    colNames = F,
    keepNA = F
  )
  openxlsx::showGridLines(wb,"Home",showGridLines = FALSE)
  
  #SiteList sheet
  site_list<-data.frame(siteID=d$sites$name,Inactive=0)
  openxlsx::writeDataTable(
    wb,
    "SiteList",
    site_list,
    xy = c(1, 1),
    colNames = T,
    keepNA = F,
    tableName = "site_table"
  )
  openxlsx::createNamedRegion(wb = wb,
                              sheet="SiteList",
                              name="SiteList",
                              rows=1:(nrow(site_list)+1),
                              cols=1)
  openxlsx::dataValidation(
    wb,
    "SiteList" ,
    col = 2,
    rows = 1:nrow(site_list),
    type = "whole",
    operator = "between",
    value = c(0, 2)
  )
  
  openxlsx::writeDataTable(
    wb,
    "Mechs",
    data.frame(mechID=d$mechanisms$mechanism),
    xy = c(1, 2),
    colNames = F,
    keepNA = F,
    tableName = "mech_table"
  )
  openxlsx::createNamedRegion(wb = wb,
                              sheet="Mechs",
                              name="MechList",
                              rows=1:(length(d$mechanisms$mechanism)+1),
                              cols=1)

  
  #Munge the data a bit to get it into shape
  d$data_prepared <- d$data %>% 
    dplyr::mutate(match_code = gsub("_dsd$", "", DataPackCode)) %>%
    dplyr::mutate(match_code = gsub("_ta$", "", match_code)) %>%
    dplyr::left_join(d$mechanisms, by = "attributeoptioncombo") %>%
    dplyr::left_join(d$sites, by = c("orgunit" = "organisationunituid")) %>%
    dplyr::select(name, mechanism, supportType, match_code, value) %>%
    dplyr::group_by(Site=name, Mechanism=mechanism, Type=supportType, match_code) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE))
    #Duplicates were noted here, but I think this should not have to be done
    #At this point. 
  
  for (i in 1:length(d$schemas$schema)) {
    
    write_site_level_sheet(wb = wb,
                           schema = d$schemas$schema[[i]],
                           d = d)
  }
  openxlsx::saveWorkbook(wb = wb,
                         file = output_file_path,
                         overwrite = TRUE)
  print(paste0("Successfully saved output to ",output_file_path))
}

