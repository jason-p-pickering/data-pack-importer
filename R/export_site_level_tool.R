#' @export
#' @title write_site_level_sheet(wb,schema,df)
#'
#' @description Validates the layout of all relevant sheets in a data pack workbook
#' @param wb_path Workbook object
#' @param schema Schema object for this sheet
#' @param df Data frame object 

write_site_level_sheet <- function(wb,schema,df) {

  #Is this always true??
  fields<-unlist(schema$fields)[-c(1:4)]
  #Filter  out this indicator
  df_indicator<- df$data_prepared %>% 
    dplyr::filter(match_code %in% fields) %>%
    na.omit()
  
  if (NROW(df_indicator) > 0){
    #Create the styling for the main data table
    s <- openxlsx::createStyle(numFmt = "#,##0;-#,##0;;")
    #Dont error even if the table does not exist
    foo <- tryCatch( {openxlsx::removeTable(wb,schema$sheet_name,schema$sheet_name)},
                     error = function(err) {},
                     finally = {} )  
    
    #Subtotal fomulas
    subtotal_formula_columns<-seq(from=0,to=(length(fields)-1),by=1) + 5 
    subtotal_fomulas<-paste0('=SUBTOTAL(109,INDIRECT($B$1&"["&',openxlsx::int2col(subtotal_formula_columns),'6&"]"))')
    for (i in 1:(length(subtotal_fomulas))) {
    openxlsx::writeFormula(wb, schema$sheet_name,subtotal_fomulas[i],xy = c(i+4, 5))
    }
    
    #Create the OU level summary

    sums<- df$sums %>% 
      dplyr::select(match_code,value) %>%
      filter(match_code %in% fields) %>%
      dplyr::mutate(match_code=factor(match_code,levels = fields)) %>%
      tidyr::spread(match_code,value,drop=FALSE)
      
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
    
    #Spread the data, being sure not to drop any levels
    df_indicator<-df_indicator %>% 
      dplyr::mutate(match_code=factor(match_code,levels = fields)) %>%
      tidyr::spread(match_code,value,drop=FALSE)
    
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
        '<>"",INDEX(site_list[Inactive],MATCH(',
        schema$sheet_name,
        '!$B',formula_cell_numbers,
        ',site_list[siteID],0)+1)=1),"!!","")')
        
   # inactiveFormula<-paste0("IF(AND(",schema$sheet_name,"!$B",7:((NROW(df_indicator)+6)*3),"<>\"\",INDEX(SiteList!$B:$B,MATCH(",schema$sheet_name,"!$B",7:(NROW(df_indicator)+6),",SiteList,0)+1)=1),\"!!\",\"\")")
    openxlsx::writeFormula(wb,schema$sheet_name,inactiveFormula,xy=c(1,7))
    openxlsx::dataValidation(wb,schema$sheet_name,cols=2,rows=7:5000,"list",value="site_list")
    openxlsx::dataValidation(wb,schema$sheet_name,cols=3,rows=7:5000,"list",value="mech_list")
    openxlsx::dataValidation(wb,schema$sheet_name,cols=4,rows=7:5000,"list",value="DSDTA")
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
  
  template_path <- paste0(d$wb_info$support_files_path, template_name)
  
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
  
  #Create the concatenated PSNU > OU_Name (UID) string
  d$sites$name_full <-
    paste0(d$sites$psnu_name,
           " > ",
           d$sites$name,
           " ( ",
           d$sites$organisationunituid,
           " )")
  
  wb <- openxlsx::loadWorkbook(file = template_path)
  sheets<-openxlsx::getSheetNames(template_path)
  openxlsx::sheetVisibility(wb)[which(sheets =="Mechs")]<-"veryHidden"
  
  #Fill in the Homepage details
  
  #OU Hidden
  openxlsx::writeData(
    wb,
    "Home",
    d$wb_info$ou_name,
    xy = c(15, 2),
    colNames = F,
    keepNA = F
  )
  #OU Styled
  openxlsx::writeData(
    wb,
    "Home",
    d$wb_info$ou_name,
    xy = c(2, 26),
    colNames = F,
    keepNA = F
  )
  # #Style both of the sums and formula rows and columns
  # s <-
  #   openxlsx::createStyle(numFmt = "TEXT",
  #                         fontSize = 44,
  #                         fontColour = "#8E271D",
  #                         valign = "center")
  # openxlsx::addStyle(
  #   wb,
  #   "Home",
  #   style = s,
  #   rows = 26,
  #   cols =2,
  #   gridExpand = FALSE
  # )
  # 
  #OU UID
  openxlsx::writeData(
    wb,
    "Home",
    d$wb_info$ou_uid,
    xy = c(15, 3),
    colNames = F,
    keepNA = F
  )
  #Distribution method
  openxlsx::writeData(
    wb,
    "Home",
    paste("Distribution method: ",d$wb_info$distribution_method),
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
  site_list<-data.frame(siteID=d$sites$name_full,Inactive=0)
  openxlsx::writeDataTable(
    wb,
    "SiteList",
    site_list,
    xy = c(1, 1),
    colNames = T,
    keepNA = F,
    tableName = "site_list"
  )
  openxlsx::dataValidation(wb, "SiteList" , col = 2, rows = 1:nrow(site_list), 
                 type = "whole"
                 , operator = "between", value = c(0, 2))
  
  openxlsx::writeDataTable(
    wb,
    "Mechs",
    d$mechanisms$mechanism,
    xy = c(1, 2),
    colNames = F,
    keepNA = F,
    tableName = "mech_list"
  )

  if (d$wb_info$wb_type == "HTS_SITE") {
    schemas <- datapackimporter::hts_site_schema
  }
  if (d$wb_info$wb_type == "NORMAL_SITE") {
    schemas <- datapackimporter::main_site_schema
  }
  
  #Munge the data a bit to get it into shape
  d$data_prepared <- d$data %>% dplyr::mutate(match_code = gsub("_dsd$", "", DataPackCode)) %>%
    dplyr::mutate(match_code = gsub("_ta$", "", match_code)) %>%
    dplyr::left_join(d$mechanisms, by = "attributeoptioncombo") %>%
    dplyr::left_join(d$sites, by = c("orgunit" = "organisationunituid")) %>%
    dplyr::select(name = name_full, mechanism, supportType, match_code, value) %>%
    dplyr::group_by(name, mechanism, supportType, match_code) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE))
    #Duplicates were noted here, but I think this should not have to be done
    #At this point. 
  
  for (i in 1:length(schemas$schema)) {
    schema <- schemas$schema[[i]]
    write_site_level_sheet(wb = wb,
                           schema = schema,
                           d = d)
  }
  openxlsx::saveWorkbook(wb = wb,
                         file = output_file_path,
                         overwrite = TRUE)
  print(paste0("Successfully saved output to ",output_file_path))
}

