#' @export
#' @title DataPackR(wb_path,distribution_method, support_files_path
#'
#' @description Process DataPack Disagg and Site tools
#' @param wb_path Path to the DataPack file to process
#' @param distribution_method Should be either 2017 or 2018
#' @param support_file_path Path to the support files
#'
#'
DataPackR <- function(wb_path, distribution_method, support_files_path) {
  parsed_data<-
    ImportSheets(wb_path,
                 distribution_method = distribution_method,
                 support_files_path = support_files_path)
  if (parsed_data$wb_info$wb_type %in% c("NORMAL","HTS")) {
    print("Found a disagg tool!")
    prepare_export_to_datim(parsed_data)
    #Distribute the site level data
    site_data<-distributeSite(parsed_data)
    #Create the site level tool
    export_site_level_tool(site_data)
  } else if (parsed_data$wb_info$wb_type %in% c("NORMAL_SITE","HTS_SITE")) {
    print("Found a site level tool!")
    prepare_export_to_datim(parsed_data)
  } else {
    stop("Looks like a bad DataPack!")
  }


}