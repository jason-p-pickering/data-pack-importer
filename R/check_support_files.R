#' @export
#' @importFrom compare compare
#' @title check_support_files(support_files_path)
#'
#' @description Checks DataPack support files to ensure they are compatible
#' with this version of the code. 
#' @param support_files_path  The absolute file path to support files directory.
#' @return Returns a list of files which are not correct. 
#' 
check_support_files<-function(support_files_path) {
  
  want_md5s<-list("distrClusterFY17.rda"="ad996b7e0c7c175805776f696519bcac",
                  "distrClusterFY18.rda"="452f33618ae607f9dc4f1ffe034fddd7",
                  "distrSiteFY17.rda"="1901a2988c1154ffbf1abe448b53e195",
                  "distrSiteFY18.rda"="2571cce803e13c0a32541a3543c12f7b",
                  "mech_list.rda" = "fd6f7ad8f8fc9b9bdfd6a280778ef64f",
                  "ous_list.rda"= "13c3a6970f8bdabad7bf482219a2ad0a",
                  "siteExport.rda" = "6cce458fb120fa8f4a69edc3e89800e1",
                  "SiteLevelReview_HTS_TEMPLATE.xlsx" =  "4c346d0d600343197b882c2efa4b6587",
                  "SiteLevelReview_TEMPLATE.xlsx" = "314fcd7af68e5db03320ff55ad663552")
  
  support_files<-paste0(support_files_path,names(want_md5s))
  foo<-as.list(tools::md5sum(support_files))
  names(foo)<-basename(names(foo))
  not_good<-compare(want_md5s,foo)
  if (!not_good$result) {
    print(not_good$detailedResult)
    stop("Please ensure your support files are present and up to date!")
  } else {
    print("Support files look good.")
  }
}
