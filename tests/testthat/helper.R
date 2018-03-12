## usage:
## test_sheet("blanks.xls")
test_sheet <-
  function(fname)
    rprojroot::find_testthat_root_file("sheets", fname)

test_support_files_directory <-
  function() {
    support_files<-getOption("datapack_support_files")
    
    if (is.null(support_files)){
      test_dir <-
        paste0(rprojroot::find_testthat_root_file(), "/resources/")
  } else {
    test_dir <- support_files
  }
return(test_dir) }


skip_if_no_resource_files <- function() {
  support_files <- "/home/jason/consultancy/datim/datapack/"
  if (!file.exists(support_files)) {
    skip("Support files not available")
  }
}


## once https://github.com/hadley/testthat/commit/c83aba9 is on CRAN
## can use testthat::test_path() for this, i.e.,
#test_sheet <- function(fname) testthat::test_path("sheets", fname)