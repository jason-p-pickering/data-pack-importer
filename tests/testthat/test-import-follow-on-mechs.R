
context("import_follow_on_mechs")




test_that("can import follow on mechs", {

  support_files <- test_support_files_directory()
  distribution_method<-2017
  
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="normal",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  
  sheet_name<-"Follow on Mech List"
  openxlsx::writeData(wb,sheet = sheet_name,x="11111",xy=c(3,5))
  openxlsx::writeData(wb,sheet = sheet_name,x="99999",xy=c(4,5))
  openxlsx::writeData(wb,sheet = sheet_name,x="Just a note",xy=c(5,5))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  expect_warning(d<-ImportSheets(template_copy, support_files=support_files, distribution_method))
  
  expect_equal(d$follow_on_mechs$`Closing Out`,"11111")
  expect_equal(d$follow_on_mechs$`Follow on`,"99999")
  expect_equal(d$follow_on_mechs$`Notes`,"Just a note")
  unlink(template_copy)
  })