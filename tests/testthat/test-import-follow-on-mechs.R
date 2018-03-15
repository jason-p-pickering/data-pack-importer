

context("import_follow_on_mechs")

test_that("can warn on bogus follow on mechs", {

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
  expect_warning(d<-ImportSheets(template_copy, support_files=support_files, distribution_method),
                 "Invalid closing mechs were found in the Follow-on Mechs sheet! 11111")
  expect_equal(is.null(d$follow_on_mechs),TRUE)
  unlink(template_copy)
  })

test_that("can warn on missing follow on mechs", {
  
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
  openxlsx::writeData(wb,sheet = sheet_name,x="Just a note",xy=c(5,5))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  expect_warning(d<-ImportSheets(template_copy, support_files=support_files, distribution_method),
                 "Blank mechanisms were found in the Follow-on mechs sheet!")
  expect_equal(is.null(d$follow_on_mechs),TRUE)
  unlink(template_copy)
})

test_that("can warn on duplicated mechanisms", {
  
  support_files <- test_support_files_directory()
  distribution_method<-2017
  
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="normal",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  
  sheet_name<-"Follow on Mech List"
  openxlsx::writeData(wb,sheet = sheet_name,x="70013",xy=c(3,5))
  openxlsx::writeData(wb,sheet = sheet_name,x="70013",xy=c(4,5))
  openxlsx::writeData(wb,sheet = sheet_name,x="Just a note",xy=c(5,5))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  expect_warning(d<-ImportSheets(template_copy, support_files=support_files, distribution_method),
                 "Follow on and closing mechs cannot be the same!")
  expect_equal(is.null(d$follow_on_mechs),TRUE)
  unlink(template_copy)
})

test_that("can produce follow on mechs", {
  
  support_files <- test_support_files_directory()
  distribution_method<-2017
  
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="normal",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  
  sheet_name<-"Follow on Mech List"
  openxlsx::writeData(wb,sheet = sheet_name,x="70013",xy=c(3,5))
  openxlsx::writeData(wb,sheet = sheet_name,x="70014",xy=c(4,5))
  openxlsx::writeData(wb,sheet = sheet_name,x="Just a note",xy=c(5,5))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  expect_warning(d<-ImportSheets(template_copy, support_files=support_files, distribution_method),
                 "Nothing found in the IMPATT sheet!")
  expect_equal(d$follow_on_mechs$closingCode,'70013')
  expect_equal(d$follow_on_mechs$followOnCode,'70014')
  unlink(template_copy)
})