
context("import_impatt")




test_that("can import impatt data", {
  
  support_files <- test_support_files_directory()
  distribution_method<-2017
  
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="normal",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  d_impatt<-datapackimporter::psnus[["l1KFEXKI4Dg"]]
  d_impatt$snu_priotization_fy19<-"NOT DEFINED"
  d_impatt$plhiv_fy19<-10
  names(d_impatt)<-c("psnu","psnuuid","snu_priotization_fy19","plhiv_fy19")
  openxlsx::removeTable(wb,sheet = "IMPATT Table", table = "impatt")
  openxlsx::writeDataTable(wb,sheet = "IMPATT Table",x = d_impatt,tableName = "impatt",xy=c(3,6))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  
  
  expect_silent(d<-ImportSheets(template_copy, support_files=support_files, distribution_method))
  expect_equivalent(unique(d$data$dataelement),c("r4zbW3owX9n","rORzrY9rpQ1"))
  expect_equal(all(d$data$period =="2018Oct"),TRUE)
  expect_equal(all(d$data$categoryoptioncombo =="HllvX50cXC0"),TRUE)
  expect_equal(all(d$data$categoryoptioncombo =="HllvX50cXC0"),TRUE)
  expect_equivalent(unique(d$data$orgunit), datapackimporter::psnus[["l1KFEXKI4Dg"]]$id)
  
  })