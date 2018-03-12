
context("fail_duplicate_data")

support_files <- test_support_files_directory()
distribution_method<-2017


test_that("can error on duplicate data", {
  
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="normal",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  #PSNU name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Foo PSNU",xy = c(3,7))
  #PSNU UID
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="AFaZlpk6uf6",xy = c(4,7))
  #Priority
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="NOT DEFINED",xy = c(5,7))
  #Mech ID
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="13258",xy = c(6,7))
  #Mechname
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Foo mech",xy = c(7,7))
  #DE Type
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="DSD",xy = c(8,7))
  #OU type
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Foo type",xy = c(9,7))
  #D_gend_gbv_fy19
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x=100,xy = c(48,7))
  #Duplicated data
  #PSNU name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Foo PSNU",xy = c(3,8))
  #PSNU UID
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="AFaZlpk6uf6",xy = c(4,8))
  #Priority
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="NOT DEFINED",xy = c(5,8))
  #Mech ID
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="13258",xy = c(6,8))
  #Mechname
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Foo mech",xy = c(7,8))
  #DE Type
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="DSD",xy = c(8,8))
  #OU type
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Foo type",xy = c(9,8))
  #D_gend_gbv_fy19
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x=200,xy = c(48,8))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  
  expect_error(d<-ImportSheets(template_copy, support_files=support_files, distribution_method)) 
  })