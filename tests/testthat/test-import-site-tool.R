
context("import_normal_site_tool")

test_that("can warn on missing site name in site tool", {
  
  support_files <- test_support_files_directory()
  distribution_method<-2017
  
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("SiteLevelReview_TEMPLATE.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="NORMAL_SITE",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  #Mechanism name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="70013 - [Placeholder - 70013 Botswana USAID]",xy = c(3,7))
  #Type
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="DSD",xy = c(4,7))
  #Value
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x=100,xy = c(5,7))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  
  expect_warning(d<-ImportSheets(template_copy, support_files=support_files, distribution_method),
                 "Missing rows in detected in sheet GEND_GBV in field `Site`")
  unlink(template_copy)
})

test_that("can warn on missing mechanism in site tool", {
  
  support_files <- test_support_files_directory()
  distribution_method<-2017
  
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("SiteLevelReview_TEMPLATE.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="NORMAL_SITE",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  #Site name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Test District > Test Site A  {Facility} (VdhX8b4RH6U)",xy = c(2,7))
  #Type
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="DSD",xy = c(4,7))
  #Value
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x=100,xy = c(5,7))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  
  expect_warning(d<-ImportSheets(template_copy, support_files=support_files, distribution_method),
                 "Missing rows in detected in sheet GEND_GBV in field `Mechanism`")
  unlink(template_copy)
})

test_that("can warn on missing mechanism in site tool", {
  
  support_files <- test_support_files_directory()
  distribution_method<-2017
  
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("SiteLevelReview_TEMPLATE.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="NORMAL_SITE",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  #Site name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Test District > Test Site A  {Facility} (VdhX8b4RH6U)",xy = c(2,7))
  #Mechanism name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="70013 - [Placeholder - 70013 Botswana USAID]",xy = c(3,7))
  #Type
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="DSD",xy = c(4,7))
  #Value
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x=100,xy = c(5,7))
  #Duplicate
  #Site name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Test District > Test Site B  {Facility} (AdhX8b4RH6U)",xy = c(2,8))
  #Mechanism name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="70013 - [Placeholder - 70013 Botswana USAID]",xy = c(3,8))
  #Type
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="",xy = c(4,8))
  #Value
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x=200,xy = c(5,8))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  
  expect_warning(d<-ImportSheets(template_copy, support_files=support_files, distribution_method),
                 "Missing rows in detected in sheet GEND_GBV in field `Type`")
  unlink(template_copy)
})


test_that("can import a site tool", {
  
  support_files <- test_support_files_directory()
  distribution_method<-2017
  
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("SiteLevelReview_TEMPLATE.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="NORMAL_SITE",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  #Site name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Test District > Test Site A  {Facility} (VdhX8b4RH6U)",xy = c(2,7))
  #Mechanism name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="70013 - [Placeholder - 70013 Botswana USAID]",xy = c(3,7))
  #Type
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="DSD",xy = c(4,7))
  #Value
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x=100,xy = c(5,7))
  #Duplicate
  #Site name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Test District > Test Site B  {Facility} (AdhX8b4RH6U)",xy = c(2,8))
  #Mechanism name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="70013 - [Placeholder - 70013 Botswana USAID]",xy = c(3,8))
  #Type
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="TA",xy = c(4,8))
  #Value
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x=200,xy = c(5,8))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  expect_silent(d<-ImportSheets(template_copy, support_files=support_files, distribution_method))
  expect_equivalent(names(d),c("wb_info","schemas","data"))
  expect_equal(d$data$value[d$data$orgunit =="VdhX8b4RH6U"],"100")
  expect_equal(d$data$value[d$data$orgunit =="AdhX8b4RH6U"],"200")
  expect_equal(all(d$data$period == "2018Oct"),TRUE)
  expect_equal(all(d$data$categoryoptioncombo == "HllvX50cXC0"),TRUE)
  expect_equal(all(d$data$attributeoptioncombo == "BooXMSFBYBU"),TRUE)
  
  unlink(template_copy)
})