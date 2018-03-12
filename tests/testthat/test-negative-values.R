
context("negative_values")

generate_negative_values<-function() {

  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="normal",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  #PSNU name
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Greater Gabarone Cluster",xy = c(3,7))
  #PSNU UID
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="Y6TnOG79VvP",xy = c(4,7))
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
  openxlsx::writeData(wb = wb,sheet="GEND_GBV", x=-100,xy = c(48,7))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  return(template_copy)
}

test_that("warning on negative values", {
  
support_files <- test_support_files_directory()
distribution_method<-2017

template_copy<-generate_negative_values()
#Warning is expected with negative values
expect_warning(d<-ImportSheets(template_copy, support_files=support_files, distribution_method),
               "Negative values were found in the data in sheet GEND_GBV!") 
expect_equal(d$data$dataelement, "NTzodLPuWgg")
expect_equal(d$data$period,"2018Oct")
expect_equal(d$data$orgunit,"Y6TnOG79VvP")
expect_equal(d$data$categoryoptioncombo,"HllvX50cXC0")
expect_equal(d$data$attributeoptioncombo,"X4RVqWqOwbX")
expect_equal(d$data$value,"-100")

unlink(template_copy)
})

