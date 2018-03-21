context("hts_worbook_info")

support_files <- test_support_files_directory()
distribution_method<-2017

test_that("can generate workbook info", {

   wb_info_names <-
     c(
       "wb_path",
       "timestamp",
       "wb_type",
       "ou_name",
       "ou_uid",
       "is_clustered",
       "distribution_method",
       "support_files_path")

       x <-
     GetWorkbookInfo(
       test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"),
       support_files = support_files, distribution_method)

expect_type(x, "list")
expect_type(x$wb_info, "list")
expect_type(x$schemas, "list")
expect_named(x, c("wb_info", "schemas"))
expect_named(x$wb_info, wb_info_names)
expect_equal(x$wb_info$wb_path, test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"))
expect_is(as.POSIXlt(x$wb_info$timestamp), "POSIXlt")
expect_equal(x$wb_info$wb_type, "NORMAL")
expect_equal(x$wb_info$ou_name, "OU")
expect_equal(x$wb_info$ou_uid, "X__1")
expect_equal(x$wb_info$is_clustered, FALSE)
expect_equal(x$wb_info$distribution_method, distribution_method)
expect_equal(x$wb_info$support_files_path, support_files)

})

test_that("can validate good NORMAL template" , {

  
  template_copy=paste0(tempfile(),".xlsx")
  file.copy(from = test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"), to=template_copy)
  wb = openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
  openxlsx::writeData(wb = wb,sheet="Home", x="normal",xy = c(15,3))
  openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
  openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
  
  expect_silent(d<-ValidateWorkbook(template_copy, support_files=support_files, distribution_method)) 
  expect_equal(d$wb_info$wb_path,expected = template_copy)
  expect_equal(d$wb_info$wb_type , "NORMAL")
  expect_equal(d$wb_info$ou_name, "Botswana")
  expect_equal(d$wb_info$ou_uid,"l1KFEXKI4Dg")
  expect_is(as.POSIXlt(d$wb_info$timestamp), "POSIXlt")
  expect_equal(d$wb_info$is_clustered,TRUE)
  expect_equal(d$wb_info$distribution_method,distribution_method)
  expect_equal(d$wb_info$support_files_path,support_files)
  expect_equal(d$schemas$mode,"NORMAL")
  expect_equivalent(d$schemas$schema,datapackimporter::main_schema$schema)
  unlink(template_copy)
  })
   
 test_that("can fail with missing sheet" , {
   

   template_copy=paste0(tempfile(),".xlsx")
   file.copy(from = test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"), to=template_copy)
   wb = openxlsx::loadWorkbook(template_copy)
   openxlsx::removeWorksheet(wb = wb, sheet = "Home")
   openxlsx::saveWorkbook(wb,file=template_copy,overwrite = TRUE)
  expect_error(ValidateWorkbook(wb,
                 support_files=support_files, distribution_method))
  unlink(template_copy)})

 test_that("can pass with extra sheet" , {

   template_copy=paste0(tempfile(),".xlsx")
   wb = openxlsx::loadWorkbook(test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"))
   openxlsx::addWorksheet(wb,sheetName = "ExtraSheet")
   openxlsx::writeData(wb = wb,sheet="ExtraSheet", x="Botswana",xy = c(1,1))
   openxlsx::saveWorkbook(wb,file = template_copy,overwrite = TRUE)
   expect_silent(ValidateWorkbook(template_copy,
                                 support_files=support_files, distribution_method))
   unlink(template_copy)})
 
 
 test_that("can fail with modified sheet" , { 
   
   
   template_copy=paste0(tempfile(),".xlsx")
   wb = openxlsx::loadWorkbook(test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"))
   openxlsx::removeTable(wb,"GEND_GBV","gend_gbv_T")
   openxlsx::writeData(wb = wb,sheet="GEND_GBV", x="foo",xy = c(48,6))
   openxlsx::saveWorkbook(wb,file = template_copy,overwrite = TRUE)
   expect_error(suppressWarnings(ValidateWorkbook(template_copy,
                                  support_files=support_files, distribution_method)))
   unlink(template_copy)})
 
 test_that("can fail if not a valid type of tool" , {
   support_files <- test_support_files_directory()
   distribution_method<-2017
   template_copy=paste0(tempfile(),".xlsx")
   file.copy(from = test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"), to=template_copy)
   wb = openxlsx::loadWorkbook(template_copy)
   openxlsx::writeData(wb = wb,sheet="Home", x="Botswana",xy = c(15,1))
   openxlsx::writeData(wb = wb,sheet="Home", x="",xy = c(15,3))
   openxlsx::writeData(wb = wb,sheet="Home", x="l1KFEXKI4Dg",xy = c(15,4))
   openxlsx::saveWorkbook(wb = wb,file = template_copy,overwrite = TRUE)
   expect_error(ValidateWorkbook(template_copy,
                    support_files=support_files, distribution_method),
                "Are you sure this is a disagg or site review tool?")
   unlink(template_copy)})
 