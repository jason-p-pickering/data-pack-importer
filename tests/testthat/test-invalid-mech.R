
context("invalid_mechanism_psnu_data")

generate_invalid_mech_data <- function() {
  template_copy <- paste0(tempfile(), ".xlsx")
  file.copy(from = test_sheet("COP18DisaggToolTemplate_5304cdb.xlsx"), to = template_copy)
  wb <- openxlsx::loadWorkbook(template_copy)
  openxlsx::writeData(wb = wb, sheet = "Home", x = "Botswana", xy = c(15, 1))
  openxlsx::writeData(wb = wb, sheet = "Home", x = "normal", xy = c(15, 3))
  openxlsx::writeData(wb = wb, sheet = "Home", x = "l1KFEXKI4Dg", xy = c(15, 4))
  # PSNU name
  openxlsx::writeData(wb = wb, sheet = "GEND_GBV", x = "Greater Gabarone Cluster", xy = c(3, 7))
  # PSNU UID
  openxlsx::writeData(wb = wb, sheet = "GEND_GBV", x = "Y6TnOG79VvP", xy = c(4, 7))
  # Priority
  openxlsx::writeData(wb = wb, sheet = "GEND_GBV", x = "NOT DEFINED", xy = c(5, 7))
  # Non-existent mech ID
  openxlsx::writeData(wb = wb, sheet = "GEND_GBV", x = "7212", xy = c(6, 7))
  # Mechname
  openxlsx::writeData(wb = wb, sheet = "GEND_GBV", x = "Foo mech", xy = c(7, 7))
  # DE Type
  openxlsx::writeData(wb = wb, sheet = "GEND_GBV", x = "DSD", xy = c(8, 7))
  # OU type
  openxlsx::writeData(wb = wb, sheet = "GEND_GBV", x = "Foo type", xy = c(9, 7))
  # D_gend_gbv_fy19
  openxlsx::writeData(wb = wb, sheet = "GEND_GBV", x = 100, xy = c(48, 7))
  openxlsx::saveWorkbook(wb = wb, file = template_copy, overwrite = TRUE)
  return(template_copy)
}

test_that("can fail bad mechanisms in psnu data", {
  support_files <- test_support_files_directory()
  distribution_method <- 2017
  # Confirm that the mechs do not exist in our test file
  test_mechs <- readRDS(paste0(support_files, "mech_list.rda"))
  expect_equal(test_mechs$code %in% "7212", FALSE)

  template_copy <- generate_invalid_mech_data()
  # Warning is expdected here because no IMPATT Table
  expect_error(
    d <- ImportSheets(template_copy, support_files = support_files, distribution_method),
    "The following mechanisms in sheet GEND_GBV were invalid:7212"
  )
  unlink(template_copy)
})