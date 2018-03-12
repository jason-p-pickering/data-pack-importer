context("datasets")

test_that("can load datasets", {
  expect_silent(datapackimporter::clusters)
  expect_silent(datapackimporter::sites_exclude)
  expect_silent(datapackimporter::psnus)
  expect_silent(datapackimporter::militaryUnits)
  expect_silent(datapackimporter::hts_site_schema)
  expect_silent(datapackimporter::main_site_schema)
  expect_silent(datapackimporter::rCOP18deMapT)
  expect_silent(datapackimporter::mechs)
  expect_silent(datapackimporter::impatt)
  expect_silent(datapackimporter::main_schema)
  expect_silent(datapackimporter::hts_schema)
  
  })