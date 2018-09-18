
context("roll_up_hts_data")

test_that("can roll up clustered hts data", {
  
  support_files <-  test_support_files_directory()
  distribution_method<-2017
  
  value_a<-list(dataelement = "i79NVuApSrF", # HTS emergency ward
                period="2018Oct",
                orgunit="Y6TnOG79VvP",
                categoryoptioncombo="SYFxsQKDZB6", # Male, 30-34 Pos
                attributeoptioncombo="BooXMSFBYBU",
                value="200"
  )
  value_b<-list(dataelement = "i79NVuApSrF", # HTS emergency ward
                period="2018Oct",
                orgunit="Y6TnOG79VvP",
                categoryoptioncombo="HEpqnVEHzUA", # Female positive
                attributeoptioncombo="BooXMSFBYBU",
                value="300")
  d<-list()
  d$wb_info$wb_path<-tempfile()
  d$wb_info$wb_type<-"HTS"
  d$wb_info$is_clustered<-TRUE
  d$wb_info$ou_name<-"Botswana"
  d$wb_info$ou_uid<-"l1KFEXKI4Dg"
  d$wb_info$support_files_path<-test_support_files_directory()
  d$wb_info$distribution_method<-2017
  d$data<-rbind.data.frame(value_a,value_b,stringsAsFactors = FALSE)
  
  d<-prepare_export_to_datim(d)
  #Should include HTS_TST (N, DSD) T_PSNU: HTS received result
  expect_equivalent(unique(d$data$dataelement),c("i79NVuApSrF","bCdPl0retrn"))
  expect_equal(sum(as.numeric(d$data[d$data$dataelement=="bCdPl0retrn","value"])),sum(as.numeric(d$data[d$data$dataelement=="i79NVuApSrF","value"])))
  
})

test_that("can roll up non-clustered normal psnu data", {

  value_a<-list(dataelement = "ajV9fIG2L0S", # GEND_GBV
                period="2018Oct",
                orgunit="lRPR9ANGlm4", #Chadize
                categoryoptioncombo="GGtPtwWGpuU",
                attributeoptioncombo="BooXMSFBYBU",
                value="200")
  value_b<-list(dataelement = "ajV9fIG2L0S", # GEND_GBV
                period="2018Oct",
                orgunit="uZ7ineguntq", #Chama
                categoryoptioncombo="GGtPtwWGpuU",
                attributeoptioncombo="BooXMSFBYBU",
                value="200")
  
  d<-list()
  d$wb_info$wb_path<-tempfile()
  d$wb_info$wb_type<-"NORMAL"
  d$wb_info$is_clustered<-FALSE
  d$wb_info$ou_name<-"Zambia"
  d$wb_info$ou_uid<-"f5RoebaDLMx"
  d$wb_info$support_files_path<-test_support_files_directory()
  d$wb_info$distribution_method<-2017
  d$data<-rbind.data.frame(value_a,value_b,stringsAsFactors = FALSE)
  
  d<-prepare_export_to_datim(d)
  #Should include HTS_TST (N, DSD) T_PSNU: HTS received result
  expect_equivalent(unique(d$data$dataelement),c("ajV9fIG2L0S"))
  expect_equal(sum(as.numeric(d$data[,"value"])), 400)
  
})

test_that("can roll up non-clustered  psnu hts data", {
  
  value_a<-list(dataelement = "i79NVuApSrF", # HTS emergency ward
                period="2018Oct",
                orgunit="lRPR9ANGlm4",
                categoryoptioncombo="SYFxsQKDZB6", # Male, 30-34 Pos
                attributeoptioncombo="BooXMSFBYBU",
                value="200"
  )
  value_b<-list(dataelement = "i79NVuApSrF", # HTS emergency ward
                period="2018Oct",
                orgunit="lRPR9ANGlm4",
                categoryoptioncombo="HEpqnVEHzUA", # Female positive
                attributeoptioncombo="BooXMSFBYBU",
                value="300")
  d<-list()
  d$wb_info$wb_path<-tempfile()
  d$wb_info$wb_type<-"HTS"
  d$wb_info$is_clustered<-TRUE
  d$wb_info$ou_name<-"Zambia"
  d$wb_info$ou_uid<-"f5RoebaDLMx"
  d$wb_info$support_files_path<-test_support_files_directory()
  d$wb_info$distribution_method<-2017
  d$data<-rbind.data.frame(value_a,value_b,stringsAsFactors = FALSE)
  
  d<-prepare_export_to_datim(d)
  #Should include HTS_TST (N, DSD) T_PSNU: HTS received result
  expect_equivalent(unique(d$data$dataelement),c("i79NVuApSrF","bCdPl0retrn"))
  expect_equal(sum(as.numeric(d$data[d$data$dataelement=="bCdPl0retrn","value"])),sum(as.numeric(d$data[d$data$dataelement=="i79NVuApSrF","value"])))
  
})


test_that("can exclude hts keypops data", {
  
  support_files<-test_support_files_directory()
  
  distribution_method<-2017
  
  value_a<-list(dataelement = "i79NVuApSrF", # HTS emergency ward
                period="2018Oct",
                orgunit="Y6TnOG79VvP",
                categoryoptioncombo="SYFxsQKDZB6", # Male, 30-34 Pos
                attributeoptioncombo="BooXMSFBYBU",
                value="200"
  )
  value_b<-list(dataelement = "n1qR2SPLQxJ", # KeyPop/Resul
                period="2018Oct",
                orgunit="Y6TnOG79VvP",
                categoryoptioncombo="xYyVHiXrvSi", # PWID positive
                attributeoptioncombo="BooXMSFBYBU",
                value="600")
  
  value_c<-list(dataelement = "i79NVuApSrF", # HTS emergency ward
                period="2018Oct",
                orgunit="Y6TnOG79VvP",
                categoryoptioncombo="HEpqnVEHzUA", # Female positive
                attributeoptioncombo="BooXMSFBYBU",
                value="300")
  d<-list()
  d$wb_info$wb_path<-tempfile()
  d$wb_info$wb_type<-"HTS"
  d$wb_info$is_clustered<-TRUE
  d$wb_info$ou_name<-"Botswana"
  d$wb_info$ou_uid<-"l1KFEXKI4Dg"
  d$wb_info$support_files_path<-test_support_files_directory()
  d$wb_info$distribution_method<-2017
  d$data<-rbind.data.frame(value_a,value_b,value_c,stringsAsFactors = FALSE)
  
  d<-prepare_export_to_datim(d)
  #Should include HTS_TST (N, DSD) T_PSNU: HTS received result
  expect_equivalent(unique(d$data$dataelement),c("i79NVuApSrF","n1qR2SPLQxJ","bCdPl0retrn"))
  #Numerator rollup should exclude KepPops
  expect_equal(sum(as.numeric(d$data[d$data$dataelement=="bCdPl0retrn","value"])),500)
  #Kep pops should still be there
  expect_equal(sum(as.numeric(d$data[d$data$dataelement=="n1qR2SPLQxJ","value"])),600)
  #Modalities should still be there
  expect_equal(sum(as.numeric(d$data[d$data$dataelement=="i79NVuApSrF","value"])),500)
  
})