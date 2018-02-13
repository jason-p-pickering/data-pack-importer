library(readxl)
library(rlist)
require(jsonlite)
require(datimvalidation)


ProduceSchema <-
  function(row = 6,
           start_col = 3,
           end_col = 1000,
           sheet_name,
           sheet_path) {
    
    if (sheet_name == "Follow on Mech List") {
      
      foo <-
        list(
          sheet_name = "Follow on Mech List",
          row = 4,
          start_col = 3,
          end_col = 5,
          method ='skip',
          fields = as.list(c("Closing Out","Follow on","Notes")))
      
    } else if (sheet_name == 'Allocation by SNUxIM') {
      row=6
      start_col = 2
      end_col = 232
      sheet_name=sheet_name
      foo<-list(sheet_name=sheet_name,
                row=row,
                start_col = start_col,
                end_col = end_col,
                method = 'skip',
      fields = as.list(names(as.list(
        read_excel(
          path = sheet_path,
          sheet = sheet_name,
          range = cell_limits(c(row, start_col),
                              c(row, end_col)))))))
    } else if (sheet_name == "IMPATT Table") {
      row=6
      start_col = 3
      end_col = 6
      sheet_name=sheet_name
      foo<-list(sheet_name=sheet_name,
                row=row,
                start_col = start_col,
                end_col = end_col,
                method = 'impatt',
                fields = as.list(c("psnu","psnuuid","snu_priotization_fy19","plhiv_fy19")))
    } else {  foo <-
      list(
        sheet_name = sheet_name,
        row = row,
        start_col = start_col,
        end_col = end_col,
        method = 'standard',
        fields = as.list(names(as.list(
          read_excel(
            path = sheet_path,
            sheet = sheet_name,
            range = cell_limits(c(row, start_col),
                                c(row, end_col))
          )
        )))
      )
    #Remove any unnamed columns
    foo$fields <- foo$fields[!grepl("X_", foo$fields)]
    foo$end_col = start_col + length(foo$fields)-1
    
        }
    return(foo)
  }

produceSchemas <- function(sheet_path,mode) {
  
  sheets <- excel_sheets(sheet_path)
  #Exclude these two , as they are custom
  custom_sheets<-c("Home")
  sheets <-sheets[!(sheets %in% custom_sheets)]
  foo <- list()
  for (i in 1:length(sheets)) {
    bar <- ProduceSchema(sheet_path = sheet_path, sheet_name = sheets[i])
    foo <- list.append(foo, bar)
  }
  return(list(mode=mode,schema=foo))
}


processMechs<-function() {
  
  url<-paste0(getOption("baseurl"),"api/sqlViews/fgUtV6e9YIX/data.csv")
  d<-read.csv(url,stringsAsFactors = FALSE)
  return(d[,c("code","uid")])
}


processDataElements<-function() {
  read.csv("data-raw/DataPackCodes.csv",stringsAsFactors = FALSE,na="") %>%
  dplyr::select(.,code=DataPackCode,combi=pd_2019_P) %>% 
    dplyr::filter(.,complete.cases(.))
  }

## COPdataElements
## TODO: Move this to one of the standard views so we 
## do not need to login to DATIM

GenerateCOPDataElements <- function() {
  
  getCOPDataElements <- function(dataset_uid,period,level) {
    de <- read.csv(url(paste0(getOption("baseurl"),"api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:",dataset_uid)),stringsAsFactors=FALSE,header=TRUE)
    de$period <- period
    de$level <- level
    de$pdLvl <- paste("pd",period,level,sep="_",collapse=NULL)
    de$indicator <- str_trim(str_extract(str_replace_all(de$dataelement,"(?<=IMPATT)\\.","_"),"^\\w+\\s"))
    return(de)
  }
  
  dataset_config<-data.frame(
  dataset = c(
    #2019 Targets
    #(Facility,       Community,        PSNU)
    "eyI0UOWJnDk",    "l796jk9SW7q",    "msJImKkTJkV",
    #IMPATT
    "pTuDWXzkAkJ",
    #2018 Targets
    #(Facility,       Community)
    "AitXBHsC7RA",    "BuRoS9i851o",
    #2017Q4
    #(Facility,       Community)
    "uTvHPA1zqzi",    "O3VMNi8EZSV"),
  period = c(rep("19_T",3),"IMPT",rep(c("18_T","17_4"), each=2)),
  level = c("F","C","P","P",rep(c("F","C"),2)))
  
  COPdataElements=NULL
  for (i in 1:length(dataset_config)) {
    COPdataElements=rbind(COPdataElements,getCOPDataElements(dataset_config$dataset[i],dataset_config$period[i],dataset_config$level[i]))
  }
  
  #TEMPORARY: Code List Fix --> add four lines for PMTCT_ART in PSNU code list which are currently not appearing in sqlView.
  COPdataElements <- COPdataElements %>%
    rbind(c("MER Target Setting: PSNU (Facility and Community Combined)","PMTCT_ART (N, DSD, NewExistingArt/Sex/HIVStatus) T_PSNU: ART","PMTCT_ART (N, DSD, NewExistingArt/Sex/HIV) T_PSNU","PMTCT_ART_N_DSD_NewExistingArt_Sex_HIV_T_PSNU","DFdm1fjKS5u","Number of HIV-positive pregnant women who received ART to reduce risk of mother-to-child-transmission during pregnancy.","Life-long ART, New, Female, Positive","Q2EBeMBa8Ga","Q2EBeMBa8Ga","19_T","P","pd_19_T_P","PMTCT_ART")
          ,c("MER Target Setting: PSNU (Facility and Community Combined)","PMTCT_ART (N, DSD, NewExistingArt/Sex/HIVStatus) T_PSNU: ART","PMTCT_ART (N, DSD, NewExistingArt/Sex/HIV) T_PSNU","PMTCT_ART_N_DSD_NewExistingArt_Sex_HIV_T_PSNU","DFdm1fjKS5u","Number of HIV-positive pregnant women who received ART to reduce risk of mother-to-child-transmission during pregnancy.","Life-long ART, Already, Female, Positive","RTYO8ycjbCt","RTYO8ycjbCt","19_T","P","pd_19_T_P","PMTCT_ART")
          ,c("MER Target Setting: PSNU (Facility and Community Combined)","PMTCT_ART (N, TA, NewExistingArt/Sex/HIVStatus) T_PSNU: ART", "PMTCT_ART (N, TA, NewExistingArt/Sex/HIV) T_PSNU", "PMTCT_ART_N_TA_NewExistingArt_Sex_HIV_T_PSNU", "w4pjh8fNZx8","Number of HIV-positive pregnant women who received ART to reduce risk of mother-to-child-transmission during pregnancy.","Life-long ART, New, Female, Positive","Q2EBeMBa8Ga","Q2EBeMBa8Ga","19_T","P","pd_19_T_P","PMTCT_ART")
          ,c("MER Target Setting: PSNU (Facility and Community Combined)","PMTCT_ART (N, TA, NewExistingArt/Sex/HIVStatus) T_PSNU: ART", "PMTCT_ART (N, TA, NewExistingArt/Sex/HIV) T_PSNU", "PMTCT_ART_N_TA_NewExistingArt_Sex_HIV_T_PSNU", "w4pjh8fNZx8","Number of HIV-positive pregnant women who received ART to reduce risk of mother-to-child-transmission during pregnancy.","Life-long ART, Already, Female, Positive","RTYO8ycjbCt","RTYO8ycjbCt","19_T","P","pd_19_T_P","PMTCT_ART")
    ) %>%
    unique() %>% 
    subset(.,COPdataElements$indicator %in% c("TX_RET","TX_NEW","TX_CURR","TX_PVLS","PMTCT_STAT","PMTCT_ART","PMTCT_EID","TB_STAT","TB_ART","TB_PREV","TX_TB","HTS_TST","VMMC_CIRC","HTS_SELF","OVC_SERV","OVC_HIVSTAT","KP_PREV","PP_PREV","KP_MAT","GEND_GBV","GEND_GBV_PEP","PrEP_NEW","IMPATT_PLHIV"))

  return(COPdataElements)
  
}



##Procedural logic to generate the actual schemas
sheet_path = "data-raw/MalawiCOP18DisaggTool_HTSv2018.02.10.xlsx"
mode="HTS"
hts_schema<-produceSchemas(sheet_path,mode)

sheet_path = "data-raw/MalawiCOP18DisaggToolv2018.02.10.xlsx"
mode="NORMAL"
main_schema<-produceSchemas(sheet_path,mode)

schemas<-list(hts=hts_schema,normal=main_schema)
names(schemas)<-c("hts","normal")

mechs<-processMechs()
des<-processDataElements()
impatt<-fromJSON("data-raw/impatt_option_set.json")

loadSecrets("/home/jason/.secrets/datim.json")
COPdataElements<-GenerateCOPDataElements()


devtools::use_data(hts_schema,main_schema,mechs,des,impatt,COPdataElements, internal = TRUE,overwrite = TRUE)