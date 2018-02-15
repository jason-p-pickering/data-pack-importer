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


##Procedural logic to generate the actual schemas
##HTS Template
sheet_path = "data-raw/MalawiCOP18DisaggTool_HTSv2018.02.10.xlsx"
mode="HTS"
hts_schema<-produceSchemas(sheet_path,mode)

##Normal template
sheet_path = "data-raw/MalawiCOP18DisaggToolv2018.02.10.xlsx"
mode="NORMAL"
main_schema<-produceSchemas(sheet_path,mode)

schemas<-list(hts=hts_schema,normal=main_schema)
names(schemas)<-c("hts","normal")

#List of mechanisms
mechs<-processMechs()
#List of data elements
des<-processDataElements()
#IMPATT option set
impatt<-fromJSON("data-raw/impatt_option_set.json")

loadSecrets("/home/jason/.secrets/datim.json")
source("data-raw/transform_code_lists.R")
rCOP18deMap<-generateCOP18deMap()

#Save the data to sysdata.Rda. Be sure to rebuild the package and commit after this!
devtools::use_data(hts_schema,main_schema,mechs,des,impatt,rCOP18deMap, internal = TRUE,overwrite = TRUE)