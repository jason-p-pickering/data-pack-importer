library(readxl)
library(rlist)
require(jsonlite)

ProduceSchema <-
  function(row = 6,
           start_col = 3,
           end_col = 1000,
           sheet_name,
           sheet_path) {
    
    if (sheet_name == "Follow on Mech List") {
      
      foo <-
        list(
          sheet = "Follow on Mech List",
          row = 4,
          start_col = 3,
          end_col = 5,
          method ='skip',
          fields = as.list(c("Closing Out","Follow on","Notes")))
      
    } else if (sheet_name == 'Allocation by SNUxIM') {
      row=6
      start_col = 2
      end_col = 232
      sheet=sheet_name
      foo<-list(sheet=sheet,
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
      sheet=sheet_name
      foo<-list(sheet=sheet,
                row=row,
                start_col = start_col,
                end_col = end_col,
                method = 'impatt',
                fields = as.list(c("psnu","psnuuid","snu_priotization_fy19","plhiv_fy19")))
    } else {  foo <-
      list(
        sheet = sheet_name,
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
  
  url<-"https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv"
  d<-read.csv(url,stringsAsFactors = FALSE)
  return(d[,c("code","uid")])
}


processDataElements<-function() {
  read.csv("data-raw/DataPackCodes.csv",stringsAsFactors = FALSE,na="") %>%
  dplyr::select(.,code=DataPackCode,combi=pd_2019_P) %>% 
    dplyr::filter(.,complete.cases(.))
  }



sheet_path = "data-raw/KenyaCOP18DisaggTool_HTSv2018.01.26.xlsx"
mode="HTS"
hts_schema<-produceSchemas(sheet_path,mode)

sheet_path = "data-raw/KenyaCOP18DisaggToolv2018.01.26.xlsx"
mode="NORMAL"
main_schema<-produceSchemas(sheet_path,mode)

schemas<-list(hts=hts_schema,normal=main_schema)
names(schemas)<-c("hts","normal")

mechs<-processMechs()
des<-processDataElements()

impatt<-fromJSON("data-raw/impatt_option_set.json")

devtools::use_data(hts_schema,main_schema,mechs,des,impatt,internal = TRUE,overwrite = TRUE)