library(readxl)
library(rlist)
library(jsonlite)
library(datimvalidation)
library(tidyr)

ProduceSchema <-
  function(row = 6,
           start_col = 3,
           end_col = 1000,
           sheet_name,
           sheet_path,
           method="standard") {
    
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
        method = method ,
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
  custom_sheets<-c("Home","POPrun","POPSubset","POPSubsetAlt","POPsubsetAlt","POPref","template","POPsubset","ESRI_MAPINFO_SHEET")
  sheets <-sheets[!(sheets %in% custom_sheets)]
  foo<-lapply(sheets,function(x) {ProduceSchema(sheet_name=x,sheet_path = sheet_path)})
  return(list(mode=mode,schema=foo))
}

produceSiteToolSchemas <- function(sheet_path,mode) {
  
  sheets <- excel_sheets(sheet_path)
  #Exclude these two , as they are custom
  custom_sheets<-c("SiteList","Mechs","Home")
  sheets <-sheets[!(sheets %in% custom_sheets)]
  foo<-lapply(sheets,function(x) {ProduceSchema(sheet_name=x,sheet_path = sheet_path,start_col = 1, method="site_tool")})
  return(list(mode=mode,schema=foo))
}


processMechs<-function() {
  
  url<-paste0(getOption("baseurl"),"api/sqlViews/fgUtV6e9YIX/data.csv")
  d<-read.csv(url,stringsAsFactors = FALSE)
  return(d[,c("code","uid")])
}


processDataElements<-function() {
  read.csv(paste0(here(),"./data-raw/DataPackCodes.csv"),stringsAsFactors = FALSE,na="") %>%
  dplyr::select(.,code=DataPackCode,combi=pd_2019_P) %>% 
    dplyr::filter(.,complete.cases(.))
  }

getOrganisationUnitGroups <- function() {
  url <-
    paste0(getOption("baseurl"),
           "api/organisationUnitGroups?format=json&paging=false")
  organisationUnitGroups <-
    fromJSON(content(GET(url), "text"), flatten = TRUE)
  organisationUnitGroups <- as.data.frame(organisationUnitGroups)
  names(organisationUnitGroups) <- c("siteTypeUID", "siteType")
  return(organisationUnitGroups)
}


siteToolSchema<-function(wb_path) {
  sheets <- excel_sheets(wb_path)
}

getSiteList <- function(siteType) {
            organisationUnitGroups <- getOrganisationUnitGroups()
            stUID<-organisationUnitGroups[organisationUnitGroups$siteType==siteType,][1]
            url<-paste0(getOption("baseurl"),"api/organisationUnitGroups/",stUID,"?fields=organisationUnits[id],id,name&format=json")
            resp<-fromJSON(content(GET(url),"text"), flatten=TRUE)
            resp<-as.data.frame(resp)
            names(resp)<-c("siteType","siteTypeUID","orgUnit")
            return(resp)
}


##Procedural logic to generate the actual schemas
##PSNU HTS Template
sheet_path = "./data-raw/COP18DisaggToolTemplate_HTS_5304cdb.xlsx"
mode="HTS"
hts_schema<-produceSchemas(sheet_path,mode)

##Normal PSNU template
sheet_path = "./data-raw/COP18DisaggToolTemplate_5304cdb.xlsx"
mode="NORMAL"
main_schema<-produceSchemas(sheet_path,mode)

#Normal Site level  tools
sheet_path="./data-raw/SiteLevelReview_TEMPLATE.xlsx"
mode="NORMAL_SITE"
main_site_schema<-produceSiteToolSchemas(sheet_path,mode)

#Normal HTS Site level  tool
sheet_path="./data-raw/SiteLevelReview_HTS_TEMPLATE.xlsx"
mode="HTS_SITE"
hts_site_schema<-produceSiteToolSchemas(sheet_path,mode)

schemas<-list(hts=hts_schema,normal=main_schema)
names(schemas)<-c("hts","normal")

#List of mechanisms
datimvalidation::loadSecrets(getOption("datim_credentials"))
mechs<-processMechs()
#List of data elements
#des<-processDataElements()
#IMPATT option set
impatt<-fromJSON("./data-raw/impatt_option_set.json")


source("./data-raw/transform_code_lists.R")
rCOP18deMapT<-generateCodeListT()%>% mapDataPackCodes()
rCOP18deMap<-generateCOP18deMap(rCOP18deMapT)


clusters <- 
  read.csv("./data-raw/COP18Clusters.csv",stringsAsFactors=F,header=T) %>%
    mutate(operatingUnitUID=case_when(operatingunit=="Botswana"~"l1KFEXKI4Dg"
                                      ,operatingunit=="Cameroon"~"bQQJe0cC1eD"
                                      ,operatingunit=="Haiti"~"JTypsdEUNPw"
                                      ,operatingunit=="Mozambique"~"h11OyvlPxpJ"
                                      ,operatingunit=="Namibia"~"FFVkaV9Zk1S"
                                      ,operatingunit=="Burundi"~"Qh4XMQJhbk8"
                                      ,TRUE~""))
#Sites to exclude
sites_exclude<-c('fNH1Ny5vXI5', 'Tiqj6KDtx3p', 'BspXUn4c2i0', 'wnFyQ8gWVuP', 'b0WbjlNgwpg', 'Smw76afBRxh', 'TyDdI16aem2', 'u6UHEEYSsrY', 'ZHAEPwL6s87', 'oitze45vmuG', 'imQAg2FmqIi', 'JWb1FJrb6u0', 'oU9JrXHFBwo', 'ZvjmhaNkDJP', 'ph5hfp4TDYa', 'NDGAjm5He3s', 'S0wsB3mH7As', 'WKQumwV8vzz', 'aIl7B0aJZE7', 'EwvYCRwMaj2', 'Zj3QFD5LCN0', 'DWqxLhccQpN', 'FMA01mDjzg9', 'Wt4Ap0dVT0K', 'kTDYtuRlsRJ', 'B2aBYUFKEtP', 'eBMjxJa6Hyo', 'Jn8Dy8Kt8r6', 'BP8kSSf9mVh', 'uM7bKbyQMUb', 'xRNWRGhiL2x', 'CLsTOua0sYz', 'foN7Fc7qqd5', 'Pn5Egy0nEvw', 'ZU5YFwWSAM7', 'ahCpXE5nYKO', 'WQUnNhUravY', 'lSrgJWMVhKP', 'SWMW9b7WMMG', 'LdH3sTixu4G', 'PUWNeEDqKjG', 'kQLMdNG7tOr', 'qjxX1U1zOV9', 'un7KU5UBkTp', 'nMYhhbh463E', 'cugQdSJzIzf', 'Vgz3Af04heg', 'VXhW2lbMHeT', 'o1OrLbuDePL', 'gdWruPti7dW', 'kpLxWaoSWp5', 'GGNlHihWQLS', 'c78scqZGQPc', 'WXCDaZ8ldbb', 'DmpYVwgbt0k', 'kbLOPXlsHH4', 'KabE1XwF8CH', 'sk68oHctZOt', 'boqES0AhYHD', 'ecpaElyx1MZ', 'TDk0oLAqK6H', 'p3n96zLyWoP', 'hF8sLm9vE1U', 't5GdyeN9riy', 'Fu0wZlUnntH', 'TixiR1SsebU', 'u86Kfypb8DG', 'JJJOwYzvDZo', 'Dgi2sUBjGzO', 'e9eJh4Dn286', 'dV6akh4l1Ej', 'I93yMz1rjkQ', 'TVrtknExg0t', 'FL40UCPHJke', 'WxIBVamFcg0', 'BpLP6v9NeWX', 'D7uuBfToHfb', 'ItoS9FGQg24', 'M8Yb2Y9rgNe', 'tBcAME3DNk1', 'jBOH9BBbqEW', 'J9Nmumn9DRc', 'sEJ8peJ3Jz6', 'g0HJxd9XWMy', 'tLcy3vpV6LF', 'QITi8Rd6xV5', 'zrHn3k5oIAT', 'szenMEdV4sF', 'EzzYi29hyNF', 'RJWMt1CU1HW', 'JSmcOMrC6zZ', 'RQykElqy1HR', 'Ae8uPosEFeF', 'NEk0GiXI2SW', 'HSoAojlwB7Q', 'hRq9qYMyBE7', 'Rq9EVeiR0PU', 'OyDnBG2RCgS', 'q3WGbWcjdWf', 'aGQbouk9S3E', 'GMHwNlqPAzS', 'm6eYOfLPzmF', 'lAhBMeGXsvQ', 'zZXWPXydW2S', 'VGVbROfDHWh', 'bMtviLCfDub', 'ZCbh020F2TA', 'cVnfnV5N1w5', 'L6HMMjCf2em', 'U9YejzJibuv', 'ASSntKFP1Ns')

#Map of OUs and PSNUs
ous<-httr::GET(paste0(getOption("baseurl"),"api/organisationUnits?filter=level:eq:3&fields=id,name")) %>% 
  httr::content("text") %>% 
  jsonlite::fromJSON() %>%
  rlist::list.extract("organisationUnits") 

ou_prioritization_levels<-httr::GET(paste0(getOption("baseurl"),"api/dataStore/dataSetAssignments/ous")) %>% 
  httr::content("text") %>% 
  jsonlite::fromJSON() %>% 
    plyr::ldply(function(x) {data.frame(x,stringsAsFactors = FALSE)}) %>%
  dplyr::select(name=name3,prioritization) %>%
  distinct %>%
  inner_join(ous,by=c("name")) 

getOrgunitsAtLevel <- function(parent_id,level) {
  url<-paste0(getOption("baseurl"),"api/organisationUnits?filter=path:like:",parent_id,"&filter=level:eq:",level,"&fields=id,name&paging=false")
  
  httr::GET(url) %>%
    httr::content("text") %>% 
    jsonlite::fromJSON() %>%
    rlist::list.extract("organisationUnits") 
}

psnus<-mapply(getOrgunitsAtLevel,ou_prioritization_levels$id,ou_prioritization_levels$prioritization)

militaryUnits<-getSiteList("Military")

generate_support_files_md5s<- function(support_files_path) { 
  file_names <- c(
    "distrClusterFY17.rda",
    "distrClusterFY18.rda",
    "distrSiteFY17.rda" ,
    "distrSiteFY18.rda" ,
    "mech_list.rda",
    "ous_list.rda" ,
    "SiteLevelReview_HTS_TEMPLATE.xlsx" ,
    "SiteLevelReview_TEMPLATE.xlsx"
  )
  support_files <- paste0(support_files_path, file_names)
 foo<-as.list(tools::md5sum(support_files))
 names(foo) <- basename(names(foo))
 return(foo)
 }

support_files_md5<-generate_support_files_md5s(getOption("support_files_path"))
#Save the data to sysdata.Rda. Be sure to rebuild the package and commit after this!
devtools::use_data(hts_schema,main_schema,main_site_schema,hts_site_schema,mechs,impatt,rCOP18deMapT,clusters, sites_exclude,psnus,militaryUnits,support_files_md5,internal = TRUE,overwrite = TRUE)
