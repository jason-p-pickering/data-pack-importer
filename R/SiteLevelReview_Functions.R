LoadDependencies <- function() {
    if (!require("pacman")) {
        install.packages("pacman",repos="https://cloud.r-project.org") }
    pacman::p_load(httr,jsonlite,assertthat,rlist,dplyr) 
}

LoadConfig<-function(config_path=NA) {
    #Load from a file
    if (!is.na(config_path) ) {
        foo <- assertthat::assert_that(file.exists(config_path))
        s<-jsonlite::fromJSON(config_path) 
        options("baseurl"= s$dhis$baseurl )
        options("config"=config_path)
        url<-URLencode( URL = paste0(getOption("baseurl"),"api/me") )
        #Logging in here will give us a cookie to reuse
        r<-httr::GET(url ,
                     httr::authenticate(s$dhis$username,s$dhis$password),
                     httr::timeout(60))
        assertthat::assert_that(r$status == 200L)
        print("Successfully logged in!")
        r<- httr::content(r, "text")
        me<-jsonlite::fromJSON(r)
        options("organisationUnit" = me$organisationUnits$id) } else {
            stop("You must specify a credentials file!")
        }
}


GetMechsForOU<-function(ou_uid,endDate="2019-09-30") {
    
    ou_name<-paste0(getOption("baseurl"),"api/organisationUnits/",ou_uid,"/name") %>%
        GET %>%
        content(.,"text") %>%
        fromJSON(.,flatten = TRUE) %>%
        list.extract(.,"name")
    
    r<-paste0(getOption("baseurl"),"api/sqlViews/fgUtV6e9YIX/data.json") %>%
        GET %>% 
        content(.,"text") %>%
        fromJSON(.,flatten = TRUE)
    
    d<-as.data.frame(r$rows,stringsAsFactors = FALSE) %>% 
        setNames(.,r$headers$name) %>%
        dplyr::filter(ou==ou_name & enddate == endDate) %>%
        dplyr::select(uid,mechanism)
    
    return(d)
}

GetPrioritizationLevel<-function(ou_uid) {
    ou_name<-paste0(getOption("baseurl"),"api/organisationUnits/",ou_uid,"/name") %>%
        GET %>%
        content(.,"text") %>%
        fromJSON(.,flatten = TRUE) %>%
        list.extract(.,"name")
    
    r<-paste0(getOption("baseurl"),"/api/sqlViews/W5RIpSXU1DP/data.json?paging=false&filter=name3:eq:",ou_name,"&fields=prioritization") %>%
        GET %>% 
        content(.,"text") %>%
        fromJSON(.,flatten = TRUE)
    
    d<-as.data.frame(r$rows,stringsAsFactors=FALSE)
    
    return(d)
}

GetPSNUsiteMap <- function(ou_uid) {
    psnuLevel<-GetPrioritizationLevel(ou_uid)
    
    orgHierarchy<-paste0(getOption("baseurl"),"/api/sqlViews/kEtZ2bSQCu2/data.json?filter=uidlevel3:eq:",ou_uid,"&fields=organisationunituid,name,level",psnuLevel,"name") %>%
        GET %>% 
        content(.,"text") %>%
        fromJSON(.,flatten = TRUE)
    
    d<-as.data.frame(orgHierarchy$rows,stringsAsFactors = FALSE) %>% 
        setNames(.,orgHierarchy$headers$name) %>%
        mutate(DataPackSiteID=paste0(level6name," > ",name," > (",organisationunituid,")")) %>%
        select(-level6name,-name)
    
    return(d)
}

getOUname<-function(ou_uid,config_path){
    LoadDependencies()
    LoadConfig(config_path)
    ou_name<-paste0(getOption("baseurl"),"api/organisationUnits/",ou_uid,"/name") %>%
        GET %>%
        content(.,"text") %>%
        fromJSON(.,flatten = TRUE) %>%
        list.extract(.,"name")
}

siteFileStructure<-function(schemaType){
    d<-tibble()
    for(i in 2:17){
        d<-tibble(DataPackCode=as.character(schemaType$schema[[i]]$fields)) %>%
    #Drop all non-essential fields
            filter(!str_detect(DataPackCode,"^A_")) %>%
            slice(8:n()) %>%
    #Tag by Tab Name
            mutate(DataPackTabName=schemaType$schema[[i]]$sheet_name) %>%
    #Convert Tab Names where these were oddly grouped in Disagg Tool
            mutate(DataPackTabName=case_when(str_detect(DataPackCode,"^kp_")~"KP"
                                             ,str_detect(DataPackCode,"^ovc_hivstat")~"OVC"
                                             ,str_detect(DataPackCode,"^D_pmtct")~"PMTCT"
                                             ,DataPackTabName=="OVC_SERV"~"OVC"
                                             ,DataPackTabName=="PMTCT_STAT"~"PMTCT"
                                             ,DataPackTabName=="Index"~"IndexTesting"
                                             ,TRUE~DataPackTabName)) %>%
    #Tag by File name
            mutate_(.dots=setNames(ifelse(schemaType$mode=="NORMAL","\"DisaggTool\"","\"HTS_DisaggTool\""),"DataPackFilename")) %>%
            rbind(d,.)
    }
    return(d)
}

getSiteReviewHeaders<-function(){
    d<-siteFileStructure(main_schema) %>% rbind(siteFileStructure(hts_schema)) %>%
        arrange(DataPackTabName) %>%
    #Drop repeats
        filter(!(DataPackCode=="D_vmmc_circ_fy19" & DataPackTabName=="VMMC")) %>%
    #Create and preserve order across sources
        group_by(DataPackTabName) %>%
        mutate(line=paste0("N",ifelse(row_number()<10,0,""),row_number())) %>%
        ungroup() %>%
        right_join(unique(filter(select(COP18deMapT,-DataPackFilename,-pd_2017_2018_S,-pd_2019_S,-pd_2019_P,-FiscalYear,-COPidName,-supportType,-DataPackTabName),!is.na(DataPackCode) & IMPATT==FALSE & Age!="Unknown Age"))
                   ,by=c("DataPackCode")) %>%
        arrange(DataPackFilename,DataPackTabName,line) %>%
        group_by(DataPackFilename,DataPackTabName) %>%
        mutate(line=paste0("N",ifelse(row_number()<10,0,""),row_number())) %>%
        ungroup() %>%
    #Create clear Labels
        mutate(siteReviewLbl=paste(indicator,numeratorDenom,Modality,KeyPop,KnownNewStatus,NewExistingART
                                   ,Indication,TBStatus,pregBF,VMMCTechnique,Age,Sex,HIVStatus,otherDisagg
                                   ,sep=", ")) %>%
        mutate(siteReviewLbl=str_replace_all(siteReviewLbl,"(, ){2,}",", ")) %>%
        mutate(siteReviewLbl=str_replace(siteReviewLbl,", $","")) %>%
        select(siteReviewLbl,DataPackCode,DataPackFilename,DataPackTabName,line)
}

writeSiteLevelTool <- function(topFolder,ou_uid,config_path,schema) {
    rails<-siteReviewHeaders %>%
        dplyr::select(DataPackFilename,DataPackTabName) %>%
        dplyr::filter(DataPackFilename==schema) %>%
        unique %>%
        dplyr::arrange(DataPackFilename,DataPackTabName)
    
    ou_name<-getOUname(ou_uid,config_path)
    
    templatePath<-ifelse(schema=="DisaggTool"
                         ,paste0(topFolder,"SiteLevelReview_TEMPLATE.xlsx")
                         ,paste0(topFolder,"SiteLevelReview_HTS_TEMPLATE.xlsx"))
                                                          
    wb<-loadWorkbook(templatePath,create=FALSE)
    setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
    writeWorksheet(wb,siteIDs$DataPackSiteID,sheet="SiteList",startRow=2,startCol=1,header=FALSE)
    writeWorksheet(wb,mechs$mechanism,sheet="Mechs",startRow=2,startCol=1,header=FALSE)
    
    #Begin for loop - one per sheet
    for(i in 1:NROW(rails)){
        
        sheet_name=as.character(rails[i,2])
        
        data<-dv %>%
            dplyr::filter(DataPackTabName==sheet_name) %>%
            dplyr::select(-DataPackCode,-DataPackFilename,-DataPackTabName) %>%
            dplyr::group_by(DataPackSiteID,mechanism,supportType,line) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup() %>%
            tidyr::spread(line,value) %>%
            dplyr::filter(!is.na(DataPackSiteID))
        
        if(NROW(data)==0){next}
        
        sums <- data %>%
            dplyr::select(-DataPackSiteID,-mechanism,-supportType) %>%
            colSums(na.rm=TRUE) %>%
            t() %>%
            data.frame
        
        inactiveFormula<-paste0("IF(AND(",sheet_name,"!$B",7:(NROW(data)+6),"<>\"\",INDEX(SiteList!$B:$B,MATCH(",sheet_name,"!$B",7:(NROW(data)+6),",SiteList,0)+1)=1),\"!!\",\"\")")
        
        writeWorksheet(wb,data,sheet=sheet_name,startRow=7,startCol=2,header=FALSE)
        writeWorksheet(wb,sums,sheet=sheet_name,startRow=4,startCol=5,header=FALSE)
        setCellFormula(wb,sheet_name,7:(NROW(data)+6),1,inactiveFormula)
    }
    #End for loop    
    
    saveWorkbook(wb,file=paste0(topFolder,"/SiteLevelReview"
                                ,ifelse(schema=="DisaggTool"
                                        ,""
                                        ,"_HTS")
                                ,"_",ou_name,".xlsx"))
}