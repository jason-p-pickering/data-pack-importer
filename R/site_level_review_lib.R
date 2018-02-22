


#' Title
#'
#' @param schemaType 
#'
#' @return
#' @export
#'
#' @examples
siteFileStructure<-function(schemaType){

    #Does this need to be hard-coded??
    for(i in 2:17){
        d<-dplyr::tibble(DataPackCode=as.character(schemaType$schema[[i]]$fields)) %>%
    #Drop all non-essential fields
          dplyr::filter(!str_detect(DataPackCode,"^A_")) %>%
    #How can you guarentee the correct order with this slice?
    #Is it always 8??
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
            mutate_(.dots=stats::setNames(ifelse(schemaType$mode=="NORMAL","\"DisaggTool\"","\"HTS_DisaggTool\""),"DataPackFilename")) %>%
            dplyr::bind_rows(d,.)
    }
    return(d)
}

getSiteReviewHeaders<-function(){
    d<-siteFileStructure(main_schema) %>% rbind(siteFileStructure(hts_schema)) %>%
        dplyr::arrange(DataPackTabName) %>%
    #Drop repeats
      dplyr::filter(!(DataPackCode=="D_vmmc_circ_fy19" & DataPackTabName=="VMMC")) %>%
    #Create and preserve order across sources
      dplyr::group_by(DataPackTabName) %>%
      dplyr::mutate(line=paste0("N",ifelse(row_number()<10,0,""),row_number())) %>%
      dplyr::ungroup() %>%
      dplyr::right_join(unique(filter(select(COP18deMapT,-DataPackFilename,-pd_2017_2018_S,-pd_2019_S,-pd_2019_P,-FiscalYear,-COPidName,-supportType,-DataPackTabName),!is.na(DataPackCode) & IMPATT==FALSE & Age!="Unknown Age"))
                   ,by=c("DataPackCode")) %>%
      dplyr::arrange(DataPackFilename,DataPackTabName,line) %>%
      dplyr::group_by(DataPackFilename,DataPackTabName) %>%
      dplyr::mutate(line=paste0("N",ifelse(row_number()<10,0,""),row_number())) %>%
      dplyr::ungroup() %>%
    #Create clear Labels
      dplyr::mutate(siteReviewLbl=paste(indicator,numeratorDenom,Modality,KeyPop,KnownNewStatus,NewExistingART
                                   ,Indication,TBStatus,pregBF,VMMCTechnique,Age,Sex,HIVStatus,otherDisagg
                                   ,sep=", ")) %>%
      dplyr::mutate(siteReviewLbl=str_replace_all(siteReviewLbl,"(, ){2,}",", ")) %>%
      dplyr::mutate(siteReviewLbl=str_replace(siteReviewLbl,", $","")) %>%
      dplyr::select(siteReviewLbl,DataPackCode,DataPackFilename,DataPackTabName,line)
}

writeSiteLevelTool <- function(topFolder,ou_uid,config_path,schema) {
    rails<-siteReviewHeaders %>%
        dplyr::select(DataPackFilename,DataPackTabName) %>%
        dplyr::filter(DataPackFilename==schema) %>%
        dplyr::distinct%>%
        dplyr::arrange(DataPackFilename,DataPackTabName)
    
    ou_name<-getOUname(ou_uid,config_path)
    
    templatePath<-ifelse(schema=="DisaggTool"
                         ,paste0(topFolder,"SiteLevelReview_TEMPLATE.xlsx")
                         ,paste0(topFolder,"SiteLevelReview_HTS_TEMPLATE.xlsx"))
                                                          
    wb<-XLConnect::loadWorkbook(templatePath,create=FALSE)
    XLConnect::setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
    XLConnect::writeWorksheet(wb,siteIDs$DataPackSiteID,sheet="SiteList",startRow=2,startCol=1,header=FALSE)
    XLConnect::writeWorksheet(wb,mechs$mechanism,sheet="Mechs",startRow=2,startCol=1,header=FALSE)
    
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
        
        XLConnect::writeWorksheet(wb,data,sheet=sheet_name,startRow=7,startCol=2,header=FALSE)
        XLConnect::writeWorksheet(wb,sums,sheet=sheet_name,startRow=4,startCol=5,header=FALSE)
        XLConnect::setCellFormula(wb,sheet_name,7:(NROW(data)+6),1,inactiveFormula)
    }
    #End for loop    
    
      XLConnect::saveWorkbook(wb,file=paste0(topFolder,"/SiteLevelReview"
                                ,ifelse(schema=="DisaggTool"
                                        ,""
                                        ,"_HTS")
                                ,"_",ou_name,".xlsx"))
}