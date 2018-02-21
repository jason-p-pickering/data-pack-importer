require(dplyr)
require(stringr)

#' @title getCOPDataElements()
#'
#' @description Extracts DATIM dataElement/categoryOptionCombo code list for specified dataset & time period.
#' @param ds DATIM Dataset UID.
#' @param pd Code indicating which time period for which to query code list.
#' @param lvl Organization unit level related to code list in question (P=PSNU, C=Community, F=Facility)
#' @return Returns a dataframe ready for use in preparing source files.
#'

getCOPDataElements <- function(ds,pd,lvl) {
    de <- read.csv(url(paste0(getOption("baseurl"),"api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:",ds)),stringsAsFactors=FALSE,header=TRUE) %>%
        mutate(period = pd) %>%
        mutate(level = lvl) %>%
        mutate(pdLvl = paste("pd",period,level,sep="_",collapse=NULL)) %>%
        mutate(indicator = str_trim(str_extract(str_replace_all(dataelement,"(?<=IMPATT)\\.","_"),"^\\w+\\s")))
    return(de)
}




transformDEs <- function(de){
    ds <- de %>%
        #Prepare cross-period codes for each MER indicator
        mutate(COPdeName = str_replace_all(str_replace_all(dataelement,"(\\) TARGET)|( v\\d)|\\:(.)*| T_PSNU|\\)|,|\\(","")," |/","_")) %>%
        mutate(COPcocName = as.character(categoryoptioncombo)) %>%
        mutate(COPuid = paste(dataelementuid,categoryoptioncombouid,sep=".",collapse=NULL)) %>%
        #Break out dataElementName for helpful referencing
        mutate(indicator = str_trim(str_extract(str_replace_all(dataelement,"IMPATT\\.",""),"^\\w+\\s"))) %>%
        mutate(numeratorDenom = ifelse(str_detect(code,"_[ND]_"),
                                              str_replace_all(str_extract(code,"_[ND]_"),"_",""),
                                              "")) %>%
        mutate(supportType = ifelse(str_detect(dataelement,"(DSD|TA)(?=\\W)"),
                                           str_replace_all(str_extract(dataelement,"(DSD|TA)(?=\\W)"),"_",""),
                                           "")) %>%
        
        #Break out categoryOptionCombo for helpful referencing
        ##Modality
        mutate(Modality=ifelse(str_detect(COPdeName,"HTS_TST(.)+(Emergency_Ward_|HomeMod_|Index_|IndexMod_|Inpat_|Malnutrition_|MobileMod_|OtherMod_|OtherPITC_|Pediatric_|PMTCT_ANC_|STI_Clinic_|TBClinic_|VCT_|VCTMod_|VMMC_)")
                                      ,str_extract(dataelement,"Emergency Ward|HomeMod|IndexMod|Index|Inpat|Malnutrition|MobileMod|OtherMod|OtherPITC|Pediatric|PMTCT ANC|STI Clinic|TBClinic|VCTMod|VCT|VMMC")
                                      ,"")) %>%
        ##Age
        mutate(Age=case_when(str_detect(COPdeName,"Age") ~ str_trim(str_extract(COPcocName,"(?<!D)(<|<=)?( )?\\d{1,2}( months)?(( )?-( )?\\d{1,2}( months| years)?|\\+)?|Unknown Age"))
                                           ,str_detect(dataelement,"HTS_TST(.)+(Malnutrition|Pediatric)") ~ "<5"
                                           ,TRUE~"")) %>%
        ##AggregatedAge
        mutate(AggregatedAge=Age)
    word=c("< 2 months","<= 2 months","2 - 12 months","<1","<5","1-9","2 months - 9 years","<10","10-14","<15","15-17","15-19","18-24","20-24","25-29","15-29","30-34","35-39","40-49","30-49","25-49","15\\+","25\\+","30\\+","50\\+","Unknown Age")
    tran=c(rep("<15",10),rep("15+",15),"Unknown Age")
    dict=data.frame(word,tran,stringsAsFactors = FALSE)
    for (k in 1:length(dict$word)){ds$AggregatedAge=str_replace_all(ds$AggregatedAge,paste0("^",dict$word[k],"$"),dict$tran[k])}
    
    ##Aggregated Age Flag (to distinguish otherwise identical indicators)
    ds <- ds %>% mutate(AggregatedAgeFlag=ifelse(str_detect(dataelement,"Age Aggregated|(PLHIV|HIV_PREV)(.)+Age")
                                                        ,"AgeAggregated"
                                                        ,"")) %>%
        ##Sex
        mutate(Sex=ifelse(str_detect(COPdeName,"Sex|HTS_TST(.)+VCT_Age_Result")
                                 |str_detect(COPcocName,"Female PWID|Male PWID|MSM|FSW")
                                 ,str_extract(COPcocName,"Male|Female|Unknown Sex|MSM|FSW")
                                 ,""))
    dict=data.frame(word=c("MSM","FSW"),tran=c("Male","Female"),stringsAsFactors = FALSE)
    for (k in 1:length(dict$word)){ds$Sex<-str_replace_all(ds$Sex,dict$word[k],dict$tran[k])}
    rm(dict)
    ds <- ds %>%
        mutate(Sex=case_when(str_detect(dataelement,"HTS_TST|OVC_SERV|PMTCT_EID|TB_ART|TX_(CURR|NEW|PVLS|RET)") & str_detect(Age,"(<1|1-9|<5)$") ~ "Unknown Sex",TRUE~Sex)) %>%
        mutate(Sex=case_when(str_detect(dataelement,"HTS_TST(.)+VMMC|VMMC_CIRC") ~ "Male"
                                           ,str_detect(dataelement,"HTS_TST(.)+PMTCT|PMTCT_ART|PMTCT_STAT") ~ "Female"
                                           ,TRUE ~ Sex)) %>%
        
        ##HIVStatus
        #            Convert to "HIV Positive", "HIV Negative", "HIV Unknown"
        mutate(HIVStatus=ifelse(str_detect(COPdeName,"_Result|_HIVStatus|_KnownNewResult|_KnownNewPosNeg|OVC_HIVSTAT_(.)+_ReportedStatus")
                                       | (str_detect(COPdeName,"KP_PREV_(N|D)_(DSD|TA)_KeyPop_Status|PP_PREV_(N|D)_(DSD|TA)_Status") & str_detect(COPcocName,"Positive|Negative"))
                                       ,str_extract(COPcocName,"Positive|Negative|Unknown(?=($|,))|Undisclosed")
                                       ,"")) %>%
        mutate(HIVStatus=str_replace(HIVStatus,"Undisclosed","Unknown")) %>%
        mutate(HIVStatus=case_when(str_detect(COPdeName,"^(IMPATT.PLHIV|PMTCT_ART|TB_ART|TB_PREV|TX_CURR|TX_NEW|TX_PVLS|TX_RET|TX_TB)|OVC_HIVSTAT_(.)+_StatusPosART") & !(HIVStatus %in% ("Positive")) ~ "Positive"
                                                 ,str_detect(COPdeName,"OVC_HIVSTAT(.)+StatusNotRep") & !(HIVStatus %in% ("Unknown")) ~ "Unknown"
                                                 ,str_detect(COPdeName,"HTS_TST(.)+Positive") ~ "Positive"
                                                 ,TRUE ~ HIVStatus)) %>%
        ##KeyPop
        mutate(KeyPop=ifelse(str_detect(COPdeName,"_KeyPop")
                                    ,str_extract(COPcocName,"FSW|MSM(( not)? SW)?|TG(( not)? SW)?|PWID|People in prisons and other enclosed settings|Other Key Populations")
                                    ,"")) %>%
        ##KnownNewStatus
        mutate(KnownNewStatus=ifelse(str_detect(COPcocName,"Known at Entry|Newly Tested or Testing Referred|Newly Identified|Declined Testing Or Testing Referral")
                                            ,str_extract(COPcocName,"Known|New|Declined")
                                            ,"")) %>%
        ##NewExistingART
        mutate(NewExistingART=ifelse(str_detect(COPdeName,regex("NewExistingArt",ignore_case=T))
                                            ,str_extract(COPcocName,"New|Already")
                                            ,"")) %>%
        ##Indication
        mutate(Indication=ifelse(str_detect(COPdeName,"TX_PVLS(.)+(Indication|RoutineTargeted)")
                                        ,str_extract(COPcocName,"Routine|Targeted|Undocumented Test Indication")
                                        ,"")) %>%
        ##TBStatus
        mutate(TBStatus=ifelse(str_detect(COPdeName,"TX_NEW(.)+TB_Diagnosis|TB_STAT|TB_ART|TX_TB_(N|D(.)+(TBScreen|PositiveScreen))")
                                      ,ifelse(str_detect(COPdeName,"TX_TB_D")
                                              ,paste0("TB ",str_extract(COPcocName,"(?<=(TB Screen - |^))(Posi|Nega)tive"))
                                              ,"TB Positive")
                                      ,"")) %>%
        ##pregBF
        mutate(pregBF=ifelse(str_detect(dataelement,"PMTCT_(ART|STAT)|TX_(NEW|RET|PVLS)(.)+PregnantOrBreastfeeding")
                                    ,ifelse(str_detect(dataelement,"PMTCT_(ART|STAT)")
                                            ,"Pregnant"
                                            ,str_extract(COPcocName,"Pregnant|Breastfeeding"))
                                    ,"")) %>%
        ##VMMCTechnique
        mutate(VMMCTechnique=ifelse(str_detect(dataelement,"VMMC_CIRC(.)+Tech")
                                           ,str_extract(COPcocName,"Device based|Surgical Technique")
                                           ,""))
    ##otherDisagg
    pattern=paste(ifelse(ds$KeyPop=="","0000",ds$KeyPop)
                  ,ifelse(ds$Sex=="","0000",ds$Sex)
                  ,ifelse(ds$Age=="","0000",paste0("\\Q",ds$Age,"\\E"))
                  ,"TB Screen - (Posi|Nega)tive"
                  ,ifelse(ds$HIVStatus=="","0000",ds$HIVStatus)
                  ,"Declined Testing Or Testing Referral","Newly Tested or Testing Referred","Newly Identified","Known at Entry","Life-long ART"
                  ,ifelse(ds$KnownNewStatus=="","0000",ds$KnownNewStatus)
                  ,ifelse(ds$NewExistingART=="","0000",ds$NewExistingART)
                  ,ifelse(ds$Indication=="","0000",ds$Indication)
                  ,ifelse(ds$pregBF=="","0000",ds$pregBF)
                  ,ifelse(ds$VMMCTechnique=="","0000",ds$VMMCTechnique)
                  ,"default"
                  ,sep="|")
    ds <- ds %>%    
        mutate(otherDisagg=str_replace_all(COPcocName,pattern,"")) %>%
        mutate(otherDisagg=ifelse(str_detect(COPdeName,"TX_TB(.)+Specimen_Sent"),"Specimen Sent",otherDisagg)) %>%
        mutate(otherDisagg=str_trim(str_replace_all(otherDisagg,"^,( ,)?|(,( )?)+$|,( ,)+|\\(((,)?( )?)\\)",""))) %>%
        mutate(otherDisagg=ifelse(str_detect(dataelement, "TX_TB(.)+PositiveScreen")
                                         ,str_replace(otherDisagg,"Negative","")
                                         ,otherDisagg)) %>%
        
        ###Fix otherDisaggs where necessary
        mutate(otherDisagg=case_when(str_detect(COPdeName,"TB_PREV") ~ str_replace(otherDisagg,"Alternative Regimen","Alternative TPT Regimen")
                                                   ####GEND_GBV(.)*PEP
                                                   ,str_detect(COPdeName,"GEND_GBV(.)+PEP") ~ "PEP"
                                                   ####TX_RET
                                                   ,str_detect(COPdeName,"TX_RET(.)+") ~ ifelse(str_detect(COPdeName,"24mo|36mo"),str_extract(dataelement,"24mo|36mo"),"12mo")
                                                   ####VMMC_CIRC
                                                   ,str_detect(COPdeName,"VMMC_CIRC(.)+TechFollowUp") ~ "Followed up within 14 days"
                                                   ,TRUE ~ otherDisagg)) %>%
        
        ##IMPATT
        mutate(IMPATT = pdLvl=="pd_IMPT_P") %>%
        
        mutate(COPidName=paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,HIVStatus,otherDisagg,sep="|"))
    
    return(ds)
}




generateCodeListT <- function(){
  
  ##Define Datasets, Periods, and Levels for which to pull dataElement Code Lists
  dataset = c(
    #2019 Targets
    #(Facility,Community,PSNU)
    "eyI0UOWJnDk",
    "l796jk9SW7q",
    "msJImKkTJkV",
    #IMPATT
    "pTuDWXzkAkJ",
    #2018 Targets
    #(Facility, Community)
    "AitXBHsC7RA",
    "BuRoS9i851o",
    #2017Q4
    #(Facility,Community)
    "uTvHPA1zqzi",
    "O3VMNi8EZSV"
  )
  period = c(rep("19_T", 3), "IMPT", rep(c("18_T", "17_4"), each = 2))
  level = c("F", "C", "P", "P", rep(c("F", "C"), 2))
  tech_areas <-
    c(
      "TX_RET",
      "TX_NEW",
      "TX_CURR",
      "TX_PVLS",
      "PMTCT_STAT",
      "PMTCT_ART",
      "PMTCT_EID",
      "TB_STAT",
      "TB_ART",
      "TB_PREV",
      "TX_TB",
      "HTS_TST",
      "VMMC_CIRC",
      "HTS_SELF",
      "OVC_SERV",
      "OVC_HIVSTAT",
      "KP_PREV",
      "PP_PREV",
      "KP_MAT",
      "GEND_GBV",
      "GEND_GBV_PEP",
      "PrEP_NEW",
      "IMPATT_PLHIV"
    )
  
  COPdataElements<-NULL
  for (i in 1:length(dataset)) {
    COPdataElements=bind_rows(COPdataElements,getCOPDataElements(dataset[i],period[i],level[i]))
  }
  
  
  COP18deMapT <- COPdataElements %>%
    unique %>% filter( indicator %in% tech_areas) %>% 
    transformDEs %>% 
    select(
      COPidName,
      indicator,
      numeratorDenom,
      supportType,
      Modality,
      KeyPop,
      KnownNewStatus,
      NewExistingART,
      Indication,
      TBStatus,
      pregBF,
      VMMCTechnique,
      Age,
      AggregatedAge,
      AggregatedAgeFlag,
      Sex,
      HIVStatus,
      otherDisagg,
      IMPATT,
      pdLvl,
      COPuid
    ) %>% 
    spread(pdLvl, COPuid) %>%
    mutate(
      pd_2017 = case_when((is.na(pd_17_4_F) &
                             !is.na(pd_17_4_C)) ~ pd_17_4_C,
                          (is.na(pd_17_4_F) &
                             !is.na(pd_IMPT_P)) ~ pd_IMPT_P,
                          TRUE ~ pd_17_4_F
      ),
      pd_2018 = case_when((is.na(pd_18_T_F) &
                             !is.na(pd_18_T_C)) ~ pd_18_T_C,
                          (is.na(pd_18_T_F) &
                             !is.na(pd_IMPT_P)) ~ pd_IMPT_P,
                          TRUE ~ pd_18_T_F
      ),
      pd_2019_S = case_when(is.na(pd_19_T_F) ~ pd_19_T_C,
                            TRUE ~ pd_19_T_F),
      pd_2019_P = case_when((is.na(pd_19_T_P) &
                               !is.na(pd_IMPT_P)) ~ pd_IMPT_P,
                            TRUE ~ pd_19_T_P)
    ) %>%
    select(
      -pd_17_4_C,
      -pd_17_4_F,
      -pd_18_T_C,
      -pd_18_T_F,
      -pd_19_T_C,
      -pd_19_T_F,
      -pd_19_T_P,
      -pd_IMPT_P
    ) %>%
    gather(Period, pd_2017_2018_S, pd_2017, pd_2018) %>%
    mutate(FiscalYear = as.numeric(str_extract(Period, "\\d{4}"))) %>%
    select(-Period) %>%
    filter(!(is.na(pd_2019_S) &
               is.na(pd_2019_P) & is.na(pd_2017_2018_S))) %>%
    filter(!(IMPATT == TRUE & FiscalYear == 2017))
  
  return(COP18deMapT)


}


mapDataPackCodes <- function(COP18deMapT) {
    
  dp_codes_file<-"data-raw/DataPackCodeListKeysMatched.csv"
  
  blankToNA<-function (x) {ifelse(x=="",NA,x)}
  
  DataPackCodes <-
    read.csv(dp_codes_file,
             stringsAsFactors = FALSE,
             header = TRUE) %>%
    unique %>%
    mutate(supportType = case_when(IMPATT == FALSE ~ "DSD", TRUE ~ "")) %>%
    mutate(
      DataPackCode = blankToNA(DataPackCode),
      DataPackFilename = blankToNA(DataPackFilename),
      DataPackTabName = blankToNA(DataPackTabName)
    ) %>%
    bind_rows(mutate(filter(., IMPATT == FALSE), supportType = "TA"), .) %>%
    mutate(
      DataPackCode = case_when(
        (!is.na(DataPackCode) & DataPackCode != "plhiv_fy19") ~ paste(DataPackCode, tolower(supportType), sep = "_"),
                               TRUE ~ DataPackCode)
      ) %>%
    #Re-create COPidName using DSD/TA
    mutate(
      COPidName = paste(
        indicator,
        numeratorDenom,
        supportType,
        Modality,
        KeyPop,
        KnownNewStatus,
        NewExistingART,
        Indication,
        TBStatus,
        pregBF,
        VMMCTechnique,
        Age,
        AggregatedAge,
        AggregatedAgeFlag,
        Sex,
        HIVStatus,
        otherDisagg,
        sep = "|"
      )
    ) %>%
    #Select only necessary columns
      select(DataPackCode,DataPackFilename,DataPackTabName,COPidName)
  #Pull in Data Pack Codes
    COP18deMapT <- COP18deMapT %>%
        dplyr::full_join(DataPackCodes,by=c("COPidName"))
  
    return(COP18deMapT)
}

generateCOP18deMap<-function() {
  COP18deMapT<-generateCodeListT() %>%
      mapDataPackCodes()
  
FY19deMap <- COP18deMapT %>%
  mutate(matchCode = COPidName) %>%
  select(matchCode,pd_2019_P,pd_2019_S) %>%
  filter(!is.na(pd_2019_P)) %>%
  unique()


rCOP18deMap <- COP18deMapT %>%
  select(-pd_2019_S,-pd_2019_P) %>%
  filter(!is.na(pd_2017_2018_S)) %>%
  unique() %>%
  mutate(matchCode=COPidName) %>%
  
  #1) Many to One Combinations#
        ##TB_STAT (KnownNewStatus+HIVStatus+Sex+AgeAggregated -> Sex+AgeAggregated)
            mutate(matchCode=case_when(indicator=="TB_STAT" & KnownNewStatus!="" & HIVStatus!="" ~ paste(indicator,numeratorDenom,supportType,Modality,KeyPop,"",NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,"",otherDisagg,sep="|"),TRUE ~ matchCode)) %>%
        ##PMTCT_EID (Age+HIVStatus -> Age)
            mutate(matchCode=case_when(indicator=="PMTCT_EID" & HIVStatus!="" ~ paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,"",otherDisagg,sep="|"),TRUE ~ matchCode)) %>%
        ##OVC_HIVSTAT (HIVStatus+otherDisagg)
            mutate(matchCode=case_when(indicator=="OVC_HIVSTAT" & HIVStatus!="" & otherDisagg %in% c("","Undisclosed to IP") ~ paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,"","",sep="|"),TRUE ~ matchCode))
        ##GEND_GBV (ViolenceServiceType/Age/Sex -> ViolenceServiceType)
            rCOP18deMap <- rCOP18deMap %>%
              filter(indicator=="GEND_GBV" & FiscalYear==2017 & Age!="" & Sex!="" & otherDisagg %in% c('Physical and/or Emotional Violence','Sexual Violence (Post-Rape Care)')) %>%
              mutate(matchCode=paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,"","",AggregatedAgeFlag,"",HIVStatus,otherDisagg,sep="|")) %>%
              bind_rows(rCOP18deMap,.) %>%
  
  #2) More Complicated
        ##TB_PREV
            ###(6-12 Month IPT|Continuous IPT --> IPT) & (NewExistingART="" --> NewExistingART="New")
                mutate(matchCode=case_when(indicator=="TB_PREV" & otherDisagg %in% c("6-12 Month IPT","Continuous IPT") ~ paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,"New",Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,HIVStatus,"IPT",sep="|")
                                         ,indicator=="TB_PREV" & otherDisagg %in% c("Alternative TPT Regimen") ~ paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,"New",Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,HIVStatus,otherDisagg,sep="|")
                                         ,TRUE ~ matchCode))
            ###(ADD ROWS for splitting out NewExistingART to include "Already")
                rCOP18deMap <- rCOP18deMap %>%
                  filter(indicator=="TB_PREV" & otherDisagg !="") %>%
                  mutate(matchCode=str_replace(matchCode,"\\|New\\|","\\|Already\\|")) %>%
                  bind_rows(rCOP18deMap,.)

        ##New Age Bands
            ageBands<-data.frame(Age=c(rep("25-49",4),rep("30-49",3))
                                 ,finererAge=c("25-29","30-34","35-39","40-49","30-34","35-39","40-49"),stringsAsFactors = FALSE)

            rCOP18deMap <- rCOP18deMap %>%
              filter(Age %in% c("30-49","25-49") & !indicator %in% c("TX_PVLS","TX_RET","TB_ART")) %>%
              left_join(ageBands,by=c("Age")) %>%
              mutate(matchCode=paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,finererAge,AggregatedAge,AggregatedAgeFlag,Sex,HIVStatus,otherDisagg,sep="|")) %>%
              select(names(rCOP18deMap)) %>%
                bind_rows(rCOP18deMap,.) %>%
              filter(!(str_detect(matchCode,"25-49|30-49") & !str_detect(matchCode,"TX_PVLS|TX_RET|TB_ART")))
            rm(ageBands)

        ##TX_PVLS|TX_RET|TB_ART
            ###(ADD ROWS to split <1 & 1-9 Unknown to Male/Female)
                rCOP18deMap <- rCOP18deMap %>%
                  mutate(matchCode=case_when(indicator %in% c("TX_PVLS","TX_RET","TB_ART") & Age %in% c("<1","1-9") ~ paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,AggregatedAge,AggregatedAge,"AgeAggregated","Male",HIVStatus,otherDisagg,sep="|")
                                             ,TRUE ~ matchCode))
                rCOP18deMap <- rCOP18deMap %>%
                  filter(indicator %in% c("TX_PVLS","TX_RET","TB_ART") & Age %in% c("<1","1-9")) %>%
                  mutate(matchCode=str_replace(matchCode,"\\|Male\\|","\\|Female\\|")) %>%
                    bind_rows(rCOP18deMap,.) %>%
                  ###(Convert Age bands from fine to coarse)
                  mutate(matchCode=case_when(indicator %in% c("TX_PVLS","TX_RET","TB_ART") & Age %in% c("10-14","15-19","20-24","25-49","50+") ~ paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,AggregatedAge,AggregatedAge,"AgeAggregated",Sex,HIVStatus,otherDisagg,sep="|")
                                             ,TRUE~matchCode))

    #3) One to Many combinations
        ##A) New indicators
            ###HTS_TST Emergency Ward <- HTS_TST PITC
                rCOP18deMap<-rCOP18deMap %>%
                  filter(indicator=="HTS_TST" & Modality=="OtherPITC") %>%
                  mutate(matchCode=str_replace(matchCode,"\\|OtherPITC\\|","\\|Emergency Ward\\|")) %>%
                    bind_rows(rCOP18deMap,.)
    
            ###HTS_TST STI Clinic <- HTS_TST PITC
                rCOP18deMap<-rCOP18deMap %>%
                  filter(indicator=="HTS_TST" & str_detect(matchCode,"\\|Emergency Ward\\|")) %>%
                  mutate(matchCode=str_replace(matchCode,"\\|Emergency Ward\\|","\\|STI Clinic\\|")) %>%
                    bind_rows(rCOP18deMap,.)
    
        ##B) New Disaggs
            ###TX_TB (Positive,Negative ->Positive-New,Positive-Already,Negative-New,Negative-Already)
                rCOP18deMap<-rCOP18deMap %>%
                  mutate(matchCode=case_when(indicator=="TX_TB" & numeratorDenom=="D" & TBStatus!="" ~ paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,"Already",Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,HIVStatus,otherDisagg,sep="|"),TRUE~matchCode))
                rCOP18deMap<-rCOP18deMap %>%
                  filter(indicator=="TX_TB" & numeratorDenom=="D" & TBStatus!="") %>%
                  mutate(matchCode=str_replace(matchCode,"\\|Already\\|","\\|New\\|")) %>%
                    bind_rows(rCOP18deMap,.)
    
        ##C) OVC_SERV (10-14 Male,Female -> <1 Unknown, 1-9 Unknown)
                rCOP18deMap <- rCOP18deMap %>%
                  filter(indicator=="OVC_SERV" & Age=="10-14" & otherDisagg!="") %>%
                  mutate(matchCode=paste(indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,"1-9",AggregatedAge,AggregatedAgeFlag,"Unknown Sex",HIVStatus,otherDisagg,sep="|")) %>%
                    bind_rows(rCOP18deMap,.)
                
                rCOP18deMap <- rCOP18deMap %>%
                  filter(indicator=="OVC_SERV" & otherDisagg!="" & str_detect(matchCode,"\\|1-9\\|")) %>%
                  mutate(matchCode=str_replace(matchCode,"\\|1-9\\|","\\|<1\\|")) %>%
                    bind_rows(rCOP18deMap,.)
    
    
    #4) DO NOT DISTRIBUTE TO SITE (Below for moving to PSNU only. Need to re-adjust to not persist to Site to force manual redistribution)
        ##PrEP_NEW (Other Key Populations - not distributed to Site, but moved to PSNU level by following PrEP_NEW N)
            rCOP18deMap <- rCOP18deMap %>%
              filter(indicator=="PrEP_NEW" & Sex=="" & Age=="" & KeyPop=="") %>%
              mutate(matchCode=paste(indicator,numeratorDenom,supportType,Modality,"Other Key Populations",KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,HIVStatus,otherDisagg,sep="|")) %>%
                bind_rows(rCOP18deMap,.)
        
        ##HTS_SELF (New indicator with no reliable model)
            ###KeyPops (Follow HTS_TST KeyPops to get to PSNU)
                rCOP18deMap <- rCOP18deMap %>%
                  filter(indicator=="HTS_TST" & KeyPop!="" & HIVStatus!="" & Modality=="" & Age=="") %>%
                  mutate(matchCode=paste("HTS_SELF",numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,"","Directly-Assisted",sep="|")) %>%
                    bind_rows(rCOP18deMap,.)
                rCOP18deMap<- rCOP18deMap %>%
                  filter(str_detect(matchCode,"HTS_SELF(.)+Directly-Assisted")) %>%
                  mutate(matchCode=str_replace(matchCode,"Directly-Assisted","Unassisted")) %>%
                    bind_rows(rCOP18deMap,.)
    
            ###Disaggs: Testing Type, Age/Sex, Unassisted by use
                HTS_SELF.Disaggs<-COP18deMapT %>%
                  filter(indicator=="HTS_SELF" & KeyPop=="") %>%
                  select(COPidName,supportType) %>%
                  unique() %>%
                  mutate(matchCode=paste0("HTS_TST|N|",supportType,"||||||||||||||")) %>%
                  select(-supportType)
                rCOP18deMap <- rCOP18deMap %>%
                  filter(matchCode %in% c("HTS_TST|N|DSD||||||||||||||","HTS_TST|N|TA||||||||||||||")) %>%
                  left_join(HTS_SELF.Disaggs,by=c("matchCode")) %>%
                  select(-matchCode) %>%
                  mutate(matchCode=COPidName.y,
                         COPidName=COPidName.x) %>%
                  select(-COPidName.y,-COPidName.x) %>%
                  select(names(rCOP18deMap)) %>%
                    bind_rows(rCOP18deMap,.)

rCOP18deMap <- rCOP18deMap %>%
  full_join(FY19deMap,by=c("matchCode")) %>%
    select(COPidName,indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,HIVStatus,otherDisagg,IMPATT,DataPackCode,DataPackFilename,DataPackTabName,FiscalYear,pd_2017_2018_S,pd_2019_S,pd_2019_P) %>%
    arrange(COPidName)

return(rCOP18deMap)
}