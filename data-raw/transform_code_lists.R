

generateCodeListT <- function() {
  
  getCOPDataElements <- function(ds,pd,lvl) {
    de <- read.csv(url(paste0(getOption("baseurl"),"api/sqlViews/DotdxKrNZxG/data.csv?var=dataSets:",ds)),stringsAsFactors=FALSE,header=TRUE)
    de$period <- pd
    de$level <- lvl
    de$pdLvl <- paste("pd",de$period,de$level,sep="_",collapse=NULL)
    de$indicator <- str_trim(str_extract(str_replace_all(de$dataelement,"(?<=IMPATT)\\.","_"),"^\\w+\\s"))
    return(de)
  }
  
  ##Define Datasets, Periods, and Levels for which to pull dataElement Code Lists
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
    "uTvHPA1zqzi",    "O3VMNi8EZSV")
  period = c(rep("19_T",3),"IMPT",rep(c("18_T","17_4"), each=2))
  level = c("F","C","P","P",rep(c("F","C"),2))
  
  
  
  COPdataElements=NULL
  for (i in 1:length(dataset)) {
    COPdataElements=rbind(COPdataElements,getCOPDataElements(dataset[i],period[i],level[i]))
  }
  
  #TEMPORARY: Code List Fix --> add four lines for PMTCT_ART in PSNU code list which are currently not appearing in sqlView.
  COPdataElements <- COPdataElements %>%
    rbind(c("MER Target Setting: PSNU (Facility and Community Combined)","PMTCT_ART (N, DSD, NewExistingArt/Sex/HIVStatus) T_PSNU: ART","PMTCT_ART (N, DSD, NewExistingArt/Sex/HIV) T_PSNU","PMTCT_ART_N_DSD_NewExistingArt_Sex_HIV_T_PSNU","DFdm1fjKS5u","Number of HIV-positive pregnant women who received ART to reduce risk of mother-to-child-transmission during pregnancy.","Life-long ART, New, Female, Positive","Q2EBeMBa8Ga","Q2EBeMBa8Ga","19_T","P","pd_19_T_P","PMTCT_ART")
          ,c("MER Target Setting: PSNU (Facility and Community Combined)","PMTCT_ART (N, DSD, NewExistingArt/Sex/HIVStatus) T_PSNU: ART","PMTCT_ART (N, DSD, NewExistingArt/Sex/HIV) T_PSNU","PMTCT_ART_N_DSD_NewExistingArt_Sex_HIV_T_PSNU","DFdm1fjKS5u","Number of HIV-positive pregnant women who received ART to reduce risk of mother-to-child-transmission during pregnancy.","Life-long ART, Already, Female, Positive","RTYO8ycjbCt","RTYO8ycjbCt","19_T","P","pd_19_T_P","PMTCT_ART")
          ,c("MER Target Setting: PSNU (Facility and Community Combined)","PMTCT_ART (N, TA, NewExistingArt/Sex/HIVStatus) T_PSNU: ART", "PMTCT_ART (N, TA, NewExistingArt/Sex/HIV) T_PSNU", "PMTCT_ART_N_TA_NewExistingArt_Sex_HIV_T_PSNU", "w4pjh8fNZx8","Number of HIV-positive pregnant women who received ART to reduce risk of mother-to-child-transmission during pregnancy.","Life-long ART, New, Female, Positive","Q2EBeMBa8Ga","Q2EBeMBa8Ga","19_T","P","pd_19_T_P","PMTCT_ART")
          ,c("MER Target Setting: PSNU (Facility and Community Combined)","PMTCT_ART (N, TA, NewExistingArt/Sex/HIVStatus) T_PSNU: ART", "PMTCT_ART (N, TA, NewExistingArt/Sex/HIV) T_PSNU", "PMTCT_ART_N_TA_NewExistingArt_Sex_HIV_T_PSNU", "w4pjh8fNZx8","Number of HIV-positive pregnant women who received ART to reduce risk of mother-to-child-transmission during pregnancy.","Life-long ART, Already, Female, Positive","RTYO8ycjbCt","RTYO8ycjbCt","19_T","P","pd_19_T_P","PMTCT_ART")
    ) %>%
    unique()
  
  COPdataElements = subset(COPdataElements,COPdataElements$indicator %in% c("TX_RET","TX_NEW","TX_CURR","TX_PVLS","PMTCT_STAT","PMTCT_ART","PMTCT_EID","TB_STAT","TB_ART","TB_PREV","TX_TB","HTS_TST","VMMC_CIRC","HTS_SELF","OVC_SERV","OVC_HIVSTAT","KP_PREV","PP_PREV","KP_MAT","GEND_GBV","GEND_GBV_PEP","PrEP_NEW","IMPATT_PLHIV"))
  
  
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #II. Standardize all dataElement/categoryOptionCombo Code Lists#####
  #////////////////////////////////////////////////////
  
  #****************************************************
  ##Function to Standardize dataElement Code Lists
  #****************************************************
  
  transformDEs <- function(de){
    #Prepare cross-period codes for each MER indicator
    de$COPdeName = str_replace_all(str_replace_all(de$dataelement,"(\\) TARGET)|( v\\d)|\\:(.)*| T_PSNU|\\)|,|\\(","")," |/","_")
    de$COPcocName = as.character(de$categoryoptioncombo)
    de$COPuid = paste(de$dataelementuid,de$categoryoptioncombouid,sep=".",collapse=NULL)
    #Break out dataElementName for helpful referencing
    #de$indicator <- str_trim(str_extract(str_replace_all(de$dataelement,"IMPATT\\.",""),"^\\w+\\s"))
    de$numeratorDenom = ""
    de$numeratorDenom = ifelse(str_detect(de$code,"_[ND]_"),
                               str_replace_all(str_extract(de$code,"_[ND]_"),"_",""),
                               de$numeratorDenom)
    de$supportType = ""
    de$supportType = ifelse(str_detect(de$dataelement,"(DSD|TA)(?=\\W)"),
                            str_replace_all(str_extract(de$dataelement,"(DSD|TA)(?=\\W)"),"_",""),
                            de$supportType)
    de$Modality=""
    de$KeyPop=""
    de$KnownNewStatus=""
    de$NewExistingART=""
    de$Indication=""
    de$TBStatus=""
    de$pregBF=""
    de$VMMCTechnique=""
    de$Age=""
    de$AggregatedAge=""
    de$AggregatedAgeFlag=""
    de$Sex=""
    de$HIVStatus=""
    de$otherDisagg=""
    de$IMPATT=""
    #Break out categoryOptionCombo for helpful referencing
    ##Modality
    de$Modality=ifelse(str_detect(de$COPdeName,"HTS_TST(.)+(Emergency_Ward_|HomeMod_|Index_|IndexMod_|Inpat_|Malnutrition_|MobileMod_|OtherMod_|OtherPITC_|Pediatric_|PMTCT_ANC_|STI_Clinic_|TBClinic_|VCT_|VCTMod_|VMMC_)")
                       ,str_extract(de$dataelement,"Emergency Ward|HomeMod|IndexMod|Index|Inpat|Malnutrition|MobileMod|OtherMod|OtherPITC|Pediatric|PMTCT ANC|STI Clinic|TBClinic|VCTMod|VCT|VMMC")
                       ,de$Modality)
    ##Age
    de$Age=ifelse(str_detect(de$COPdeName,"Age")
                  ,str_trim(str_extract(de$COPcocName,"(?<!D)(<|<=)?( )?\\d{1,2}( months)?(( )?-( )?\\d{1,2}( months| years)?|\\+)?|Unknown Age"))
                  ,de$Age)
    de$Age=ifelse(str_detect(de$dataelement,"HTS_TST(.)+(Malnutrition|Pediatric)")
                  ,"<5"
                  ,de$Age)
    ##AggregatedAge
    word=c("< 2 months","<= 2 months","2 - 12 months","<1","<5","1-9","2 months - 9 years","<10","10-14","<15","15-17","15-19","18-24","20-24","25-29","15-29","30-34","35-39","40-49","30-49","25-49","15\\+","25\\+","30\\+","50\\+","Unknown Age")
    tran=c(rep("<15",10),rep("15+",15),"Unknown Age")
    dict=data.frame(word,tran,stringsAsFactors = FALSE)
    de$AggregatedAge=de$Age
    for (k in 1:length(dict$word)){de$AggregatedAge<-str_replace_all(de$AggregatedAge,paste0("^",dict$word[k],"$"),dict$tran[k])}
    ##Aggregated Age Flag (to distinguish otherwise identical indicators)
    de$AggregatedAgeFlag=ifelse(str_detect(de$dataelement,"Age Aggregated|(PLHIV|HIV_PREV)(.)+Age")
                                ,"AgeAggregated"
                                ,de$AggregatedAgeFlag)
    ##Sex
    de$Sex=ifelse(str_detect(de$COPdeName,"Sex|HTS_TST(.)+VCT_Age_Result")
                  |str_detect(de$COPcocName,"Female PWID|Male PWID|MSM|FSW")
                  ,str_extract(de$COPcocName,"Male|Female|Unknown Sex|MSM|FSW")
                  ,de$Sex)
    dict=data.frame(word=c("MSM","FSW"),tran=c("Male","Female"),stringsAsFactors = FALSE)
    for (k in 1:length(dict$word)){de$Sex<-str_replace_all(de$Sex,dict$word[k],dict$tran[k])}
    rm(dict)
    de$Sex=ifelse(str_detect(de$dataelement,"HTS_TST|OVC_SERV|PMTCT_EID|TB_ART|TX_(CURR|NEW|PVLS|RET)") & str_detect(de$Age,"(<1|1-9|<5)$")
                  ,"Unknown Sex"
                  ,de$Sex)
    de$Sex=ifelse(str_detect(de$dataelement,"HTS_TST(.)+VMMC|VMMC_CIRC")
                  ,"Male"
                  ,de$Sex)
    de$Sex=ifelse(str_detect(de$dataelement,"HTS_TST(.)+PMTCT|PMTCT_ART|PMTCT_STAT")
                  ,"Female"
                  ,de$Sex)
    
    ##HIVStatus
    #            Convert to "HIV Positive", "HIV Negative", "HIV Unknown"
    de$HIVStatus=ifelse(str_detect(de$COPdeName,"_Result|_HIVStatus|_KnownNewResult|_KnownNewPosNeg|OVC_HIVSTAT_(.)+_ReportedStatus")
                        | (str_detect(de$COPdeName,"KP_PREV_(N|D)_(DSD|TA)_KeyPop_Status|PP_PREV_(N|D)_(DSD|TA)_Status")
                           & str_detect(de$COPcocName,"Positive|Negative"))
                        ,str_extract(de$COPcocName,"Positive|Negative|Unknown(?=($|,))|Undisclosed")
                        ,de$HIVStatus)
    de$HIVStatus=str_replace(de$HIVStatus,"Undisclosed","Unknown")
    de$HIVStatus=ifelse(str_detect(de$COPdeName,"^(IMPATT.PLHIV|PMTCT_ART|TB_ART|TB_PREV|TX_CURR|TX_NEW|TX_PVLS|TX_RET|TX_TB)|OVC_HIVSTAT_(.)+_StatusPosART")
                        & !(de$HIVStatus %in% ("Positive"))
                        ,"Positive"
                        ,de$HIVStatus)
    de$HIVStatus=ifelse(str_detect(de$COPdeName,"OVC_HIVSTAT(.)+StatusNotRep") & !(de$HIVStatus %in% ("Unknown"))
                        ,"Unknown"
                        ,de$HIVStatus)
    de$HIVStatus=ifelse(str_detect(de$COPdeName,"HTS_TST(.)+Positive"),"Positive",de$HIVStatus)
    ##KeyPop
    de$KeyPop=ifelse(str_detect(de$COPdeName,"_KeyPop")
                     ,str_extract(de$COPcocName,"FSW|MSM(( not)? SW)?|TG(( not)? SW)?|PWID|People in prisons and other enclosed settings|Other Key Populations")
                     ,de$KeyPop)
    ##KnownNewStatus
    de$KnownNewStatus=ifelse(str_detect(de$COPcocName,"Known at Entry|Newly Tested or Testing Referred|Newly Identified|Declined Testing Or Testing Referral")
                             ,str_extract(de$COPcocName,"Known|New|Declined")
                             ,de$KnownNewStatus)
    ##NewExistingART
    de$NewExistingART=ifelse(str_detect(de$COPdeName,regex("NewExistingArt",ignore_case=T))
                             ,str_extract(de$COPcocName,"New|Already")
                             ,de$NewExistingART)
    ##Indication
    de$Indication=ifelse(str_detect(de$COPdeName,"TX_PVLS(.)+(Indication|RoutineTargeted)")
                         ,str_extract(de$COPcocName,"Routine|Targeted|Undocumented Test Indication")
                         ,de$Indication)
    ##TBStatus
    de$TBStatus=ifelse(str_detect(de$COPdeName,"TX_NEW(.)+TB_Diagnosis|TB_STAT|TB_ART|TX_TB_(N|D(.)+(TBScreen|PositiveScreen))")
                       ,ifelse(str_detect(de$COPdeName,"TX_TB_D")
                               ,paste0("TB ",str_extract(de$COPcocName,"(?<=(TB Screen - |^))(Posi|Nega)tive"))
                               ,"TB Positive")
                       ,de$TBStatus)
    ##pregBF
    de$pregBF=ifelse(str_detect(de$dataelement,"PMTCT_(ART|STAT)|TX_(NEW|RET|PVLS)(.)+PregnantOrBreastfeeding")
                     ,ifelse(str_detect(de$dataelement,"PMTCT_(ART|STAT)")
                             ,"Pregnant"
                             ,str_extract(de$COPcocName,"Pregnant|Breastfeeding"))
                     ,de$pregBF)
    ##VMMCTechnique
    de$VMMCTechnique=ifelse(str_detect(de$dataelement,"VMMC_CIRC(.)+Tech")
                            ,str_extract(de$COPcocName,"Device based|Surgical Technique")
                            ,de$VMMCTechnique)
    ##otherDisagg
    pattern=paste(ifelse(de$KeyPop=="","0000",de$KeyPop)
                  ,ifelse(de$Sex=="","0000",de$Sex)
                  ,ifelse(de$Age=="","0000",paste0("\\Q",de$Age,"\\E"))
                  ,"TB Screen - (Posi|Nega)tive"
                  ,ifelse(de$HIVStatus=="","0000",de$HIVStatus)
                  ,"Declined Testing Or Testing Referral","Newly Tested or Testing Referred","Newly Identified","Known at Entry","Life-long ART"
                  ,ifelse(de$KnownNewStatus=="","0000",de$KnownNewStatus)
                  ,ifelse(de$NewExistingART=="","0000",de$NewExistingART)
                  ,ifelse(de$Indication=="","0000",de$Indication)
                  ,ifelse(de$pregBF=="","0000",de$pregBF)
                  ,ifelse(de$VMMCTechnique=="","0000",de$VMMCTechnique)
                  ,"default"
                  ,sep="|")
    de$otherDisagg=str_replace_all(de$COPcocName,pattern,"")
    de$otherDisagg=ifelse(str_detect(de$COPdeName,"TX_TB(.)+Specimen_Sent"),"Specimen Sent",de$otherDisagg)
    de$otherDisagg=str_trim(str_replace_all(de$otherDisagg,"^,( ,)?|(,( )?)+$|,( ,)+|\\(((,)?( )?)\\)",""))
    de$otherDisagg=ifelse(str_detect(de$dataelement, "TX_TB(.)+PositiveScreen")
                          ,str_replace(de$otherDisagg,"Negative","")
                          ,de$otherDisagg)
    
    ###Fix otherDisaggs where necessary
    ####TB_PREV(.)*TherapyType
    # word<-c('6-12 Month IPT','Alternative Regimen','Continuous IPT')
    # tran<-c('IPT','Alternative TPT Regimen','IPT')
    # for (k in 1:length(word)){de$otherDisagg=str_replace_all(de$otherDisagg,word[k],tran[k])}
    # rm(word,tran)
    de$otherDisagg=ifelse(str_detect(de$COPdeName,"TB_PREV"),
                          str_replace(de$otherDisagg,"Alternative Regimen","Alternative TPT Regimen"),
                          de$otherDisagg)
    ####GEND_GBV(.)*PEP
    de$otherDisagg=ifelse(str_detect(de$COPdeName,"GEND_GBV(.)+PEP"),"PEP",de$otherDisagg)
    ####HTS_TST
    ####TX_RET
    de$otherDisagg=ifelse(str_detect(de$COPdeName,"TX_RET(.)+")
                          ,ifelse(str_detect(de$COPdeName,"24mo|36mo"),str_extract(de$dataelement,"24mo|36mo"),"12mo")
                          ,de$otherDisagg)
    ####VMMC_CIRC
    de$otherDisagg=ifelse(str_detect(de$COPdeName,"VMMC_CIRC(.)+TechFollowUp")
                          ,"Followed up within 14 days"
                          ,de$otherDisagg)
    
    ##IMPATT
    de$IMPATT=de$pdLvl=="pd_IMPT_P"
    
    de$COPidName=paste(de$indicator,de$numeratorDenom,de$supportType,de$Modality,de$KeyPop,de$KnownNewStatus,de$NewExistingART,de$Indication,de$TBStatus,de$pregBF,de$VMMCTechnique,de$Age,de$AggregatedAge,de$AggregatedAgeFlag,de$Sex,de$HIVStatus,de$otherDisagg,sep="|")
    
    return(de)
  }
  
  TRANSFORM <- transformDEs(COPdataElements)
  
  #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  #III. Transpose and Compare dataElements #####
  #////////////////////////////////////////////////////
  
  COP18deMap = subset(TRANSFORM,
                      select=c(COPidName,indicator,numeratorDenom,supportType,Modality,KeyPop,KnownNewStatus,NewExistingART,Indication,TBStatus,pregBF,VMMCTechnique,Age,AggregatedAge,AggregatedAgeFlag,Sex,HIVStatus,otherDisagg,IMPATT
                               ,pdLvl,COPuid))
  
  COP18deMapT <- COP18deMap %>%
    spread(pdLvl,COPuid) %>%
    mutate(pd_2017 = case_when((is.na(pd_17_4_F) & !is.na(pd_17_4_C)) ~ pd_17_4_C,
                               (is.na(pd_17_4_F) & !is.na(pd_IMPT_P)) ~ pd_IMPT_P,
                               TRUE ~ pd_17_4_F),
           pd_2018 = case_when((is.na(pd_18_T_F) & !is.na(pd_18_T_C)) ~ pd_18_T_C,
                               (is.na(pd_18_T_F) & !is.na(pd_IMPT_P)) ~ pd_IMPT_P,
                               TRUE ~ pd_18_T_F),
           pd_2019_S = case_when(is.na(pd_19_T_F) ~ pd_19_T_C,
                                 TRUE ~ pd_19_T_F),
           pd_2019_P = case_when((is.na(pd_19_T_P) & !is.na(pd_IMPT_P)) ~ pd_IMPT_P,
                                 TRUE ~ pd_19_T_P)
    ) %>%
    select(-pd_17_4_C,-pd_17_4_F,-pd_18_T_C,-pd_18_T_F,-pd_19_T_C,-pd_19_T_F,-pd_19_T_P,-pd_IMPT_P) %>%
    gather(Period, pd_2017_2018_S, pd_2017, pd_2018) %>%
    mutate(FiscalYear=as.numeric(str_extract(Period,"\\d{4}"))) %>%
    select(-Period) %>%
    filter(!(is.na(pd_2019_S) & is.na(pd_2019_P) & is.na(pd_2017_2018_S))) %>%
    filter(!(IMPATT==TRUE & FiscalYear==2017))
  
  return(COP18deMapT)


}