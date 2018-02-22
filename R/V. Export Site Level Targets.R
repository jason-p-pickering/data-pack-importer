##################################
#Export to Site Level Review Tool#
##################################
pacman::p_load(stringr,tidyr,dplyr)

# source("data-raw/transform_code_lists.R")
# COP18deMapT<-generateCodeListT() %>%
#     mapDataPackCodes()
# load("R/sysdata.rda")

#Setup parameters####

    #DHIS2 authentication file path
        config_path="/Users/scott/.secrets/datim.json"
    #Operating unit. In this case, Malawi. 
        ou_uid<-"lZsCb6y0KDX"
        ou_name<-getOUname(ou_uid,config_path)
    #Location of Templates files
        topFolder="/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Site Review Export/Site Level Review Tool/"


#Load Mech and Site IDs in SLRT format####
    LoadDependencies()
    LoadConfig(config_path)
    mechs<-GetMechsForOU(ou_uid) %>% unique %>% arrange(mechanism)
    siteIDs<-GetPSNUsiteMap(ou_uid) %>% unique %>% arrange(DataPackSiteID)
    
    siteReviewHeaders<-getSiteReviewHeaders()


#Prepare Data Pack Data####
    #Need to stitch this up with prior steps
    dv<-dv %>%
        left_join(select(filter(COP18deMapT,FiscalYear==2018),pd_2019_P,FiscalYear,supportType,DataPackCode),by=c("pd_2019_P")) %>%
        select(-dataelement,-categoryoptioncombo,-FiscalYear,-pd_2019_P) %>%
        filter(!is.na(DataPackCode)) %>%
        #Pull in mechIDs and SiteIDs in Data Pack format
            left_join(mechs,by=c("attroptioncombo"="uid")) %>%
            left_join(siteIDs,by=c("orgUnit"="organisationunituid")) %>%
        select(DataPackSiteID,mechanism,supportType,DataPackCode,value) %>%
        left_join(select(siteReviewHeaders,-siteReviewLbl),.,by=c("DataPackCode")) %>%
        arrange(DataPackFilename,DataPackTabName,line,DataPackSiteID,mechanism,supportType,value) %>%
        select(DataPackFilename,DataPackTabName,DataPackSiteID,mechanism,supportType,DataPackCode,line,value)

#Write to SLRT Templates
    #Need to test this across other configurations
        dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/lib/server/libjvm.dylib'))
        library(XLConnect)
        options(java.parameters = "-Xmx2048m")
        xlcFreeMemory()
    
    #Write to Normal SLRT
        writeSiteLevelTool(topFolder,ou_uid,config_path,"DisaggTool")
        xlcFreeMemory()
    
    #Write to HTS SLRT
        writeSiteLevelTool(topFolder,ou_uid,config_path,"HTS_DisaggTool")
        xlcFreeMemory()