#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#I. Set up distribution source files####
#////////////////////////////////////////////////////
#Paste file path to site-level export here:
    siteExportPath="/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Distribution Source Files/COP18DistributionSource_20180214_2.csv"
#Where to save distribution source files:
    sourceFolder="/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Distribution Source Files/"
    
#Distribution Sources
    siteExport<- read.csv(file=siteExportPath,stringsAsFactors=FALSE,header=TRUE)
    saveRDS(siteExport,paste0(sourceFolder,"siteExport.rda"))


distrSource <- function(df,FY=NULL) {
    
    #clusterMap<-datapackimporter::clusters
    #clusterMap<-clusters
    #rCOP18deMap<-datapackimporter::rCOP18deMap
    
    ds <- df %>%
        #Filter to specified Fiscal Year
            filter(FiscalYear==FY) %>%
        #Remove all zero and null values
            filter(Value !=0) %>%
            filter(!is.na(Value)) %>%
        #Remove data entered against non-Community/non-Facility hierarchy levels
            filter(PSNUuid != "") %>%
        #Remove "Sensitive Site" data
            filter(!orgUnit %in% c('fNH1Ny5vXI5','Tiqj6KDtx3p','BspXUn4c2i0','wnFyQ8gWVuP','b0WbjlNgwpg','Smw76afBRxh','TyDdI16aem2','u6UHEEYSsrY','ZHAEPwL6s87','oitze45vmuG','imQAg2FmqIi','JWb1FJrb6u0','oU9JrXHFBwo','ZvjmhaNkDJP','ph5hfp4TDYa','NDGAjm5He3s','S0wsB3mH7As','WKQumwV8vzz','aIl7B0aJZE7','EwvYCRwMaj2','Zj3QFD5LCN0','DWqxLhccQpN','FMA01mDjzg9','Wt4Ap0dVT0K','kTDYtuRlsRJ','B2aBYUFKEtP','eBMjxJa6Hyo','Jn8Dy8Kt8r6','BP8kSSf9mVh','uM7bKbyQMUb','xRNWRGhiL2x','CLsTOua0sYz','foN7Fc7qqd5','Pn5Egy0nEvw','ZU5YFwWSAM7','ahCpXE5nYKO','WQUnNhUravY','lSrgJWMVhKP','SWMW9b7WMMG','LdH3sTixu4G','PUWNeEDqKjG','kQLMdNG7tOr','qjxX1U1zOV9','un7KU5UBkTp','nMYhhbh463E','cugQdSJzIzf','Vgz3Af04heg','VXhW2lbMHeT','o1OrLbuDePL','gdWruPti7dW','kpLxWaoSWp5','GGNlHihWQLS','c78scqZGQPc','WXCDaZ8ldbb','DmpYVwgbt0k','kbLOPXlsHH4','KabE1XwF8CH','sk68oHctZOt','boqES0AhYHD','ecpaElyx1MZ','TDk0oLAqK6H','p3n96zLyWoP','hF8sLm9vE1U','t5GdyeN9riy','Fu0wZlUnntH','TixiR1SsebU','u86Kfypb8DG','JJJOwYzvDZo','Dgi2sUBjGzO','e9eJh4Dn286','dV6akh4l1Ej','I93yMz1rjkQ','TVrtknExg0t','FL40UCPHJke','WxIBVamFcg0','BpLP6v9NeWX','D7uuBfToHfb','ItoS9FGQg24','M8Yb2Y9rgNe','tBcAME3DNk1','jBOH9BBbqEW','J9Nmumn9DRc','sEJ8peJ3Jz6','g0HJxd9XWMy','tLcy3vpV6LF','QITi8Rd6xV5','zrHn3k5oIAT','szenMEdV4sF','EzzYi29hyNF','RJWMt1CU1HW','JSmcOMrC6zZ','RQykElqy1HR','Ae8uPosEFeF','NEk0GiXI2SW','HSoAojlwB7Q','hRq9qYMyBE7','Rq9EVeiR0PU','OyDnBG2RCgS','q3WGbWcjdWf','aGQbouk9S3E','GMHwNlqPAzS','m6eYOfLPzmF','lAhBMeGXsvQ','zZXWPXydW2S','VGVbROfDHWh','bMtviLCfDub','ZCbh020F2TA','cVnfnV5N1w5','L6HMMjCf2em','U9YejzJibuv','ASSntKFP1Ns')) %>%
        #Create COPuid for easy re-mapping
            mutate(pd_2017_2018_S=paste(dataElement,categoryOptionCombo,sep=".",collapse=NULL)) %>%
        #Pull in FY2019 PSNU-level dataElement and categoryOptionCombo codes
            merge(rCOP18deMap[rCOP18deMap$FiscalYear==FY,c("pd_2017_2018_S","pd_2019_P","FiscalYear")],by=c("pd_2017_2018_S")) %>%
            select(-FiscalYear.y,-FiscalYear.x,-dataElement,-categoryOptionCombo) %>%
        #Constrain to only those data values entered against dataElements mappable to FY19
            filter(!is.na(pd_2019_P)) %>%
        #Create new dataElement and categoryOptionCombo codes
            mutate(dataElement=str_extract(pd_2019_P,"^(.)+(?=(\\.))"),
                   categoryOptionCombo=str_extract(pd_2019_P,"(?<=(\\.))(.)+$")) %>%
            select(-pd_2019_P,-pd_2017_2018_S) %>%
            select(PSNUuid,attributeOptionCombo,dataElement,categoryOptionCombo,orgUnit,Value)
    
    return(ds)
}


    
    distrSourceFY17<-distrSource(siteExport,FY=2017)
        
    distrSourceFY18<-distrSource(siteExport,FY=2018)
    
    rm(siteExport)


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#II. PREPARE SITE-LEVEL DISTRIBUTION MATRICES####
#////////////////////////////////////////////////////
#No Dedupes
#Mapped to Cluster/PSNU -- not pure PSNU

siteDistribution<-function(df) {
    
    clusterMap<-datapackimporter::clusters
    
    siteDistr <- df %>%
        #Remove all dedupe data
            filter(!attributeOptionCombo %in% c("YGT1o7UxfFu","X8hrDf6bLDC")) %>%
        #Pull in ClusterUIDs
            left_join(clusterMap[,c("psnuuid","cluster_psnuuid")], by=c("PSNUuid"="psnuuid")) %>%
            mutate(dpOrgUnit=case_when(is.na(cluster_psnuuid)~PSNUuid
                                       ,TRUE~cluster_psnuuid)) %>%
            select(dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo,orgUnit,Value) %>%
        #Form percentage distributions across site within PSNU/Cluster x IM x DE.COC combination
            group_by(dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo) %>%
            mutate(sitePct = (Value)/sum(Value)) %>%
        #Create id to join with Data Pack dataset
            mutate(whereWhoWhatHuh=paste(dpOrgUnit,attributeOptionCombo,dataElement,categoryOptionCombo,sep=".")) %>%
        #Reorder columns; drop Value, keep percentages
            ungroup() %>%
            select(whereWhoWhatHuh,orgUnit,sitePct)
    
    return(siteDistr)
    
}

siteDistribution(distrSourceFY17)%>%
        saveRDS(paste0(sourceFolder,"distrSiteFY17.rda"))
    
siteDistribution(distrSourceFY18)%>%
    saveRDS(paste0(sourceFolder,"distrSiteFY18.rda"))


    
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#III. PREPARE CLUSTER-PSNU DISTRIBUTION MATRICES####
#////////////////////////////////////////////////////
#No Dedupes
#Includes clustered org units only

clusterDistribution <- function(df) {
    
    clusterMap<-datapackimporter::clusters
    
    clusterDistr <- df %>%
        #Sum up to PSNU level
            select(-orgUnit) %>%
            filter(!attributeOptionCombo %in% c("YGT1o7UxfFu","X8hrDf6bLDC")) %>%
            group_by(PSNUuid,attributeOptionCombo,dataElement,categoryOptionCombo) %>%
            summarise(psnuValue=sum(Value)) %>%
        #Merge in Cluster groupings
            ungroup() %>%
            left_join(clusterMap[,c("psnuuid","cluster_psnuuid")], by=c("PSNUuid"="psnuuid")) %>%
        #Filter to include only Clustered locations
            filter(!is.na(cluster_psnuuid)) %>%
        #Summarize by Cluster
            group_by(cluster_psnuuid,attributeOptionCombo,dataElement,categoryOptionCombo) %>%
            mutate(psnuPct = psnuValue/sum(psnuValue)) %>%
            ungroup() %>%
        #Create id to join with Data Pack dataset
            mutate(whereWhoWhatHuh=paste(cluster_psnuuid,attributeOptionCombo,dataElement,categoryOptionCombo,sep=".")) %>%
            select(whereWhoWhatHuh,PSNUuid,psnuPct)
    return(clusterDistr)
}



clusterDistribution(distrSourceFY17) %>%
    saveRDS(paste0(sourceFolder,"distrClusterFY17.rda"))

clusterDistribution(distrSourceFY18) %>%
    saveRDS(paste0(sourceFolder,"distrClusterFY18.rda"))

