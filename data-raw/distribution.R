#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#I. Set up distribution source files####
#////////////////////////////////////////////////////
#Paste file path to site-level export here:
    siteExportPath=paste0(GoogleDrive,"/Facility Distribution Script/Distribution Source Files/COP18DistributionSource_20180214_2.csv")
#Where to save distribution source files:
    sourceFolder="/Users/scott/Google Drive/PEPFAR/COP Targets/Development/Facility Distribution Script/Distribution Source Files/"
    
    
    
#Distribution Sources
    siteExport<- read.csv(file=siteExportPath,stringsAsFactors=FALSE,header=TRUE)
    
    distrSourceFY17<-distrSource(siteExport,FY=2017)
        
    distrSourceFY18<-distrSource(siteExport,FY=2018)
    
    rm(siteExport)


#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#II. PREPARE SITE-LEVEL DISTRIBUTION MATRICES####
#////////////////////////////////////////////////////
#No Dedupes
#Mapped to Cluster/PSNU -- not pure PSNU
siteDistribution(distrSourceFY17)%>%
        saveRDS(paste0(sourceFolder,"distrSiteFY17.rda"))
    
siteDistribution(distrSourceFY18)%>%
    saveRDS(paste0(sourceFolder,"distrSiteFY18.rda"))


    
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
#III. PREPARE CLUSTER-PSNU DISTRIBUTION MATRICES####
#////////////////////////////////////////////////////
#Contains Dedupes
#Includes clustered org units only
clusterDistribution(distrSourceFY17) %>%
    saveRDS(paste0(sourceFolder,"distrClusterFY17.rda"))

clusterDistribution(distrSourceFY18) %>%
    saveRDS(paste0(sourceFolder,"distrClusterFY18.rda"))

