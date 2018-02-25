# Data Pack Importer

## Installation

You will need to install the package from source with the following commands.

### Ubuntu

Tested on 16.04

```bash
sudo apt-get install openssl-devel libcurl4-openssl-dev libxml2-dev
```

### Windows

todo


## Run

1. Download support files from [Sharepoint](https://www.pepfar.net/Project-Pages/collab-38/Shared%20Documents/Forms/AllItems.aspx?RootFolder=%2FProject-Pages%2Fcollab-38%2FShared%20Documents%2FCOP18%20Target%20Setting%20Process%20Improvement%2FImport%20Team&FolderCTID=0x012000C4AC9B35DC4AB84FAEEF47AE703A28CE00C799CA85D140EF45960B9C47CE99E19F&View=%7BA8BAC8D0-846B-4EFE-8763-758855081F5D%7D&InitialTabId=Ribbon%2EDocument&VisibilityContext=WSSTabPersistence#InplviewHasha8bac8d0-846b-4efe-8763-758855081f5d=RootFolder%3D%252FProject%252DPages%252Fcollab%252D38%252FShared%2520Documents%252FCOP18%2520Target%2520Setting%2520Process%2520Improvement%252FImport%2520Team)

2. Adjust `support_files` path below (it should be a path and ending with a `/`)

3. Download DisaggTool from Support Ticket, replace path to it in `disagg_tool`

4. Adjust `distribution_year` to either `2017` or `2018`. This should be indicated in the Support Ticket.

5. Run it. If successful, it outputs a Excel file in the same folder as `disagg_tool`.


```R
library(devtools)
library(datapackimporter)
library(jsonlite)
install_github("jason-p-pickering/data-pack-importer")
install_github("tkunstek/openxlsx")

# ADJUST PATHS
support_files="/path/to/support_files/"
disagg_tool="/path/to/disaggTool/ZambiaCOP18DisaggTool_HTSv2018.02.11.xlsx"
distribution_year=2017

# DO NOT CHANGE
wb<-disagg_tool
psnu_data<-ImportSheets(wb,
               distribution_method = distribution_year,
               support_files_path = support_files)
```

### Python alternative

This is not ready yet.

```R
```R
library(devtools)
library(datapackimporter)
library(jsonlite)
install_github("jason-p-pickering/data-pack-importer")
install_github("tkunstek/openxlsx")

# ADJUST PATHS
support_files="~/Github/datapack/"
psnu_json_file=paste0(folder, "zambia_psnu_data.json")
disagg_tool="/home/david/Pepfar/tests/ZambiaCOP18DisaggTool_HTSv2018.02.11.xlsx"
site_json_file="/home/david/Github/datapack-import-converter/data/zambia_site_data.json"
distribution_year=2017

# DO NOT CHANGE
wb<-disagg_tool
psnu_data<-ImportSheets(wb,
               distribution_method = distribution_year,
               support_files_path = support_files)

cat(toJSON(psnu_data, auto_unbox = TRUE), 
    file = psnu_json_file)
site_data<-distributeSite(psnu_data)
export_site_level_tool(site_data)

cat(toJSON(site_data, auto_unbox = TRUE), 
    file = site_json_file)
```
```