# Data Pack Importer

## Installation

### Vagrant

Vagrant isolates all requirements into a Virtual Machine (Ubuntu). The "host" is your local computer while the "guest" is a isolated VM. Shared documents can only be transferred via the `/vagrant` folder in your repository.

This is meant to run on almost any system. However, you will need a substantial amount of RAM (4 GB) and disk space (5 GB).

1. Install [VirtualBox](https://www.virtualbox.org/wiki/Downloads).
2. Install [Vagrant](https://www.vagrantup.com/downloads).
3. Clone this repository (**use the `production` branch**)
4. Open a terminal and change to the folder from the last step.
5. `vagrant box add ubuntu/xenial64`
6. `vagrant up` (this takes a while if doing it for the first time)
7. Open web browser: http://localhost:8787, username: `vagrant` password: `vagrant`
8. Shared files live in the `/vagrant` folder (next to the `Vagrantfile` where you cloned this repo)

## Run

1. Download support files from [Sharepoint](https://www.pepfar.net/Project-Pages/collab-38/Shared%20Documents/Forms/AllItems.aspx?RootFolder=%2FProject-Pages%2Fcollab-38%2FShared%20Documents%2FCOP18%20Target%20Setting%20Process%20Improvement%2FImport%20Team&FolderCTID=0x012000C4AC9B35DC4AB84FAEEF47AE703A28CE00C799CA85D140EF45960B9C47CE99E19F&View=%7BA8BAC8D0-846B-4EFE-8763-758855081F5D%7D&InitialTabId=Ribbon%2EDocument&VisibilityContext=WSSTabPersistence#InplviewHasha8bac8d0-846b-4efe-8763-758855081f5d=RootFolder%3D%252FProject%252DPages%252Fcollab%252D38%252FShared%2520Documents%252FCOP18%2520Target%2520Setting%2520Process%2520Improvement%252FImport%2520Team) and place it into `/path/to/repo/vagrant/support_files`

2. Download DisaggTool from Support Ticket and put it into `/path/to/repo/vagrant/disagg_tools/`

3. Adjust `distribution_year` to either `2017` or `2018`. This should be indicated in the Support Ticket.

4. Paste it into the RStudio console in http://localhost:8787. If successful, it outputs a Excel file in the `vagrant` folder.


```R
# ADJUST THIS
disagg_tool_file="ZambiaCOP18DisaggTool_HTSv2018.02.11.xlsx"
distribution_year=2017

# DO NOT CHANGE
support_files="/vagrant/support_files/"
disagg_tool=paste0("/vagrant/disagg_tools/", disagg_tool_file)
library(devtools)
library(datapackimporter)
wb<-disagg_tool
psnu_data<-ImportSheets(wb,
               distribution_method = distribution_year,
               support_files_path = support_files)
```

### Other

- To stop vagrant: `vagrant halt` in the host machine
- To update vagrant with code changes from the `data-pack-importer` repository, call either: `install_github(repo="jason-p-pickering/data-pack-importer", branch="production")` in the R console, or run `vagrant up --provision` from the host machine.
