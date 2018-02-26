# Data Pack Importer

## Installation

See [data-pack-import-vagrant](https://github.com/davidhuser/data-pack-importer-vagrant) to set up a machine with Vagrant.

If you want to install all things manually (not recommended) follow these steps.

On Ubuntu, you need the following packages:

```bash
sudo apt install libcurl4-gnutls-dev libxml2-dev libcurl4-openssl-dev libssl-dev
```

- Install R (tested on `rstudio-server-1.1.423`)
- Install R packages:

```R
install.packages("devtools")
library(devtools)
install_github(repo="jason-p-pickering/datim-validation")
install_github(repo="jason-p-pickering/data-pack-importer", ref="prod")
install_github(repo="jason-p-pickering/openxlsx", ref="26a6f60")
```

