# Data Pack Importer

## Getting Started

You will need to install the package from source with the following commands

If you are using Ubuntu, you may need to install a few dependency system packages. 

```bash
sudo apt-get install openssl-devel libcurl4-openssl-dev libxml2-dev
```

Now open up a copy of R or RStudio and invoke the following commands in the R console:
```r
install.packages("devtools")
library("devtools")
install_github("jason-p-pickering/datim-validation")
install_github("jason-p-pickering/data-pack-importer")
```


Additional files will be required to perform cluster and site level distributions. 

Once you have downloaded these files and placed them all into a single directory, you should define an option in your R environment by invoking the following command: 

```r
options("datapack_distros"="/path/to/distribution_files")
```
