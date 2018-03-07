#' \code{datapackimporter} package
#'
#' Parsing and import functions for the ICPI Data Pack
#' https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
#' @docType package
#' @name datapackimporter
#' @importFrom dplyr %>%
#' @importFrom plyr mapvalues
#' @importFrom tidyr separate
#' @importFrom stats na.omit
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c( ".",
      "attributeoptioncombo",
      "categoryoptioncombo",
      "combi",
      "closingCode",
      "Closing Out",
      "closingUID",
      "cluster_psnuuid",
      "dataelement",
      "DataPackCode",
      "DataPackSiteID",
      "DataPackSiteUID",
      "den",
      "distributed",
      "Follow on",
      "followOnCode",
      "followOnUID",
      "Inactive",
      "match_code",
      "mechanism",
      "mechanisms",
      "mechid",
      "Mechanism",
      "name",
      "num",
      "ok",
      "orgunit",
      "ou",
      "ou_uid",
      "pd_2019_P",
      "pd_2019_S",
      "period,",
      "psnuuid",
      "PSNUuid",
      "sheet_name",
      "Site",
      "sites",
      "siteID",
      "siteType",
      "snu_priotization_fy19",
      "supportType",
      "Type",
      "type",
      "uid",
      "uidlevel3",
      "value",
      "variable",
      "whereWhoWhatHuh",
      "period"
    )
  )
