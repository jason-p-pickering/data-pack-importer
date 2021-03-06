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
      "avg",
      "categoryoptioncombo",
      "combi",
      "closingCode",
      "Closing Out",
      "closingUID",
      "cluster_psnuuid",
      "code",
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
      "indicator",
      "match_code",
      "mechanism",
      "mech_code",
      "mechid",
      "Mechanism",
      "mechs",
      "Modality",
      "name",
      "num",
      "ok",
      "operatingUnitUID",
      "orgunit",
      "ou",
      "ou_uid",
      "pd_2019_P",
      "pd_2019_S",
      "pd_2019_P_de",
      "pd_2019_S_de",
      "period",
      "psnu",
      "psnuuid",
      "PSNUuid",
      "psnuPct",
      "psnu_type",
      "row_id",
      "sheet_name",
      "Site",
      "siteID",
      "siteType",
      "snu_priotization_fy19",
      "supportType",
      "Type",
      "type",
      "uid",
      "uidlevel3",
      "value",
      "value_start",
      "variable",
      "whereWhoWhatHuh",
      "period"
    )
  )
