% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_mechs_by_code}
\alias{check_mechs_by_code}
\title{check_mechs_by_code(d,sheet_name,mechanisms)}
\usage{
check_mechs_by_code(d, wb_info, sheet_name)
}
\arguments{
\item{d}{Parsed data from a single import sheet}

\item{wb_info}{Workbook info}

\item{sheet_name}{Name of the sheet}
}
\value{
TRUE if no invalid mechanisms were found, otherwise issues a warning and returns FALSE.
}
\description{
check_mechs_by_code(d,sheet_name,mechanisms)
}
