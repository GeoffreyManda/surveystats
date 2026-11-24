\name{make_survey_design}
\alias{make_survey_design}
\title{Create a stratified cluster survey design}
\description{
Stores analysis data with case weights, strata and primary sampling units. PSU identifiers are read within strata. When \code{weight} is \code{NULL}, observations receive equal weights. When \code{psu} is \code{NULL}, each row is treated as its own primary sampling unit.
}
\usage{
make_survey_design(data, weight = NULL, strata = NULL, psu = NULL)
}
\arguments{
  \item{data}{A non-empty data frame containing the analysis variables.}
  \item{weight}{A single column name containing finite, non-negative numeric case weights, or \code{NULL} for equal weights.}
  \item{strata}{A single column name identifying sampling strata, or \code{NULL} for one stratum.}
  \item{psu}{A single column name identifying primary sampling units, or \code{NULL} to treat each observation as a primary sampling unit.}
}
\value{
An object of class \code{surveystats_design} for use with \code{\link{run_indicator_register}}.
}
