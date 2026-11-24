\name{validate_indicator_register}
\alias{validate_indicator_register}
\title{Validate an indicator register against analysis data}
\description{
Checks indicator identifiers, required fields, analysis types, supported summaries and referenced data columns.
}
\usage{
validate_indicator_register(register, data)
}
\arguments{
  \item{register}{A data frame with one row per indicator. Required columns are \code{indicator_id}, \code{indicator_label}, \code{variable}, \code{kobo_type}, \code{aggregation_method}, and \code{disaggregation}. Optional columns are \code{disaggregation_label}, \code{repeat_for}, \code{group}, \code{unit}, \code{numerator}, and \code{denominator}.}
  \item{data}{A data frame containing the variables referenced by the register.}
}
\value{
Returns \code{TRUE} invisibly when validation succeeds.
}
