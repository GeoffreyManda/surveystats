\name{run_indicator_register}
\alias{run_indicator_register}
\title{Analyse a survey using an indicator register}
\description{
Computes weighted estimates from a \code{surveystats_design} object. Choice indicators return a row for each response category. Integer indicators return the requested summary. Register disaggregation and \code{repeat_for} columns divide estimates into domains.
}
\usage{
run_indicator_register(
  design,
  register,
  data = design$data,
  result_format = "proportion",
  digits = 1,
  multi_response_sep = ";",
  deff = FALSE,
  ...
)
}
\arguments{
  \item{design}{A \code{surveystats_design} object.}
  \item{register}{A validated indicator register. Optional metadata columns \code{unit}, \code{numerator}, and \code{denominator} are copied to each result row.}
  \item{data}{The data frame used to create \code{design}; defaults to the design data.}
  \item{result_format}{Either \code{proportion} (0 to 1) or \code{percent} (0 to 100) for choice indicators.}
  \item{digits}{Number of decimal places used when \code{result_format} is \code{percent}.}
  \item{multi_response_sep}{Separator used for delimited select-multiple response columns.}
  \item{deff}{If \code{TRUE}, add design-effect and effective-sample-size columns.}
  \item{...}{Reserved for future arguments. Supplying values raises an error.}
}
\value{
A data frame with one row per indicator response or numeric summary and domain. Proportion, mean and total standard errors use stratified PSU Taylor linearization. Their intervals are normal 95\% intervals. Weighted quantiles and extrema have point estimates only. A singleton-PSU stratum yields missing standard errors and intervals.
}
