% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HelperFunctions.R
\name{schmidtNum}
\alias{schmidtNum}
\title{Title Returns Schmidt number for a specific gas at a given temperature}
\usage{
schmidtNum(tempC, gas, method)
}
\arguments{
\item{tempC}{Numeric vector of temperature in degrees Celsius}

\item{gas}{Character string for gas code. Supoported gases are Ar, O2, CO2, SF6}

\item{method}{Character string to choose coeffecients from Raymond 2012 or Wanninkof 2014}
}
\value{
Schmidt number (unitless)
}
\description{
Title Returns Schmidt number for a specific gas at a given temperature
}
\note{
Temperature range is only valid from 4-35 deg Celsius
}
