% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Retrieve a selection of data from FARS files.}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A vector of character strings representing years,
four-digit format (i.e. 2013).}
}
\value{
This function returns a vector of data frames.
}
\description{
\code{fars_read_years} reads in a vector of character strings
representing years. It returns a vector of data frames with
the month and year of every accident in the corresponding year.
}
\details{
This function will raise an error if there is no file
that matches the year in \code{years}.
}
