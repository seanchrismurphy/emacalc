% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{day_min_relsd}
\alias{day_min_relsd}
\title{Calculates the (person level) relative standard deviation of daily minima of variables}
\usage{
day_min_relsd(data, variables, min, max, person_id, day_id)
}
\arguments{
\item{data}{The input dataset}

\item{variables}{A character vector of input variables e.g. c('negemo', 'posemo').}

\item{person_id}{The person level grouping variable - this should be your person id variable.}

\item{day_id}{The day level grouping variable - this should be your day id variable.}
}
\description{
This is a function that returns a dataframe in which the person-level relative standard deviation of the daily minimums
of any input variables has been calculated. Output variables will be named 'input_variable_day_min_relsd'.
Note carefully that this is -not- the min of the relative standard deviations within each day. Rather, the minimum
at each day is taken, then the relative standard deviation of these minima is calculated.
}
