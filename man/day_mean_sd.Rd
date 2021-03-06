% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{day_mean_sd}
\alias{day_mean_sd}
\title{Calculates the (person level) standard deviation of daily means of variables}
\usage{
day_mean_sd(data, variables, person_id, day_id)
}
\arguments{
\item{data}{The input dataset}

\item{variables}{A character vector of input variables e.g. c('negemo', 'posemo').}

\item{person_id}{The person level grouping variable - this should be your person id variable.}

\item{day_id}{The day level grouping variable - this should be your day id variable.}
}
\description{
This is a function that returns a dataframe in which the person-level standard deviation of the daily means
of any input variables has been calculated. Output variables will be named 'input_variable_day_mean_sd'.
Note carefully that this is -not- the mean of the standard deviations within each day. Rather, the mean
at each day is taken, then the standard deviation of these means is calculated.
}
