% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{center_cols}
\alias{center_cols}
\title{A simple function to mean center variables}
\usage{
center_cols(data, variables, grouping)
}
\arguments{
\item{data}{The dataset to operate on.}

\item{variables}{The character vector of variables to lag}

\item{grouping}{The character vector of grouping variables}
}
\description{
This function will take a single variable and return the centered version of that variable. It can be used
with the grouping parameter set to 'person_id' to get person-centered variables.
}
