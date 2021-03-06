#' emacalc: A package for performing useful calculations in ESM/EMA data
#' 
#' The emacalc package was built to make it easier to work with experience sampling data. It contains functions
#' to easily calculate person-level and day-level variables, lag variables at different levels, split data into
#' person and moment level dataframes, and numerous other utility functions.
#'
#' @author Sean C Murphy, \email{seanchrismurphy@gmail.com}
#'
#'
#' @section Person aggregates:
#' 
#' Calculate aggregate statistics at the person level
#' 
#' Person aggregation functions:
#' 
#' \code{\link{person_mean}}, \code{\link{person_min}}, \code{\link{person_max}}, \code{\link{person_sd}}, \code{\link{person_relsd}}
#' 
#' @section Day aggregates:
#' 
#' Calculate aggregate statistics at the day level
#' 
#' \code{\link{day_mean}}, \code{\link{day_min}}, \code{\link{day_max}}, \code{\link{day_sd}}, \code{\link{day_relsd}}
#' 
#' @section SD Day functions:
#' Calculate standard deviation of day level statistics
#' 
#' \code{\link{day_mean_sd}}, \code{\link{day_min_sd}},\code{\link{day_max_sd}}
#' 
#' @section Rel SD Day functions:
#' Calculate relative standard deviation of day level statistics:
#' 
#' \code{\link{day_mean_relsd}}, \code{\link{day_min_relsd}},\code{\link{day_max_relsd}}
#' 
#' @section Lagging functions:
#' Lag variables at appropriate levels:
#' 
#' \code{\link{esm_lag}}, \code{\link{esm_day_lag}}
#' 
#' @section Splitting functions:
#' Split up complete datasets into subsets of data with only higher (e.g. trait) or lower (e.g. moment) level variables
#' 
#' \code{\link{keep_bottom}}, \code{\link{keep_top}}
#' 
#' @section Utilities:
#' Tools for managing operations on many variables
#' 
#' \code{\link{operate_cols}}, \code{\link{rename_cols}}, \code{\link{rescale_cols}}, \code{\link{reverse_cols}}, \code{\link{center_cols}}
#' 
#' @section Trimming functions:
#' tools to trim datasets to only groups with enough valid observations
#' 
#' \code{\link{trim_min_obs}} \code{\link{trim_min_valid_obs}}
#' 
#' @importFrom plyr join
#' @import dplyr
"_PACKAGE"