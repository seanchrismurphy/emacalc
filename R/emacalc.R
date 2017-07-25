#' emacalc: A package for performing useful calculations in ESM/EMA data
#' 
#' The emacalc package was built to make it easier to work with experience sampling data. It contains functions
#' to easily calculate person-level and day-level variables, lag variables at different levels, split data into
#' person and moment level dataframes, and numerous other utility functions.
#'
#' @author Sean C Murphy, \email{seanchrismurphy@gmail.com}
#'
#' @importFrom plyr join
#' @import dplyr
#' @import relativeVariability
#' 
#' @section Person aggregates:
#' 
#' Calculate aggregate statistics at the person level
#' 
#' Person aggregation functions

#' \code{\link{emacalc::person_mean}}

#' 
#' \code{person_min}
#' \code{person_max}
#' \code{person_sd}
#' \code{person_relsd}
#' 
#' @section Day aggregates:
#' 
#' Calculate aggregate statistics at the day level
#' 
#' \code{day_mean}, \code{day_min}, \code{day_max}, \code{day_sd}, \code{day_relsd}
#' 
#' @section SD Day functions:
#' Calculate standard deviation of day level statistics
#' 
#' \code{day_mean_sd}, \code{day_min_sd},\code{day_max_sd}
#' 
#' @section Rel SD Day functions:
#' Calculate relative standard deviation of day level statistics:
#' 
#' \code{day_mean_relsd}, \code{day_min_relsd},\code{day_max_relsd}
#' 
#' @section Lagging functions:
#' Lag variables at appropriate levels:
#' 
#' \code{esm_lag}, \code{esm_day_lag}
#' 
#' @section Splitting functions:
#' Split up complete datasets into subsets of data with only higher (e.g. trait) or lower (e.g. moment) level variables
#' 
#' \code{keep_bottom}, \code{keep_top}
#' 
#' @section Utilities:
#' Tools for managing operations on many variables
#' 
#' \code{operate_cols}, \code{rename_cols}, \code{rescale_cols}, \code{reverse_cols}
#' 
#' @section Trimming functions:
#' tools to trim datasets to only groups with enough valid observations
#' 
#' \code{trim_min_obs} \code{trim_min_valid_obs}
"_PACKAGE"