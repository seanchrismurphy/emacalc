options(tibble.width = Inf)

require(plyr); require(dplyr)

# The simple, working version. 
#' @export
keep_top <- function(data, grouping) {
  
  # Select the relevant column names. Note that I didn't have .dots before, and again it needs it - without
  # them it just uses the first grouping variable you pass. 
  chosen <- group_by_(data, .dots = grouping) %>%
    summarize_all(n_distinct) %>%
    select_if(function(x) all(x == 1)) %>%
    colnames(.)
  
  data <- data %>%
    # Have to explicitly add back in the grouping variable(s). Note that when using select_, you need to
    # explicitly call the .dots argument and then your vector of variable names (potentially as objects). 
    # If you just put them in or try to name them or something, it doesn't work. Of course, the NSE vignette
    # only shows how it works with summarize and none of the other 5 functions, because why would you? 
    
    # However, if you call select_if directly you can see that it calls select_(.data, .dots = vars) so that
    # makes sense. 
    select_(.dots = c(grouping, chosen)) %>%
    group_by_(grouping) %>%
    slice(1) %>%
    as.data.frame()
  return(data)
}

# So for keep bottom, we just want an easy way to strip out everything above the level we're interested
# in. Then we can keep_upper before drilling down further, if need be. 
#' @export
keep_bottom <- function(data, grouping) {
  
  n_levels = length(grouping)
  
  if (n_levels < 2) {
    stop("keep_bottom shouldn't be called with a single grouping variable. Did you mean to use keep_top?")
  }
  
  # Saving code by calling keep_top from here. We're essentially finding what variables are aggregatable
  # at one level above the lowest level we're calling. 
  upper <- keep_top(data, grouping[0:(n_levels - 1)])
  
  # Then we strip those variables out of the returning dataset
  data <- data[, unique(c(grouping, setdiff(colnames(data), colnames(upper))))]
  return(data)
}


# Ok so after substantial work in the new function scratchpad, I have figured out NSE and realised I 
# mostly need mutate_at and really only need nse for grouping variables. That said, the functions I believe
# I need in order to chain together some really powerful operators are:

# Calculate SD and relative SD (given some arbritrary level of grouping)

# Calculate max, min and mean (given some arbritrary level of grouping). 

# I realise saying that that maybe that's all I need? 

# Take one example workflow. I want to be able to take the max at the day level, then calculate the sd
# of that max and have that go back into the original dataframe. 

### Ok. So I realised that the one thing I can't easily do with mutate_at at the moment is specify
### the output variables programatically, at least I think so. Having looked through it extensively it doesn't
### seem worth learning the entire underlying lazyinterp framework just to save myself writing a few extra
### functions. so I'm going to try to do the smart thing and just write the functions I'm going to need in this
### package. I can easily just write some functions for the person level and the day level, which are the most
### common use cases. 


# So because I can't specify the names of the output, I have to create a function for each use case, but
# it's not too bad. What I have ended up creating is:

# Functions to calculate mean, min, max, sd, and relative sd at the person level, and separately at the
# day level. 

# Functions to calculate the variability (sd or relativesd) of daily summary statistics (mean, min, max)

# I realise now that I probably won't use the functions to calculate, say, mean at the day level specifically, 
# not for the general variability hypothesis. Because we have specific chained functions for that. However,
# We can use it for the daily variability hypothesis - variability at the day level predicting mean negative
# emotion next day. For that we'll need lagging functions. 


# So here's person mean and day mean. I mean I could let the functions do either, and just have a parameter
# for person or day. But this seems the right level of abstraction. 

#' @export
person_mean <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at(vars, funs('mean' = mean(., na.rm = TRUE))) %>%
    as.data.frame()
  out
}

#' @export
day_mean <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_each_( vars, funs('day_mean' = mean(., na.rm = TRUE))) %>%
    as.data.frame()
  out
}

#' @export
person_max <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( vars, funs('max' = max(., na.rm = TRUE))) %>%
    as.data.frame()
  out
}

#' @export
day_max <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( vars, funs('day_max' = max(., na.rm = TRUE))) %>%
    as.data.frame()
  out
}

#' @export
person_min <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( vars, funs('min' = min(., na.rm = TRUE))) %>%
    as.data.frame()
  out
}

#' @export
day_min <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( vars, funs('day_min' = min(., na.rm = TRUE))) %>%
    as.data.frame()
  out
}

#' @export
person_sd <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( vars, funs('sd' = sd(., na.rm = TRUE))) %>%
    as.data.frame()
  out
}

#' @export
day_sd <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( vars, funs('day_sd' = sd(., na.rm = TRUE))) %>%
    as.data.frame()
  out
}

#' @export
person_rel_sd <- function(data, grouping, vars, min, max) {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( vars, funs('rel_sd' = relativeSD(., MIN = min, MAX = max))) %>%
    as.data.frame()
  out
}

#' @export
day_rel_sd <- function(data, grouping, vars, min, max) {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( vars, funs('day_rel_sd' = relativeSD(., MIN = min, MAX = max))) %>%
    as.data.frame()
  out
}


### So what I have ended up doing is creating functions to calculate the sd and relative sd of each of 
### daily max, min, and mean. 
#' @export
sd_of_day_mean <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    # Summarize to the means of each (probably) day
    summarize_at(vars, funs(mean(., na.rm = TRUE))) %>%
    # Calcualte the SD for each participant, which is another summarize call I realise. 
    summarize_at(vars, funs('sd_of_day_mean' = sd(., na.rm = TRUE))) %>%
    # Join that one score per P back into the dataset.
    join(data, .) %>%
    as.data.frame()
  return(out)
}

#' @export
relativeSD_of_day_mean <- function(data, grouping, vars, min, max) {
  require(relativeVariability)
  out <- group_by_(data, .dots = grouping) %>%
    # Summarize to the means of each (probably) day
    summarize_at(vars, funs(mean(., na.rm = TRUE))) %>%
    # Calcualte the SD for each participant, which is another summarize call I realise. 
    summarize_at(vars, funs('relative_sd_of_day_mean' = relativeSD(., min = min, max = max))) %>%
    # Join that one score per P back into the dataset.
    join(data, .) %>%
    as.data.frame()
  return(out)
}

#' @export
sd_of_day_max <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    # Summarize to the means of each (probably) day
    summarize_at(vars, funs(max(., na.rm = TRUE))) %>%
    # Calcualte the SD for each participant, which is another summarize call I realise. 
    summarize_at(vars, funs('sd_of_day_max' = sd(., na.rm = TRUE))) %>%
    # Join that one score per P back into the dataset.
    join(data, .) %>%
    as.data.frame()
  return(out)
}

#' @export
relativeSD_of_day_max <- function(data, grouping, vars, min, max) {
  require(relativeVariability)
  out <- group_by_(data, .dots = grouping) %>%
    # Summarize to the means of each (probably) day
    summarize_at(vars, funs(max(., na.rm = TRUE))) %>%
    # Calcualte the SD for each participant, which is another summarize call I realise. 
    summarize_at(vars, funs('relative_sd_of_day_max' = relativeSD(., min = min, max = max))) %>%
    # Join that one score per P back into the dataset.
    join(data, .) %>%
    as.data.frame()
  return(out)
}

#' @export
sd_of_day_min <- function(data, grouping, vars) {
  out <- group_by_(data, .dots = grouping) %>%
    # Summarize to the means of each (probably) day
    summarize_at(vars, funs(min(., na.rm = TRUE))) %>%
    # Calcualte the SD for each participant, which is another summarize call I realise. 
    summarize_at(vars, funs('sd_of_day_min' = sd(., na.rm = TRUE))) %>%
    # Join that one score per P back into the dataset.
    join(data, .) %>%
    as.data.frame()
  return(out)
}

#' @export
relativeSD_of_day_min <- function(data, grouping, vars, min, max) {
  require(relativeVariability)
  out <- group_by_(data, .dots = grouping) %>%
    # Summarize to the means of each (probably) day
    summarize_at(vars, funs(min(., na.rm = TRUE))) %>%
    # Calcualte the SD for each participant, which is another summarize call I realise. 
    summarize_at(vars, funs('relative_sd_of_day_min' = relativeSD(., min = min, max = max))) %>%
    # Join that one score per P back into the dataset.
    join(data, .) %>%
    as.data.frame()
  return(out)
}


# So this function will lag any variable based off grouping inserted. I'll probably use it moment level 
# to calculate inertia but on the calculated day level variables to do daily stuff. It should work equally
# well on both since those are named differently. 

#' @export
esm_lag <- function(data, grouping, vars, order = 'obs_id') {
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at(vars, funs('lag' = lag(., order_by = order))) %>%
    as.data.frame()
}
