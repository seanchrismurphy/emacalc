options(tibble.width = Inf)
require(plyr); require(dplyr); require(relativeVariability)

#' Keeps only variables that do not vary below the level of the grouping variables
#' 
#' Effectively, this function keeps variables that are 'at the level of' the grouping level specified. For
#' example, if person_id was specified, it would keep only variables that do not vary below the person
#' level (i.e. trait level variables). Grouping variables are always retained. If person_id and daynr were specified, it would keep only variables
#' that do not vary within day (i.e. day level variables). 
#' @param data The input dataset
#' @param grouping The grouping variables that determine the level below which variables should not vary if
#' they are to be kept.
#' 
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
    
    # However, if you call select_if directly you can see that it calls select_(.data, .dots = variables) so that
    # makes sense. 
    select_(.dots = c(grouping, chosen)) %>%
    # Fixed a bug where this didn't have .dots - it wouldn't have worked to keep anything at the day level. Didn't see it because
    # I've only been using it to keep at the person level. 
    group_by_(.dots = grouping) %>%
    slice(1) %>%
    as.data.frame()
  return(data)
}



#' Keeps only variables that -do- vary within the grouping variables
#' 
#' Effectively, this function keeps variables that are -not- invariant below the grouping level specified. For
#' example, if person_id was specified, it would keep only variables that do vary below the person
#' level. It does this by calling keep_top and excluding anything that does not vary below the person level
#' (i.e. trait level variables). If person_id and daynr were specified, it would keep only variables
#' that vary below the day level (i.e. momentary variables). Grouping variables are always retained. 
#' 
#' @param data The input dataset
#' @param grouping The grouping variables that determine the level at which variables should not vary if
#' they are to be kept.
#' 
#' @export
keep_bottom <- function(data, grouping) {
  
  # Saving code by calling keep_top from here. We're essentially finding what variables don't vary at the 
  # level we're calling, and removing those. 
  upper <- keep_top(data, grouping)
  
  # Then we strip those variables out of the returning dataset
  data <- data[, unique(c(grouping, setdiff(colnames(data), colnames(upper))))]
  return(data)
}


#' Calculates the mean of variables at the person level
#' 
#' This is a simple function that returns a dataframe in which the person-level mean of any input variables
#' has been calculated. Output variables will be named 'input_variable_mean'
#' 
#' Technically, it will calculate the mean at any level, but I have used simple dplyr
#' to set the output to 'original_variable_name_mean'. It is technically possible to write functions that
#' flexible assign the output name so a simple 'esm_mean' function could be created to work at any level and
#' allow the user to specify output variable names. But I made a design decision to be moderately constrictive
#' here for ease of use and also to ensure that variables are consistently named coming out of these functions.
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable defaults to 'person_id'
#' 
#' @export
person_mean <- function(data, variables, grouping = c('person_id')) {
  
  # Set up the expression for mutate_at which ensures that the output variables are named consistently regardless of if there's only one. This could look a bit
  # opaque, and the explanation is a little complex. Essentiallys mutate_at calls funs to create the appropriate expressions to mutate. The way that it works, 
  # there's no way to programatically specify output variable names apart from putting 'mean', which will append mean if there are more than one variables, so as
  # to delineate them, but will just call it 'mean' if there's only one. I had thought this was a hard limit, but my understanding of how the lazy evaluation works, 
  # though still incomplete, has expanded. There is a version of funs (called funs_) for NSE. It calls lazyeval differently in complex ways, but what it works out to
  # is that you can pass a list of arguments (set up with setNames for ease) to funs_ and it will craft them as expected. So the below expression says to craft a list
  # of arguments of length variables, each of which says 'varx_mean' = mean(., na.rm = TRUE). That means they will be consistently named. The flexibility of this approach
  # means that in theory it could be used to craft a mega function where the user could specify any grouping level, any function to be used, and any output name, 
  # essentially encapsulating all the functions I've written in this section. However, this would be quite unweildy and somewhat default the purpose of making functions
  # simple enoguh to easily fit in a lapply call, so for now I am just using it to ensure output consistency. 
  
  # Also note that setnames in the way that I had done it (with paste) only works with single variables, because mutate_at multiples variables and funs in 
  # every possible combination. The below if statement concisely handles this.
  
  if (length(variables) == 1) {
    mydots = setNames(rep('mean(., na.rm = TRUE)', length(variables)), paste0(variables, '_person_mean'))
  } else {
    mydots = setNames('mean(., na.rm = TRUE)', 'person_mean')
  }
  
  
  out <- group_by_(data, .dots = grouping) %>%
    # Use funs_ instead of funs, so that we can use mydots. 
    mutate_at(variables, funs_(mydots)) %>%
    as.data.frame()
  out
}

#' Calculates the mean of variables at the day level
#' 
#' This is a simple function that returns a dataframe in which the day-level mean of any input variables
#' has been calculated. Output variables will be named 'input_variable_day_mean'
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping A character vector of grouping variables at which you want to calculate the mean. This should be your person id variable followed by your day variable.
#' defaults to c('person_id', 'daynr')
#' @export
day_mean <- function(data, variables, grouping = c('person_id', 'daynr')) {

  if (length(variables) == 1) {
    mydots = setNames(rep('mean(., na.rm = TRUE)', length(variables)), paste0(variables, '_day_mean'))
  } else {
    mydots = setNames('mean(., na.rm = TRUE)', 'day_mean')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at(variables, funs_(mydots)) %>%
    as.data.frame()
  out
}

#' Calculates the max of variables at the person level
#' 
#' This is a simple function that returns a dataframe in which the person-level max of any input variables
#' has been calculated. Output variables will be named 'input_variable_max'
#' 
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable defaults to 'person_id'
#' 
#' @export
person_max <- function(data, variables, grouping = c('person_id')) {
  
  if (length(variables) == 1) {
    mydots = setNames(rep('max(., na.rm = TRUE)', length(variables)), paste0(variables, '_person_max'))
  } else {
    mydots = setNames('max(., na.rm = TRUE)', 'person_max')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( variables, funs_(mydots)) %>%
    as.data.frame()
  out
}

#' Calculates the max of variables at the day level
#' 
#' This is a simple function that returns a dataframe in which the day-level max of any input variables
#' has been calculated. Output variables will be named 'input_variable_day_max'
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping A character vector of grouping variables at which you want to calculate the max. This should be your person id variable followed by your day variable.
#' defaults to c('person_id', 'daynr')
#' @export
day_max <- function(data, variables, grouping = c('person_id', 'daynr')) {
  
  if (length(variables) == 1) {
    mydots = setNames(rep('max(., na.rm = TRUE)', length(variables)), paste0(variables, '_day_max'))
  } else {
    mydots = setNames('max(., na.rm = TRUE)', 'day_max')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( variables, funs_(mydots)) %>%
    as.data.frame()
  out
}

#' Calculates the min of variables at the person level
#' 
#' This is a simple function that returns a dataframe in which the person-level min of any input variables
#' has been calculated. Output variables will be named 'input_variable_max'
#' 
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable defaults to 'person_id'
#' 
#' @export
person_min <- function(data, variables, grouping = c('person_id')) {
  
  if (length(variables) == 1) {
    mydots = setNames(rep('min(., na.rm = TRUE)', length(variables)), paste0(variables, '_person_min'))
  } else {
    mydots = setNames('min(., na.rm = TRUE)', 'person_min')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( variables, funs_(mydots)) %>%
    as.data.frame()
  out
}

#' Calculates the min of variables at the day level
#' 
#' This is a simple function that returns a dataframe in which the day-level min of any input variables
#' has been calculated. Output variables will be named 'input_variable_day_min'
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping A character vector of grouping variables at which you want to calculate the min. This should be your person id variable followed by your day variable.
#' defaults to c('person_id', 'daynr')
#' @export
day_min <- function(data, variables, grouping = c('person_id', 'daynr')) {
  
  if (length(variables) == 1) {
    mydots = setNames(rep('min(., na.rm = TRUE)', length(variables)), paste0(variables, '_day_min'))
  } else {
    mydots = setNames('min(., na.rm = TRUE)', 'day_min')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( variables, funs_(mydots)) %>%
    as.data.frame()
  out
}

#' Calculates the standard deviation of variables at the person level
#' 
#' This is a simple function that returns a dataframe in which the person-level standard deviation of any input variables
#' has been calculated. Output variables will be named 'input_variable_sd'
#' 
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable defaults to 'person_id'
#' 
#' @export
person_sd <- function(data, variables, grouping = c('person_id')) {
  
  # Note. I had to call the packages for sd and relative sd otherwise the dplyr fun_ evaluation seems only to check base for functions and then give up, 
  # or else there's some other conflict. BUt it works like this. 
  if (length(variables) == 1) {
    mydots = setNames(rep('stats::sd(., na.rm = TRUE)', length(variables)), paste0(variables, '_person_sd'))
  } else {
    mydots = setNames('stats::sd(., na.rm = TRUE)', 'person_sd')
  }
  
  
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( variables, funs_(mydots)) %>%
    as.data.frame()
  out
}

#' Calculates the sd of variables at the day level
#' 
#' This is a simple function that returns a dataframe in which the day-level sd of any input variables
#' has been calculated. Output variables will be named 'input_variable_day_sd'
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping A character vector of grouping variables at which you want to calculate the sd. This should be your person id variable followed by your day variable.
#' defaults to c('person_id', 'daynr')
#' @export
day_sd <- function(data, variables, grouping = c('person_id', 'daynr')) {
  
  if (length(variables) == 1) {
    mydots = setNames(rep('stats::sd(., na.rm = TRUE)', length(variables)), paste0(variables, '_day_sd'))
  } else {
    mydots = setNames('stats::sd(., na.rm = TRUE)', 'day_sd')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( variables, funs_(mydots)) %>%
    as.data.frame()
  out
}


#' Calculates the relative standard deviation of variables at the person level
#' 
#' This is a simple function that returns a dataframe in which the person-level relative standard deviation of any input variables
#' has been calculated. Output variables will be named 'input_variable_relsd'
#' 
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable defaults to 'person_id'
#' 
#' @export
person_relsd <- function(data, variables, grouping = c('person_id'), min, max) {
  
  
  if (length(variables) == 1) {
    mydots = setNames(rep(paste0('relativeVariability::relativeSD(., MIN = ', min, ', MAX = ', max, ')'), length(variables)), paste0(variables, '_person_relsd'))
  } else {
    mydots = setNames(paste0('relativeVariability::relativeSD(., MIN = ', min, ', MAX = ', max, ')'), 'person_relsd')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( variables, funs_(mydots)) %>%
    as.data.frame()
  out
}

#' Calculates the relative sd of variables at the day level
#' 
#' This is a simple function that returns a dataframe in which the day-level relative sd of any input variables
#' has been calculated. Output variables will be named 'input_variable_day_relsd'
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping A character vector of grouping variables at which you want to calculate the relative sd. This should be your person id variable followed by your day variable.
#' defaults to c('person_id', 'daynr')
#' @export
day_relsd <- function(data, variables, min, max, grouping = c('person_id', 'daynr')) {
  
  if (length(variables) == 1) {
    mydots = setNames(rep(paste0('relativeVariability::relativeSD(., MIN = ', min, ', MAX = ', max, ')'), length(variables)), paste0(variables, '_day_relsd'))
  } else {
    mydots = setNames(paste0('relativeVariability::relativeSD(., MIN = ', min, ', MAX = ', max, ')'), 'day_relsd')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at( variables, funs_(mydots)) %>%
    as.data.frame()
  out
}



#' Calculates the (person level) standard deviation of daily means of variables
#' 
#' This is a function that returns a dataframe in which the person-level standard deviation of the daily means
#' of any input variables has been calculated. Output variables will be named 'input_variable_day_mean_sd'.
#' Note carefully that this is -not- the mean of the standard deviations within each day. Rather, the mean
#' at each day is taken, then the standard deviation of these means is calculated. 
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable 
#' followed by your day identifier. defaults to c('person_id', 'daynr')
#' 
#' @export
day_mean_sd <- function(data, variables, grouping = c('person_id', 'daynr')) {
  
  if (length(variables) == 1) {
    mydots = setNames(rep('stats::sd(., na.rm = TRUE)', length(variables)), paste0(variables, '_day_mean_sd'))
  } else {
    mydots = setNames('stats::sd(., na.rm = TRUE)', 'day_mean_sd')
  }
  
  
  out <- group_by_(data, .dots = grouping) %>%
    # Summarize to the means of each (probably) day
    summarize_at(variables, funs(mean(., na.rm = TRUE))) %>%
    
    # Also realised that after summarizing to get the day means, we're still grouped at the day level, meaning we wont get any variability.
    # we need to group_by person first. 
    group_by_(.dots = grouping[1:(length(grouping) - 1)]) %>%
    # Calcualte the SD for each participant, which is another summarize call I realise. 
    summarize_at(variables, funs_(mydots)) %>%
    # Join that one score per P back into the dataset. So we've basically aggregated up twice (to the day, then to the person) then joined that trait-level score
    # back into the main df. 
    join(data, .) %>%
    as.data.frame()
  return(out)
}

#' Calculates the (person level) relative standard deviation of daily mean of variables
#' 
#' This is a function that returns a dataframe in which the person-level relative standard deviation of the daily mean
#' of any input variables has been calculated. Output variables will be named 'input_variable_day_mean_relsd'.
#' Note carefully that this is -not- the mean of the relative standard deviations within each day. Rather, the mean
#' at each day is taken, then the relative standard deviation of these means is calculated. 
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable 
#' followed by your day identifier. defaults to c('person_id', 'daynr')
#' 
#' @export
day_mean_relsd <- function(data, variables, min, max, grouping = c('person_id', 'daynr')) {
  
  if (length(variables) == 1) {
    mydots = setNames(rep(paste0('relativeVariability::relativeSD(., MIN = ', min, ', MAX = ', max, ')'), length(variables)), paste0(variables, '_day_mean_relsd'))
  } else {
    mydots = setNames(paste0('relativeVariability::relativeSD(., MIN = ', min, ', MAX = ', max, ')'), 'day_mean_relsd')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    # Summarize to the means of each (probably) day
    summarize_at(variables, funs(mean(., na.rm = TRUE))) %>%
    
    # Also realised that after summarizing to get the day means, we're still grouped at the day level, meaning we wont get any variability.
    # we need to group_by person first. 
    group_by_(.dots = grouping[1:(length(grouping) - 1)]) %>%
    # Calcualte the SD for each participant, which is another summarize call I realise. 
    summarize_at(variables, funs_(mydots)) %>%
    # Join that one score per P back into the dataset. So we've basically aggregated up twice (to the day, then to the person) then joined that trait-level score
    # back into the main df. 
    join(data, .) %>%
    as.data.frame()
  return(out)
}


#' Calculates the (person level) standard deviation of daily maxima of variables
#' 
#' This is a function that returns a dataframe in which the person-level standard deviation of the daily max
#' of any input variables has been calculated. Output variables will be named 'input_variable_day_max_sd'.
#' Note carefully that this is -not- the max of the standard deviations within each day. Rather, the max
#' at each day is taken, then the standard deviation of these maxima is calculated. 
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable 
#' followed by your day identifier. defaults to c('person_id', 'daynr')
#' 
#' @export
day_max_sd <- function(data, variables, grouping = c('person_id', 'daynr')) {
  
  if (length(variables) == 1) {
    mydots = setNames(rep('stats::sd(., na.rm = TRUE)', length(variables)), paste0(variables, '_day_max_sd'))
  } else {
    mydots = setNames('stats::sd(., na.rm = TRUE)', 'day_max_sd')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    # Summarize to the means of each (probably) day
    summarize_at(variables, funs(max(., na.rm = TRUE))) %>%
    
    # Also realised that after summarizing to get the day means, we're still grouped at the day level, meaning we wont get any variability.
    # we need to group_by person first. 
    group_by_(.dots = grouping[1:(length(grouping) - 1)]) %>%
    # Calcualte the SD for each participant, which is another summarize call I realise. 
    summarize_at(variables, funs_(mydots)) %>%
    # Join that one score per P back into the dataset. So we've basically aggregated up twice (to the day, then to the person) then joined that trait-level score
    # back into the main df. 
    join(data, .) %>%
    as.data.frame()
  return(out)
}

#' Calculates the (person level) relative standard deviation of daily maxima of variables
#' 
#' This is a function that returns a dataframe in which the person-level relative standard deviation of the daily maximums
#' of any input variables has been calculated. Output variables will be named 'input_variable_day_max_relsd'.
#' Note carefully that this is -not- the max of the relative standard deviations within each day. Rather, the maximum
#' at each day is taken, then the relative standard deviation of these maxima is calculated. 
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable 
#' followed by your day identifier. defaults to c('person_id', 'daynr')
#' 
#' @export
day_max_relsd <- function(data, variables, min, max, grouping = c('person_id', 'daynr')) {
  
  
  if (length(variables) == 1) {
    mydots = setNames(rep(paste0('relativeVariability::relativeSD(., MIN = ', min, ', MAX = ', max, ')'), length(variables)), paste0(variables, '_day_max_relsd'))
  } else {
    mydots = setNames(paste0('relativeVariability::relativeSD(., MIN = ', min, ', MAX = ', max, ')'), 'day_max_relsd')
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    # Summarize to the means of each (probably) day
    summarize_at(variables, funs(max(., na.rm = TRUE))) %>%
    
    # Also realised that after summarizing to get the day means, we're still grouped at the day level, meaning we wont get any variability.
    # we need to group_by person first. 
    group_by_(.dots = grouping[1:(length(grouping) - 1)]) %>%
    # Calcualte the SD for each participant, which is another summarize call I realise. 
    summarize_at(variables, funs_(mydots)) %>%
    # Join that one score per P back into the dataset. So we've basically aggregated up twice (to the day, then to the person) then joined that trait-level score
    # back into the main df. 
    join(data, .) %>%
    as.data.frame()
  return(out)
}

#' Calculates the (person level) standard deviation of daily minima of variables
#' 
#' This is a function that returns a dataframe in which the person-level standard deviation of the daily minimums
#' of any input variables has been calculated. Output variables will be named 'input_variable_day_min_sd'.
#' Note carefully that this is -not- the min of the standard deviations within each day. Rather, the minimum
#' at each day is taken, then the standard deviation of these minima is calculated. 
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable 
#' followed by your day identifier. defaults to c('person_id', 'daynr')
#' 
#' @export
day_min_sd <- function(data, variables, grouping = c('person_id', 'daynr')) {

  if (length(variables) == 1) {
    mydots = setNames(rep('stats::sd(., na.rm = TRUE)', length(variables)), paste0(variables, '_day_min_sd'))
  } else {
    mydots = setNames('stats::sd(., na.rm = TRUE)', 'day_min_sd')
  }
  

out <- group_by_(data, .dots = grouping) %>%
  # Summarize to the means of each (probably) day
  summarize_at(variables, funs(min(., na.rm = TRUE))) %>%
  
  # Also realised that after summarizing to get the day means, we're still grouped at the day level, meaning we wont get any variability.
  # we need to group_by person first. 
  group_by_(.dots = grouping[1:(length(grouping) - 1)]) %>%
  # Calcualte the SD for each participant, which is another summarize call I realise. 
  summarize_at(variables, funs_(mydots)) %>%
  # Join that one score per P back into the dataset. So we've basically aggregated up twice (to the day, then to the person) then joined that trait-level score
  # back into the main df. 
  join(data, .) %>%
  as.data.frame()
return(out)
}


#' Calculates the (person level) relative standard deviation of daily minima of variables
#' 
#' This is a function that returns a dataframe in which the person-level relative standard deviation of the daily minimums
#' of any input variables has been calculated. Output variables will be named 'input_variable_day_min_relsd'.
#' Note carefully that this is -not- the min of the relative standard deviations within each day. Rather, the minimum
#' at each day is taken, then the relative standard deviation of these minima is calculated. 
#' 
#' @param data The input dataset
#' @param variables A character vector of input variables e.g. c('negemo', 'posemo'). 
#' @param grouping The grouping variables at which you want to calculate the mean. This should be your person id variable 
#' followed by your day identifier. defaults to c('person_id', 'daynr')
#' 
#' @export
day_min_relsd <- function(data, variables, min, max, grouping = c('person_id', 'daynr')) {

  if (length(variables) == 1) {
    # If I left min as a character in here, it got passed through as the literal min (function) instead of the argument. Have to evaluate the argument
    # here (or you might be able to call the parent environment later but that's not easily within my abilities). 
    mydots = setNames(rep(paste0('relativeVariability::relativeSD(., MIN = ', min, ', MAX = ', max, ')'), length(variables)), paste0(variables, '_day_min_relsd'))
  } else {
    mydots = setNames(paste0('relativeVariability::relativeSD(., MIN = ', min, ', MAX = ', max, ')'), 'day_min_relsd')
  }
  

out <- group_by_(data, .dots = grouping) %>%
  # Summarize to the means of each (probably) day
  summarize_at(variables, funs(min(., na.rm = TRUE))) %>%
  
  # Also realised that after summarizing to get the day means, we're still grouped at the day level, meaning we wont get any variability.
  # we need to group_by person first. 
  group_by_(.dots = grouping[1:(length(grouping) - 1)]) %>%
  # Calcualte the SD for each participant, which is another summarize call I realise. 
  summarize_at(variables, funs_(mydots)) %>%
  # Join that one score per P back into the dataset. So we've basically aggregated up twice (to the day, then to the person) then joined that trait-level score
  # back into the main df. 
  join(data, .) %>%
  as.data.frame()
return(out)
}


#' Rename a series of columns in a dataframe
#' 
#' Given a dataframe and a series of key-value pairs, replaced all the key matches within the column names of the dataframe
#' with their corresponding values, and returns a data frame as a result.
#' 
#' @param data The input dataset
#' @param ... A series of key value pairs (in character form) of target variables and what they currently are in the dataset
#' 
#' @examples
#' # Turns the Girth column in trees into an Alpha column
#' rename_cols(trees, 'Alpha' = 'Girth')
#' 
#' @export
rename_cols <- function(data, ...) {
  arguments <- list(...)
  arguments <- unlist(arguments)
  
  # Thought we needed to swap arguments but we don't. 
  # myargs <- names(arguments); names(myargs) <- arguments
  
  # Adding this line to automatically add any emotions that already match the emotions vector up above. So as long
  # as we define the study, we only need to add arguments for emotions that are mispelled or shortened. 
  data %>% rename_(.dots = arguments) %>% data.frame()
}

#' Reverse a series of columns in a dataframe
#' 
#' Given a dataframe, a list of variables, and the min and max of those variables (which must be the same)
#' reverses all variables. Returns a dataframe containing the result.
#' 
#' @param data The input dataset
#' @param variables A character vector of variables that require reversing
#' @param min The minimum of all the variables to be reversed (for variables that have different mins, use separate
#' function calls)
#' @param max The maximum of all the variables to be reversed
#' 
#' @examples
#' # Reverses the carb column in mtcars
#' reverse_cols(mtcars, 'carb', min = 1, max = 8)
#' 
#' @export
reverse_cols <- function(data, variables, min, max) {
  study_variables <- variables[variables %in% colnames(data)]
  data[study_variables] <- (max + min) - data[study_variables] 
  data
}

#' Flexibly operate on sets of variables within a dataframe
#' 
#' This simple function allows for simple operations to be performed on sets of variables within a dataframe.
#' It is useful mainly for maintaining code consistency and saving subsetting code. It can add, divide, 
#' multiply and subtract by a constant, and will perform the same operation on all variables passed in.
#' 
#' @param data The input dataset
#' @param variables A character vector of variables to be operated on
#' @param operation one of 'add', 'subtract', 'multiply', 'divide'
#' @param number A constant. This should usually be positive (subtract will automatically flip it), and will
#' make sense to say out loud "subtract 2" rather than "subtract -2" is usually what is intended. Of course,
#' you can multiply by -4, if for some reason you so wish...
#' 
#' @examples
#' # Subtract one from the carb column of mtcars
#' operate_cols(mtcars, 'carb', 'subtract', 1)
#' 
#' @export
operate_cols <- function(data, variables, operation = 'add', number) {
  study_variables <- variables[variables %in% colnames(data)]
  
  if (operation %in% 'add') {
    data[study_variables] <- data[study_variables] + number
  }
  
  if (operation %in% 'subtract') {
    data[study_variables] <- data[study_variables] - number
  }
  
  if (operation %in% 'multiply') {
    data[study_variables] <- data[study_variables] * number
  }
  
  if (operation %in% 'divide') {
    data[study_variables] <- data[study_variables] / number
  }
  data
}

#' A utility function that makes rescale_cols work. Usually not useful directly. 
my_rescale <- function (x, new.min, new.max, old.min, old.max) { 
  nx = new.min + (new.max - new.min) * (x - old.min)/(old.max - old.min)
  return(nx)
}


#' A simple (but very useful) function to rescale variables flexibly. 
#' 
#' This function allows for a set of variables to be rescaled from any original min and max to any target
#' min and max. 
#' 
#' @param data The input dataset
#' @param variables A character vector of variables to be operated on
#' @param old.min the minima of the current variables
#' @param old.max the maxima of the current variables
#' @param new.min the minima of the new variables
#' @param new.max the maxima of the new variables
#' 
#' @examples 
#' # rescale the carb column of mtcars from 1-8 to 1-100.
#' rescale_cols(mtcars, 'carb', old.min = 1, old.max = 8, new.min = 1, new.max = 100)
#' 
#' @export
rescale_cols <- function(data, variables, old.min, old.max, new.min, new.max) {
  
  study_variables <- variables[variables %in% colnames(data)]
  
  data[study_variables] <- my_rescale(data[study_variables], new.min = new.min, 
                                 new.max = new.max, old.min = old.min, 
                                 old.max = old.max)
  data
}


#' Keep only groups (usually days) with enough observations. 
#' 
#' This function allows for trimming esm data down so that only days (or people) with a minimum number of observations
#' are included. If there is a 'missed' variable (coded 1 = missed) these rows will not be counted. To 
#' make sure enough observations of a particular variable are included, use \code{\link{trim_min_valid_obs}}.
#' 
#' @param data The input dataset
#' @param grouping The grouping variables within which you require a minimum observation number. Could be just
#' your person id or your person id and day. Should take the form of a character vector. 
#' @param min.obs The minimum number of observations to require. Defaults to 2. 
#' 
#' @examples 
#' # Trim the sleep dataset to have 2 obs per ID (this does nothing, but illustrates syntax)
#' trim_min_obs(sleep, grouping = 'ID', min.obs = 2)
#' 
#' @export
trim_min_obs <- function(data, grouping, min.obs = 2) { 
  
  # I've incorporated handling missing data here so we make sure we have actual observations. 
  if ('missed' %in% colnames(data)) {
  out <- group_by_(data, .dots = grouping) %>%
  dplyr::filter(sum(missed == 0) >= min.obs) %>%
  as.data.frame()
  } else {
  out <- group_by_(data, .dots = grouping) %>%
  dplyr::filter(n() >= min.obs) %>%
  as.data.frame()
  }
  return(out)
}

#' Keep only groups (usually days) with enough non-missing observations on a set of variables. 
#' 
#' This function allows for trimming esm data down so that only days (or people) with a minimum number of observations
#' are included. If there is a 'missed' variable (coded 1 = missed) these rows will not be counted. Missing values
#' on the specified variables will also not be counted. Essentially, this ensures that there will be at least min.obs
#' number of valid values within each grouping factor retained. For a more general function, see \code{\link{trim_min_obs}}.
#' 
#' @param data The input dataset
#' @param grouping The grouping variables within which you require a minimum observation number. Could be just
#' your person id or your person id and day. Should take the form of a character vector. 
#' @param variables The variables you require a certain number of values on. 
#' @param min.obs The minimum number of observations to require. Defaults to 2. 
#' 
#' @examples 
#' # Trim the sleep dataset to have 2 obs per ID (this does nothing, but illustrates syntax)
#' trim_min_valid_obs(sleep, grouping = 'ID', variables = 'extra', min.obs = 2)
#' 
#' @export
trim_min_valid_obs <- function(data, grouping, variables, min.obs = 2) { 
  
  # I've incorporated handling missing data here so we make sure we have actual observations. 
  if ('missed' %in% colnames(data)) {
  out <- group_by_(data, .dots = grouping) %>%
  dplyr::filter(sum(missed == 0) >= min.obs) %>%
  dplyr::filter_(.dots = paste0('sum(!is.na(', variables, ')) >= ', min.obs)) %>%
  as.data.frame()
  } else {
  out <- group_by_(data, .dots = grouping) %>%
  dplyr::filter(n() >= min.obs) %>%
    dplyr::filter_(.dots = paste0('sum(!is.na(', variables, ')) >= ', min.obs)) %>%
  as.data.frame()
  }
  return(out)
}

#' Lag observation-level variables
#' 
#' This function will lag any variable on the observation level. It takes a dataset, a character vector of variables, 
#' and a character vector of grouping variables (by default the person and day ids)
#' and lags within these groups. With the default grouping levels, this will allow lags within day, but will not allow lags across day (the first lagged
#' observation of each day for each P will always be NA). 
#' 
#' It can't be used for lagging day level variables because those need to be aggregated or sliced to one obs per day first - the dplyr 
#' lag function this is built on doesn't lag at the grouping level, it just lags within groups by 1 row. 
#' 
#' \code{\link{esm_lag}} and \code{\link{esm_day_lag}} are unique in that they output variables with the same appelation (_lag) because they output what we would expect
#' for lag at each level so this intuitively makes sense. The variables will already have names to say whether they're at the day level or not. 
#' 
#' @param data The dataset to operate on.
#' @param variables The character vector of variables to lag
#' @param grouping The character vector of grouping variables (by default, c('person_id', 'daynr'))
#' @param order The observation number to order by within days. By default it is 'obs_id'. You will want
#' this to be a unique, ordered observation identifier. It does not matter whether it is unique only within
#' days (i.e. resets each day for each p). 
#' @export
esm_lag <- function(data, variables, grouping = c('person_id', 'daynr'), order = 'obs_id') {
  
  if (length(variables) == 1) {
    mydots = setNames(rep('lag(.)', length(variables)), paste0(variables, '_lag'))
  } else {
    mydots = setNames('lag(.)', 'lag')
  }
  
  out <- dplyr::group_by_(data, .dots = grouping) %>%
    dplyr::arrange_(.dots = c(grouping, order)) %>%
    # We do need to group_by I believe otherwise it won't appropriate fill in NA at the start of each day, 
    # let's say. 
    mutate_at(variables, funs_(mydots)) %>%
    as.data.frame()
  return(out)
}

#' Lag day-level (or wave-level, month-level, week-level) variables
#' 
#' This function will lag any variable at an aggregated level above the observation level (usually the day level).
#' It takes a dataset, a character vector of variables, and a character vector of grouping variables (by default the person and day ids), slices the first
#' value within each group, lags these, then merges the result back into the original dataframe. The result is that any aggregated variable (e.g. the daily mean
#' of a certain variable) will have all its (identical) values from one day applied as lagged variables to the following day, essentially as one would intuitively
#' expect.
#' 
#' Note that this function works with long data (i.e. expects one row per beep) though it may work with different formats, I wouldn't guarantee it. Could technically
#' be used to lag observation level variables if you added the beep id to the grouping vector, but no reason to use it in that fashion and I haven't tested it.  
#' 
#' \code{\link{esm_lag}} and \code{\link{esm_day_lag}} are unique in that they output variables with the same appelation (_lag) because they output what we would expect
#' for lag at each level so this intuitively makes sense. The variables will already have names to say whether they're at the day level or not. 
#' 
#' @param data The dataset to operate on.
#' @param variables The character vector of variables to lag
#' @param grouping The character vector of grouping variables (by default, c('person_id', 'daynr'))
#' @export
esm_day_lag <- function(data, variables, grouping = c('person_id', 'daynr')) {
  
  if (length(variables) == 1) {
    mydots = setNames(rep('lag(.)', length(variables)), paste0(variables, '_lag'))
  } else {
    mydots = setNames('lag(.)', 'lag')
  }
  
out <- group_by_(data, .dots = grouping) %>%
  # Slice 1 to take only 1 observation from each day(presuming that we're operating at the day level here so it doesn't matter which one)
  slice(1) %>%
  # Now ungroup, and group_by only person_id (one less grouping variable) because we need to lag across days but not people
  ungroup() %>%
  group_by_(.dots = grouping[1:(length(grouping) - 1)]) %>%
  # Now we arrange by the grouping only. We don't add order because we're working at the day level and if we've done it right, it's 
  # redunant, and if we've done it wrong, it won't fix it because day has precedence.
  arrange_(.dots = grouping) %>%
  # We don't actually need to call (.) with lag because there's no further arguments we need to access.
  mutate_at(variables, funs_(mydots)) %>%
  # Now we need to use select to grab only the grouping columns and variable columns so we can safely merge into the main df.
  ungroup() %>%
  # We have to use paste in this select call because we want to join back in the variables we've created not the inputs. If we'd used
  # summarise instead of mutate we could just join what's left, but I don't think summarize would work with lag? it expects to return
  # one value per group. 
  select_(.dots = c(grouping, paste0(variables, '_lag'))) %>%
  join(data, .) %>%
  as.data.frame()

return(out)
}

#' A simple function to mean center variables
#' 
#' This function will take a single variable and return the centered version of that variable. It can be used
#' with the grouping parameter set to 'person_id' to get person-centered variables. 
#' 
#' @param data The dataset to operate on.
#' @param variables The character vector of variables to lag
#' @param grouping The character vector of grouping variables
#' @export
center_cols <- function(data, variables, grouping) {
  
  # Writing my own centering funtion because using scales messes things up sometimes.   
  mycent <- function(x) {
    out <- x - mean(x, na.rm = TRUE)
    return(out)
  }
  
  out <- group_by_(data, .dots = grouping) %>%
    mutate_at(variables, funs('center' = mycent)) %>%
    as.data.frame()
  
  return(out)
}
