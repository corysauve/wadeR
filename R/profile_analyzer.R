#' Profile Analyzer
#'
#' Calculates summary statistics for a water profile parameter
#' @param parameter Numeric vector
#' @keywords profile
#' @export
#' @example
#'
#'temps <- c(1, 2, 3 ,4, 5, 6, 7, 8)
#'
#'profile_analyzer(temps)

profile_analyzer <- function(parameter){

  # Calculate depth of profile
  profileDepth <- length(parameter)

  # Calculate mean
  parameterMean <- mean(parameter)

  # Calculate median
  parameterMedian <- median(parameter)

  # Calculate min
  parameterMin <- min(parameter)

  # Calculate max
  parameterMax <- max(parameter)

  # Consolidate values into a vectors to create dataframe
  summaryVariable <- c("Profile Depth", "Parameter Mean", "Parameter Median", "Parameter Min", "Parameter Max")
  summaryValue <- c(profileDepth, parameterMean, parameterMedian, parameterMin, parameterMax)

  # Creating summary df
  summaryTable <- data.table::data.table(summaryVariable, summaryValue)

  return(summaryTable)
}
