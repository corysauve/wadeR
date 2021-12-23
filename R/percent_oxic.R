#' Calculate portion of water column that is oxic
#'
#' @param depths Vector of profile depths
#' @param do Vector of dissolved oxygen measurements
#' @export

percent_oxic <- function(depths, do, digits){

  # Stop if there are missing values
  if(sum(is.na(depths)) > 0 | sum(is.na(do)) > 0){
    stop("Cannot have missing values")
  }

  # Lengths of vectors must be equal
  if(length(depths) != length(do)){
    stop("Depth and DO vectors must be the same length")
  }

  # DO measurements are numeric
  if(!is.numeric(do)){
    stop("DO measurements must be numeric")
  }

  # DO measurements can't be zero
  if(min(do) <= 0){
    stop("Oxygen measurements must be positive")
  }

  all_depths <- length(depths)
  oxic_depths <- length(do[do >= 2])

  round((oxic_depths / all_depths) * 100, 1)
}
