#' Calculate the organic nitrogen concentration
#'
#' @param tn Total nitrogen concentration
#' @param no3 Nitrate concentration
#' @param nh3 Ammonia concentration
#' @param decimals Rounding digits; defaults to 3
#' @export

organic_n <- function(tn, no3, nh3, decimals=3){

  # Concentrations must be positive
  if(min(tn) > 0){
    stop("TN conc. must be positive")
  }

  if(min(no3) > 0){
    stop("TN conc. must be positive")
  }

  if(min(nh3) > 0){
    stop("TN conc. must be positive")
  }

  round(tn - (no3 + nh3), decimals)
}
