#' Calculate the Carlson Trophic State Index - Total Phosphorus
#'
#' @param tp Epilimnetic total phosphorus concentration in ug/L
#' @param digits Rounding digits; Defaults to 3
#' @export

tsi_tp <- function(tp, digits=3){

  if(is_negative(tp)){
    stop("TP conc. must be positive")
  }

  if(is_negative(digits)){
    stop("Number of rounding digits must be positive")
  }

  round(14.42 * log(tp) + 4.15, 3)
}
