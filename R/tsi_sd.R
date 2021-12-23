#' Calculate the Carlson Trophic State Index - Secchi
#'
#' @param secchi Secchi depth in meters
#' @param digits Rounding digits; Defaults to 1
#' @export

tsi_sd <- function(secchi, digits=1){

  if(is_negative(secchi)){
    stop("Secchi depth must be positive")
  }

  if(is_negative(digits)){
    stop("Number of rounding digits must be positive")
  }

  round(60 - 14.41 * log(secchi), digits)
}
