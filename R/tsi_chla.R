#' Calculate the Carlson Trophic State Index - Chlorophyll-a
#'
#' @param chla Chlorophyll-a concentration in ug/L
#' @param digits Rounding digits; defaults to 3
#' @export

tsi_chla <- function(chla, digits=3){

  round(9.81 * log(chla) + 30.6, digits)
}
