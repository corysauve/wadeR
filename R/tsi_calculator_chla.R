#' Trophic State Index - Chlorophyll-a
#'
#' Calculates the trophic state index value for a lake from a chloropyll-a concentration
#' @param chla_mgM3 Numeric value of chlorophyll-a concentration in either ug/L or mg/m3
#' @keywords trophic state
#' @export
#' @examples
#'
#' tsi_calculator_chla(0.10)

tsi_calculator_chla <- function(chla_mgM3){
  # Units = mg/m3
  tsiChla <- 9.81 * log(chla_mgM3) + 30.6
  return(tsiChla)
}
