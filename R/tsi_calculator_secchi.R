#' Trophic State Index - Secchi depth
#'
#' Calculates the trophic state index value for a lake from a Secchi depth measurement.
#' @param secchi Numeric value for a Secchi depth measurement
#' @param units Character string that accepts either 'm' or 'ft'; defaults to 'm' -- meters
#' @keywords trophic state
#' @export
#' @examples
#'
#' tsi_calculator_secchi(1.5, units = "m")

tsi_calculator_secchi <- function(secchi, units = "m"){
  # Units = ft
  if(units == "ft"){
    secchiConvert <- secchi / 3.28084 # converting feet to meters
    tsiSd <- 60 - 14.41 * log(secchiConvert)
  } else {
    tsiSd <- 60 - 14.41 * log(secchi)
  }
  return(tsiSd)
}
