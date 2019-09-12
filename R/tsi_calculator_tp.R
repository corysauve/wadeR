#' Trophic State Index - Total Phosphorus
#'
#' Calculates the trophic state index value for a lake from a epilimnetic total phosphorus concentration
#' @param tp_epi Numeric value of epilimnetic total phosphorus concentration
#' @param units Character string that accepts either ugL or mgL; defaults to mgL
#' @keywords trophic state
#' @export
#' @examples
#'
#' tsi_calculator_tp(0.01, units "mgL")

tsi_calculator_tp <- function(tp_epi, units = "mgL"){
  # Units = mg/L
  if(units == "mgL"){
    tpConvert <- tp_epi * 1000
    tsiTp <- 14.42 * log(tpConvert) + 4.15
    # Units = ug/L
  } else {
    tsiTp <- 14.42 * log(tp_epi) + 4.15
  }
  return(tsiTp)
}
