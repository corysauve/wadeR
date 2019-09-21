#' Bottom of epilimnion finder
#'
#' Determines the depth of the bottom of the epilimnion in a lake profile.
#'
#' @param depths Numeric vector of depths corresponding to the temps values
#' @param temps Numeric vector of water temperature in degrees Celsius
#' @keywords thermocline
#' @export
#' @examples

epi_finder <- function(depths, temps){

  # Uses meta_finder function in wadeR
  meta_depths <- wadeR::meta_finder(depths, temps)

  # Bottom of epi is represented by top of meta in first pos. in vector
  epi_bottom <- meta_depths[[1]]
  return(epi_bottom)

}
