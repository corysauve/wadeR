#' Calculate zooplankton concentration from tow-net sampled
#'
#' @param count Number of individuals counted
#' @param tow_length Length of tow in meters
#' @param sample_vol Sample volume in mL
#' @param new_dia Tow net diameter; defaults to 12.5
#' @returns A double
#' @export

zoo_nul <- function(count, tow_length, sample_vol, new_dia = 12.5){

  # Calculate tow volume
  tow_length <- (((new_dia / 2) * (1 / 100)) ^2) * pi * tow_length * 1000

  # Determine multiplication factor (translating from scope to water)
  sample_vol / tow_length

  multiFactor * count
}
