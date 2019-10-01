#' Zooplankton NU  per liter
#'
#' Calculates the natural units per liter of zooplankton from a sample
#' @param df Dataframe containing genus and totalCount of each genus
#' @param towLen Length in meters of zooplankton net tow at time of sampling
#' @param sampleVol Volume in mL of sample at time of identification
#' @param netDia Diameter in cm of zooplankton net opening; defaults to 12.5
#' @keywords zooplankton
#' @export
#' @examples
#'
#' zoo_nul(planktonCounts, towlength, sampleVol, netDia)

zoo_nul <- function(df, towLen, sampleVol, netDia = 12.5){

  # Calculate tow volume
  towVol <- (((netDia / 2) * (1 / 100)) ^2) * pi * towLen * 1000

  # Determine multiplication factor (translating from scope to water)
  multiFactor <- sampleVol / towVol

  df$nuL <- multiFactor * df[ ,"totalCount"]

  return(df)
}
