#' Water density
#'
#' Calculates density of water from temperature and salinity.
#' This function was borrowed heavily from the rLakeAnalyzer package on CRAN.
#' @param temps Vector of water temperature in degrees Celsius
#' @param salinity Vector of salinity in Practical Salinity Units.
#' @keywords density
#' @export
#' @examples
#'
#' water_density(temps, salinity)

water_density <-  function(temps, salinity = temps * 0) {

  # Calculation requires that both vectors are same length
  if(length(temps) != length(salinity)){
    stop("Water and salinity vectors must be same length!")
  }

  # For method selection; initially sets both to FALSE
  MM = FALSE # For Martin and McCutcheon
  UN = FALSE # For UNESCO

  # Creating temperature and salinity range for calculations
  temps_range <- c(0, 40)
  salinity_range <- c(0.5, 43)

  # Check to see if values are found in specified ranges
  if(all(salinity < salinity_range[1], na.rm = TRUE)) {
    MM <- TRUE # uses Martin and McCutcheon
  } else if (!(sum(temps < temps_range, na.rm = TRUE) || sum(temps > temps_range[2], na.rm = TRUE)) &&
             !(sum(salinity < salinity_range[1], na.rm = TRUE)) || sum(salinity > salinity_range[2], na.rm = TRUE)) {
    UN <- TRUE # uses UNESCO
  }

  if(MM){
    rho <- (1000 * (1 - (temps + 288.9414) * (temps - 3.9863) ^2 / (508929.2 * (temps + 68.12963))))
  }

  if(UN){

    # Equation 1
    rho_0 <- 999.842594 + 6.793952 * 10 ^(-2) * temps - 9.095290 * 10 ^(-3) * temps ^2 + 1.001685 * 10 ^(-4) * temps ^3 - 1.120083 * 10 ^(-6) * temps ^4 + 6.536335e-9 * temps ^5

    # Equation 2
    eq2 <- 8.24493 * 10 ^(-1) - 4.0899e-3 * temps + 7.6438 * 10 ^(-5) * temps ^2 - 8.2467 * 10 ^(-7) * temps ^3 + 5.3875 * 10 ^(-9) * temps ^4

    # Equation 3
    eq3 <- -5.72466 * 10 ^(-3) + 1.0227 * 10 ^(-4) * temps - 1.6546 * 10 ^(-6) * temps ^2

    # Equation 4
    eq4 <- 4.8314 * 10 ^(-4)

    # Equation 5
    rho <- rho_0 + eq2 * salinity + eq3 * salinity ^(3/2) + eq4 * salinity ^2
  }

  return(rho)

}

