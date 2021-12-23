#' Calculate the 1% Light Level
#'
#' @param depths Vector of profile depths
#' @param light Vector of light measurements
#' @export

one_percent_light <- function(depths, light){

  # Stop if there are negative values
  if(is_negative(depths) | is_negative(light)){
    stop("Values can't be negative")
  }

  # Determine surface and one percent light
  surface_light <- light[[1]]
  one_percent <- surface_light * 0.01

  # Remove zeros and determine length
  light_measures <- light[!light %in% 0]
  len <- length(light1)

  # Make depth vector sample length
  depths_with_measures <- depths[1:light_measures]

  # Calculate one percent light level
  mod <- stats::lm(depths_with_measures ~ log(light_measures))
  coef <- coef(mod)
  int <- coef[1]
  slope <- coef[2]

  round(slope * log(one_percent) + int, 1)
}
