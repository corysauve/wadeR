#' One Percent Light Level
#'
#' Calculate the one percent light level for a water profile
#' @param depths Vector of profile depths
#' @param do Vector of light measurements
#' @keywords profile light
#' @export
#' @examples
#'
#' per_light(depths, light)

per_light <- function(depths, light){

  # Determine surface and one percent light
  surfaceLight <- light[[1]]
  onePercent <- surfaceLight * 0.01

  # Remove zeros and determine length
  light1 <- light[!light %in% 0]
  len <- length(light1)

  # Make depth vector sample length
  depths1 <- depths[1:len]

  # Calculate one percent light level
  mod <- lm(depths1 ~ log(light1))
  coef <- coef(mod)
  int <- coef[1]
  slope <- coef[2]
  onePercentLightLevel <- slope * log(onePercent) + int

  return(onePercentLightLevel)
}
