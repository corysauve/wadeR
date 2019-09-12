#' One Percent Light Level
#'
#' Calculate the one percent light level for a water profile
#' @param depths Vector of profile depths
#' @param do Vector of light measurements
#' @keywords profile light
#' @export
#' @examples
#'
#' one_percent_light(depths, light)

one_percent_light <- function(depths, light){
  surfaceLight <- light[[1]]
  onePercent <- surfaceLight * 0.01
  mod <- lm(depths ~ log(light))
  coef <- coef(mod)
  int <- coef[1]
  slope <- coef[2]
  onePercentLightLevel <- slope * log(onePercent) + int

  return(onePercentLightLevel)
}
