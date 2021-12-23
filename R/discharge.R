#' Calculate stream discharge
#'
#' @param widths Transect measurement points
#' @param depths Depth measurements
#' @param velocity Velocity measurements
#' @return a numeric
#' @export

discharge <- function(widths, depths, vel){

    sum(widths * depths * vel)
}
