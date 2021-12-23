#' Catch-per-unit-effort
#'
#' @param catch Collected individuals
#' @param effort Duration of collection
#' @export

cpue <- function(catch, effort){

  catch / effort
}
