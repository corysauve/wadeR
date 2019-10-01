#' Catch-per-unit-effort
#'
#' Calculates

cpue <- function(spp, catch, effort){

  catch_data <- data.table::data.table(spp, catch, effort)

  catch_data$cpue <- catch_data$catch / catch_data$effort

  cpue_table <- data.table::data.table(catch_data$spp, catch_data$cpue)

  return(cpue_table)

}
