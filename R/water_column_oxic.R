#' Water Column Oxic - Percent
#'
#' This function calculates the percentage of a water column that is oxic.
#' @param depths Vector of profile depths
#' @param do Vector of profile dissolved oxygen concentrations
#' @keywords profile
#' @export
#' @examples
#' depths <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
#' do <- c(10.0, 12.3, 12.8, 9.1, 8.2, 4.1, 2.3, 1.2)
#'
#' water_column_oxic(depths, do)

water_column_oxic <- function(depths, do){
  if(length(depths) != length(do)){
    stop('must be the same length')
  } else {

    totalDepths <- length(depths)
    oxicDepths <- length(do[do >= 2])

    percentOxic <- (oxicDepths / totalDepths) * 100

    return(percentOxic)
  }
}


