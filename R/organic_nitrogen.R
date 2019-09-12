#' Organic Nitrogen
#'
#' Calculates the Organic-nitrogen concentration from total nitrogen, nitrate, and ammonia.
#' @param tn_mgL Numeric value for total nitrogen concentration in milligrams per liter
#' @param no3_mgL Numeric value for nitrate concentration in milligrams per liter
#' @param nh3_mgL Numberic value for ammonia concentration in milligrams per liter
#' @param location Character string to denote sampling location; only accepts for epilimnion -- "epi" or hypolimnion -- "hypo"
#' @keywords nitrogen
#' @export
#' @examples
#'
#' organic_nitrogen(0.50, 0.02, 0.01, "epi")

organic_nitrogen <- function(tn_mgL, no3_mgL, nh3_mgL, location){

  if(location == "epi"){
    organicN <- tn_mgL - (no3_mgL + nh3_mgL)
    return(organicN)
  } else if (location == "hypo") {
    organicN <- tn_mgL - (no3_mgL + nh3_mgL)
    return(organicN)
  } else {
    print("Did not supply correct location name")
  }
}

