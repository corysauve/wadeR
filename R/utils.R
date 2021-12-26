#' Count number of decimals
#'
#' @param x A value to count decimals from
#' @return An integer
#' @export

n_decimals <- function(x){

  if(abs(x - round(x)) > .Machine$double.eps^0.5){
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#' Are there negative values
#'
#' @param x Vector of numbers
#' @return Logical
#' @export

is_negative <- function(x){

  min(x) < 0
}

#' Calculate the cube root
#'
#' @param x Numeric value
#' @export

cube_root <- function(x){
  x ^ (1/3)
}

