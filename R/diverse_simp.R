#' Simpson Diversity Index
#'
#'
#'
#'
#'

diverse_simp <- function(counts){

  total_ind <- sum(counts)

  Pi <- counts / total_ind
  Pi2 <- Pi ^ 2

  D <- 1 / sum(Pi2)

  return(D)

}






