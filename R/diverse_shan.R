#' Shannon diveristy index
#'
#'
#'
#'

diverse_shan <- function(counts){

  total_ind <- sum(counts)

  Pi <- counts / total_ind
  Pi2 <- Pi ^ 2
  lnPi <- log(Pi)

  PiLnPi <- Pi * lnPi

  H <- -sum(PiLnPi)

  return(H)
}



