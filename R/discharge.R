#' Stream discharge
#'
#'
#'

discharge <- function(widths, depths, vel){

    q_m_seg <- widths * depths * vel
    Q_m3s <- sum(q_m_seg)
    return(Q_m3s)

}
