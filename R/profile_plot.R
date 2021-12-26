#' Plot a depth profile for a given parameter
#'
#' @param data Data frame
#' @param x Variable to plot on x-axis
#' @param y Variable to plot on y-axis
#' @param ... Optional ggplot2 arguments
#' @returns ggplot
#' @export

profile_plot <- function(data, x, y, ...){

  ggplot2::ggplot(data, ggplot2::aes({{x}}, {{y}})) +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::scale_y_reverse()
}
