#' Title
#'
#' @param SCB
#' @param plt
#' @param S_true
#' @param x_lims
#' @param y_lims
#'
#' @return
#' @export
#'
#' @examples
draw_SCB <- function(SCB, plt = ggplot2::ggplot(), S_true = NULL, x_lims = NULL, y_lims = NULL, my_color = "red") {
  checkmate::assert_names(names(SCB), must.include = c("time", "estimated_survival", "SCB_low", "SCB_high"))

  if (is.null(x_lims)) {
    x_lims <- c(0.95 * min(SCB$time), 1.05 * max(SCB$time))
  }
  if (is.null(y_lims)) {
    y_lims <- c(0, 1)
  }

  plt <- plt +
    ggplot2::geom_step(ggplot2::aes(x = SCB$time, y = SCB$estimated_survival, color = "estimated_survival"), color = my_color) +
    ggplot2::geom_step(ggplot2::aes(x = SCB$time, y = SCB$SCB_low, color = "SCB_low"), linetype = "dashed", color = my_color) +
    ggplot2::geom_step(ggplot2::aes(x = SCB$time, y = SCB$SCB_high, color = "SCB_high"), linetype = "dashed", color = my_color) +
    qwraps2::stat_stepribbon(ggplot2::aes(x = SCB$time, ymin = SCB$SCB_low, ymax = SCB$SCB_high), fill = my_color, alpha = 0.1) +
    ggplot2::xlim(x_lims[1], x_lims[2]) +
    ggplot2::ylim(y_lims[1], y_lims[2]) +
    ggplot2::labs(x = "t", y = "S(t)") +
    ggplot2::guides(color=ggplot2::guide_legend(title=NULL)) +
    ggplot2::theme(legend.position = "bottom")
  #### TODO:  Warum ist die legende hier kaputt??? hat mit stepribbon zu tun

  if (!is.null(S_true)) {
    plt <- plt + ggplot2::stat_function(fun = S_true, n = 100, mapping = ggplot2::aes(color = "S_true"), color = "grey30")
  }

  plt
}
