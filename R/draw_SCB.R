#' Draws the given Scb into a ggplot2::ggplot object
#'
#' @param SCB the SCB to be drawn, must contain columns "time", "estimated_survival", "SCB_low" and "SCB_high"
#' @param name optional name with which the scb should be labeled (default "SCB")
#' @param title optional title of the plot (default "SCB")
#' @param plt optional: the plt object to which the plot should be added
#' @param S_true optional: the true survival function
#' @param x_lims optional: the x limits for the plot. if not given, they are set to (o, max(SCB$time))
#' @param y_lims soptional: the y limits for the plot. if not given, they are set to (0, 1)
#' @param my_color optional: the color to be used for the plot (default "red")
#'
#' @return the ggplot2::ggplot object containing the plot of the given SCB
#' @export
#'
#' @examples
#' surv_data <- dplyr::tibble(time = c(1:5), event = c(1, 0, 1, 0, 1))
#' SCB <- SCB_subramanian_type1(surv_data, estimator_dikta98, gph, 0.05, 100)
#' draw_SCB(SCB)
draw_SCB <- function(SCB, name = "SCB", title = "SCB",
                     plt = ggplot2::ggplot(),
                     S_true = NULL,
                     x_lims = NULL, y_lims = NULL,
                     my_color = "red") {
  checkmate::assert_names(names(SCB), must.include = c("time", "estimated_survival", "SCB_low", "SCB_high"))

  SCB <- helper_SCB_add_front_and_back_values(SCB)

  if (is.null(x_lims)) {
    x_lims <- c(0, max(SCB$time))
  }
  if (is.null(y_lims)) {
    y_lims <- c(0, 1)
  }

  plt <- plt +
    ggplot2::geom_step(ggplot2::aes(x = SCB$time, y = SCB$estimated_survival, color = paste(name, "estimated_survival")), color = my_color) +
    ggplot2::geom_step(ggplot2::aes(x = SCB$time, y = SCB$SCB_low, color = paste(name, "SCB_low")), linetype = "dashed", color = my_color) +
    ggplot2::geom_step(ggplot2::aes(x = SCB$time, y = SCB$SCB_high, color = paste(name, "SCB_high")), linetype = "dashed", color = my_color) +
    qwraps2::stat_stepribbon(ggplot2::aes(x = SCB$time, ymin = SCB$SCB_low, ymax = SCB$SCB_high), fill = my_color, alpha = 0.1) +
    ggplot2::xlim(x_lims[1], x_lims[2]) +
    ggplot2::ylim(y_lims[1], y_lims[2]) +
    ggplot2::labs(x = "t", y = "S(t)") +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::ggtitle(title)
  #### TODO:  Warum ist die legende hier kaputt??? hat mit dem color = my_color zu tun

  if (!is.null(S_true)) {
    plt <- plt + ggplot2::stat_function(fun = S_true, n = 100, mapping = ggplot2::aes(color = "S_true"), color = "grey30")
  }

  plt
}

helper_SCB_add_front_and_back_values <- function(SCB) {
  SCB_low_at_0 <- 1 - (SCB$estimated_survival[1] - SCB$SCB_low[1])
  SCB_high_end <- utils::tail(SCB$SCB_high, n = 1)
  SCB_survival_end <- utils::tail(SCB$estimated_survival, n = 1)
  SCB %>%
    dplyr::add_row(
      time = 0, event = 1, rank = 0,
      estimated_survival = 1,
      SCB_low = SCB_low_at_0, SCB_high = 1,
      .before = 1
    ) %>%
    dplyr::add_row(
      time = max(SCB$time * 1.05), event = 1, rank = 101,
      estimated_survival = SCB_survival_end,
      SCB_low = 0, SCB_high = SCB_high_end
    )
}
