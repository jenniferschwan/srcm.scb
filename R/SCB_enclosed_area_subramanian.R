#' Calculates the enclosed area of the given SCB along the lines of Subramanian
#' "state the formula here"
#'
#' @param SCB the given SCB, must contain the columns 'time', 'SCB_low' and 'SCB_high'
#'
#' @return the area enclosed by the given SCB
SCB_enclosed_area_subramanian <- function(SCB) {
  checkmate::assert_names(names(SCB), must.include = c("time", "SCB_low", "SCB_high"))

  l <- SCB$SCB_high - SCB$SCB_low
  delta_Z <- SCB$time[-1] - SCB$time[-nrow(SCB)]

  sum(l[-nrow(SCB)] * delta_Z)
}
