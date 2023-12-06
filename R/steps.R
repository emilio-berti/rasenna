#' @title Step Length
#'
#' @param x matrix or data.frame with location coordinates.
#'
#' @details
#' Step length for time \eqn{t} is calculated as:
#' \eqn{\sqrt{(x_t - x_{t-1})^2 + (y_t - y_{t-1})^2}}
#'
#' @return The step length of each relocation.
step_length <- function(x) {
  stopifnot(is(x, "matrix") | is(x, "data.frame"))
  stopifnot(ncol(x) == 2)
  dx <- x[-1, ] - x[-nrow(x), ]
  ans <- c(NA, sqrt(rowSums(dx^2)))
  ans <- as.vector(ans) #remove names
  return (ans)
}
