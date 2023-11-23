#' @title Calculate direction
#' @param x1 coordinates at time 1.
#' @param x2 coordinates at time 2.
#' @param units either "rad" for radians of "deg" for degree.
#' @return numeric, the direction angle, see details.
#' @details Direction is calculated as the angle between two relocations.
#' @examples
#' x1 <- c(0, 0)
#' x2 <- c(1, 1)
#' direction(x1, x2, units = "deg")
direction <- function(
    x1,
    x2,
    units = "rad"
) {
  stopifnot (length(x1) == 2)
  stopifnot (length(x2) == 2)
  x <- x2 - x1
  if (all(x == 0)) {
    warning("no movement")
    return (NA)
  }
  ans <- atan2(x[2], x[1])
  if (units == "deg") ans <- ans * 180 / pi
  return (ans)
}

#' @title Calculate direction serial correlation
#' @param x matrix of coordinates, one column for x and one for y.
#' @param units either "rad" for radians of "deg" for degree.
#' @param method either "pearson" or "spearman".
#' @param plot plot the directions.
#' @return an object of class "htest", from cor.test().
#' @details Only lag 1 correlation is tested.
#' @examples
#' set.seed(123)
#' x <- matrix(rnorm(100), 50, 2)
#' cor.test.direction(x, units = "deg")
#' # give some correlation to direction
#' for (i in seq_len(nrow(x))) {
#'   if (i > 2) {
#'     d <- direction(x[i - 2, ], x[i - 1, ])
#'     d <- rnorm(1, d, 1)
#'     l <- sqrt ( sum((x[2, ] - x[1, ]) ^ 2) )
#'     x[i, ] <- x[i - 1, ] + c(l * cos(d), l * sin(d))
#'   }
#' }
#' cor.test.direction(x, units = "deg")
cor.test.direction <- function(
    x,
    method = "pearson",
    units = "rad",
    plot = TRUE
) {
  stopifnot (is(x, "matrix"))
  stopifnot (ncol(x) == 2)
  x <- x[, 2] - x[, 1]
  d <- atan2(x[-1], x[-length(x)])
  if (units == "deg") d <- d * 180 / pi
  if (plot) {
    plot(
      d[-1], d[-length(d)],
      pch = 20, cex = .5, frame = FALSE,
      xlab = "Direction (t)",
      ylab = "Direciton (t + 1)"
    )
    abline(0, 1, lty = 2)
  }
  ans <- cor.test(d[-1], d[-length(d)], method = method)
  return (ans)
}
