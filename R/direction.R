#' @title Calculate direction
#' @param x matrix of coordinates, one column for x and one for y.
#' @param units either "rad" for radians of "deg" for degree.
#' @return numeric, the direction angle, see details.
#' @details Direction is calculated as the angle between two relocations.
#' @examples
#' x <- matrix(c(0, 0, 1, 1, 2, 3), byrow = TRUE, ncol = 2)
#' direction(x, units = "deg")
direction <- function(x, units = "rad") {
  if (!is(x, "matrix")) {
    x <- as.matrix(x)
  }
  stopifnot (ncol(x) == 2)
  dx <- x[-1, ] - x[-nrow(x), ]
  if (is(dx, "numeric")) dx <- matrix(dx, ncol = 2)
  ans <- atan2(dx[, 2], dx[, 1])
  if (any(rowSums(abs(dx)) == 0)) {
    warning("no movement between some fixes")
    empty <- which(rowSums(abs(dx)) == 0)
    ans[empty] <- NA
  }
  if (units == "deg") ans <- ans * 180 / pi
  ans <- c(NA, ans) #add first not-known direction
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
#' #' # give some correlation to direction
#' for (i in seq_len(nrow(x))) {
#'   if (i > 2) {
#'     d <- direction(matrix(c(x[i - 2, ], x[i - 1, ]), byrow = TRUE, ncol = 2))
#'     d <- rnorm(1, d[2], .5)
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
  if (!is(x, "matrix")) {
    x <- as.matrix(x)
  }
  stopifnot (ncol(x) == 2)
  d <- direction(x)
  d <- d[-1]
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

#' @title Estimate Relative Direction
#' @param x matrix of coordinates, one column for x and one for y.
#' @param xh matrix of coordinates of home range center.
#' @param units either "rad" for radians of "deg" for degree.
#' @details The relative direction is calculated as the angle between a
#'  location and the center of the home range.
#' @return vector of relative directions.
relative_direction <- function(x, xh = NULL, units = "rad") {
  if (!is(x, "matrix")) {
    x <- as.matrix(x)
  }
  if (is.null(xh)) {
    message("Home range center calculated as average coordinates")
    xh <- matrix(colMeans(xy), ncol = 2)
  }
  if (!is(xh, "matrix")) {
    xh <- as.matrix(xh)
  }
  ans <- atan2(xh[, 2] - xy[, 2], xh[, 1] - xy[, 1])
  return (ans)
}
