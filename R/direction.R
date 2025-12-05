#' @title Calculate direction serial correlation
#' @importFrom graphics axis
#' @param df data.frame of the track.
#' @param method either "pearson" or "spearman".
#' @param graph plot the directions.
#' @return an object of class "htest", from cor.test().
#' @details Only lag 1 correlation is tested.
#' @examples
#' set.seed(123)
#' x <- matrix(rnorm(100), 50, 2)
#' cor_test_direction(x, units = "deg")
#' #' # give some correlation to direction
#' for (i in seq_len(nrow(x))) {
#'   if (i > 2) {
#'     d <- direction(matrix(c(x[i - 2, ], x[i - 1, ]), byrow = TRUE, ncol = 2))
#'     d <- rnorm(1, d[2], .5)
#'     l <- sqrt ( sum((x[2, ] - x[1, ]) ^ 2) )
#'     x[i, ] <- x[i - 1, ] + c(l * cos(d), l * sin(d))
#'   }
#' }
#' cor_test_direction(x, units = "deg")
cor_test_direction <- function(
    df,
    method = "pearson",
    graph = TRUE
) {
  stopifnot("x" %in% colnames(df))
  stopifnot("y" %in% colnames(df))
  
  if (!"bearing" %in% colnames(df)) {
    b <- bearing(df)
  } else {
    b <- df[["bearing"]]
  }

  if (graph) {
    plot(
      b[-1], b[-length(b)],
      pch = 20, cex = .5, frame = FALSE,
      xlab = "Direction (t)",
      ylab = "Direciton (t + 1)",
      axes = FALSE,
      asp = 1
    )
    axis(1, seq(0, 2*pi, length.out = 4), c("E", "N", "W", "S"))
    axis(2, seq(0, 2*pi, length.out = 4), c("E", "N", "W", "S"))
  }

  out <- cor.test(b[-1], b[-length(b)], method = method)

  return (out)
}
