#' @title Distance between GPS fixes
#'
#' @param df data.frame with columns 'x' and 'y'.
#'
#' @return Vector of the Euclidean distances.
#'
#' @examples
#' data(belcho)
#' distance(belcho)
distance <- function(df) {
  stopifnot("x" %in% colnames(df))
  stopifnot("y" %in% colnames(df))
  out <- .distance(df[["x"]], df[["y"]])
  out <- c(NA, out)

  # if track ID present, remove values
  if ("ID" %in% colnames(df)) {
    gaps <- which(diff(df[["ID"]]) != 0)
    gaps <- sapply(gaps, \(g) min(g + 1, length(out)))
    out[gaps] <- NA
  }

  return(out)
}
