#' @title Bearing of GPS fixes
#'
#' @param df data.frame with columns 'x' and 'y'.
#'
#' @return Vector of the bearing.
#'
#' @examples
#' data(belcho)
#' bearing(belcho)
bearing <- function(df) {
  stopifnot("x" %in% colnames(df))
  stopifnot("y" %in% colnames(df))
  out <- .bearing(df[["x"]], df[["y"]])
  out <- c(NA, out)

  # if track ID present, remove values
  if ("ID" %in% colnames(df)) {
    gaps <- which(diff(df[["ID"]]) != 0)
    out[gaps + 1] <- NA
  }

  return(out)
}

#' @title Turning angle between GPS fixes
#'
#' @param df data.frame with columns 'x' and 'y'.
#'
#' @return Vector of the turning angle.
#'
#' @examples
#' data(belcho)
#' turning_angle(belcho)
turning_angle <- function(df) {
  stopifnot("x" %in% colnames(df))
  stopifnot("y" %in% colnames(df))
  out <- .turning_angle(df[["x"]], df[["y"]])
  out <- c(NA, NA, out)

  # if track ID present, remove values
  if ("ID" %in% colnames(df)) {
    gaps <- which(diff(df[["ID"]]) != 0)
    out[gaps + 1] <- NA
    out[gaps + 2] <- NA
  }

  return(out)
}
