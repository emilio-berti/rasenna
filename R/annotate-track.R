#' @title Annotate track
#'
#' @details `annotate()` add the columns 'distance', 'bearing', and 'turningAngle'
#'  to the data.frame 'df'.
#'
#' @param df data.frame with columns 'x' and 'y'.
#'
#' @return Original data.frame with the annotated columns of the bearing.
#'
#' @examples
#' data(belcho)
#' annotate(belcho)
annotate <- function(df) {
  stopifnot("x" %in% colnames(df))
  stopifnot("y" %in% colnames(df))
  df[["distance"]] <- distance(df)
  df[["bearing"]] <- bearing(df)
  df[["turningAngle"]] <- turning_angle(df)

  # difference between relocations (hours)
  lag_hour <- as.numeric(
    difftime(
      df[["timestamp"]][-1],
      df[["timestamp"]][-nrow(df)],
      units = "hour"
    )
  )
  lag_hour <- c(NA, lag_hour)
  lag_hour[is.na(df[["distance"]])] <- NA
  df[["lagHour"]] <- lag_hour

  # speed (km / hour)
  df[["speed"]] <- df[["distance"]] / df[["lagHour"]] / 1e3

  return(df)
}