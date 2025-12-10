#' @title Relocation Outliers
#' @description Try to find outliers in the track 
#' @details Relocation points are flagged as outliers if:
#'  - Have previous or subsequent movement with speed and turning angle
#'    exceeding the 'quant' quantile.
#'  - Are outside the 'quant' isopleth of the home range calculated using 'UD' 
#'    (see `homerange()` for details).
#' @param df data.frame of the track.
#' @param quant quantile for outlier detection. 
#'  Must be a number between 0.05 and 1.00 at intervals of 0.05, 0.995, or 0.999.
#'  Default = 0.999.
#' @param ... additional arguments for kernel density estimation. See `homerange()`.
#' @return data.frame with the new column 'flagOutlier'.
#'  This colums has as value:
#'   - 0 if the relocation was not detected as an outlier.
#'   - 1 if the relocation was detected as a speed/angle outlier.
#'   - 2 if the relocation was detected as a home range outlier.
#'   - 3 if the relocation was detected as bot a speed/angle and a home range
#'       outlier.
#' @examples
#'  data(capra)
#'  capra <- flag_outliers(capra)
flag_outliers <- function(df, quant = 0.999, ...) {

  if (!"speed" %in% colnames(df)) {
    df <- annotate(df)
  }

  # speed/angle outliers -----------------------------------
  q_speed <- quantile(df$speed, quant, na.rm = TRUE)
  q_angle <- quantile(df$turningAngle, quant, na.rm = TRUE)
  too_fast <- df$speed > q_speed & df$turningAngle > q_angle
  stopifnot(length(too_fast) == nrow(df))

  # home range outliers -----------------------------------
  # isopleth lines
  v <- vect(df, geom = c("x", "y"), crs = "EPSG:7791")
  isolines <- homerange(v, method = "UD", ...)$isopleth
  isolines <- isolines[isolines$level == quant]
  isolines <- disagg(isolines)

  # if lines are not closed, close them with straigh line
  isopolys <- as.list(rep(NA, length(isolines)))
  for (i in seq_along(isolines)) {
    g <- geom(isolines[i])
    if (any(g[1, c("x", "y")] != g[nrow(g), c("x", "y")])) {
      g[nrow(g), c("x", "y")] <- g[1, c("x", "y")]
    }
    isopolys[[i]] <- vect(g, crs = crs(v), type = "polygons")
  }
  isopolys <- vect(isopolys)
  isopolys <- aggregate(isopolys)

  # spatial relation
  inside <- relate(v, isopolys, relation = "within")
  if (ncol(inside) > 1) {
    inside <- rowSums(inside)
    inside <- as.logical(inside)
  } else {
    inside <- inside[, 1]
  }

  # check sizes
  stopifnot(length(inside) == nrow(df))

  # column for flag
  flag <- numeric(nrow(df))
  flag[too_fast & inside] <- 1
  flag[!too_fast & !inside] <- 2
  flag[too_fast & !inside] <- 3

  # verbose messages
  if (any(flag == 1)) message(sum(flag == 1), " potential speed/angle ouliers")
  if (any(flag == 2)) message(sum(flag == 2), " potential home range ouliers")
  if (any(flag == 3)) message(sum(flag == 3), " potential speed/angle and home range ouliers")

  # return
  out <- df
  out[["flagOutlier"]] <- flag
  return(out)

}
