#' @title Track smoothing
#' @description Smooth small location errors in a relocation track.
#' @details
#'  Smoothing tried to remove small location errors from a track.
#'  This is achieved by defining a moving window of size 'K' and
#'  using all the values within this moving windows the correct
#'  the focus value, i.e. the value the window is centered around.
#'  The correction is either an averaging across all values in 
#'  the window or the median. Median smoothing is usually preferred 
#'  as it is robust to outliers and large temporal and spatial
#'  gaps in the track.
#' @importFrom stats median
#' @param df data.frame of the track.
#' @param K the size of the moving window.
#' @param method either "median" or "mean".
#' @return The track smoothed
#' @examples
#' set.seed(123)
#' data(capra)
#' sm <- smooth_track(capra, 3, "median")
#' d <- sqrt((capra$x - sm$x)^2 + (capra$y - sm$y)^2)
#' summary(d) # meters
#' k_vals <- seq(3, 23, by = 2)
#' sm <- lapply(k_vals, \(k) smooth_track(capra, k, "median"))
#' d <- sapply(
#'  sm,
#'  \(x) mean(sqrt((capra$x - x$x)^2 + (capra$y - x$y)^2))
#' )
#' plot(k_vals, d, xlab = "K", ylab = "Average adjustment (m)", axes = FALSE, type = "b")
#' axis(1, k_vals)
#' axis(2, seq(50, 300, by = 25))
smooth_track <- function(df, K, method = c("mean", "median")) {

  if (K %% 2 == 0) stop("K should be an odd number.")

  method <- match.arg(method)
  smooth_fun <- switch(
    method,
    mean = \(x) mean(x, na.rm = TRUE),
    median = \(x) median(x, na.rm = TRUE)
  )

  # i the in index of the track relocation.
  # K is the size of the moving window.
  # max_i is the maximum value that i can take.
  window <- function(i, K, max_i) {
    start <- i - (K - 1) / 2
    end <- i + (K - 1) / 2
    out <- seq(start, end)
    # if i is close to the start of the track
    out <- out[out > 0]
    # if i is close to the end of the track
    out <- out[out <= max_i]
    return(out)
  }

  n <- nrow(df)

  # list of windows
  win <- lapply(seq_len(nrow(df)), \(i) window(i, K, n))

  # smooth
  smoothed_x <- vapply(win, \(w) smooth_fun(df$x[w]), numeric(1))
  smoothed_y <- vapply(win, \(w) smooth_fun(df$y[w]), numeric(1))
  
  out <- df
  out[["x"]] <- smoothed_x
  out[["y"]] <- smoothed_y
  return(out)
}
