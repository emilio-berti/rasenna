#' @title Home Range
#' @description Home range of the animal according to the relocation data.
#' @details
#'  Home range is calculated using different approaches, specified by the
#'  'method' argument. 'MCP' uses the minimum convex polygon approach.
#'  If using 'UD', a KDE is calculated. The KDE can be adjusted with the 
#'  optional dots arguments.
#' @param df data.frame of the track.
#' @param crs coordinate reference system of the track. This should be an
#'  object that can be passed to `terra::crs()`.
#' @param method either "MCP" or "UD".
#' @param show_map TRUE/FALSE.
#' @param ... additional arguments for kernel density estimation, including
#'  the 'resolution' of the raster, the smoothing parameter 'sigma' and the
#'  kernel 'size'.
#' @return A list with elements the home range ('homerange') and the 
#'  isopleths ('isopleth') is 'method' is 'UD'.
#' @examples
#' library(terra)
#' data(capra)
#' homerange(capra, crs = "EPSG:7791", method = "UD")
homerange <- function(df, method = c("MCP", "UD"), show_map = FALSE, ...) {

  method <- match.arg(method)
  dots <- list(...)

  if (!"distance" %in% colnames(df)) {
    df <- annotate(df)
  }
  x <- vect(df, geom = c("x", "y"), crs = crs)

  if (method == "MCP") {

    # minimum convex hull
    out <- list(homerange = convHull(x))

    if (show_map) {
      plot(x, alpha = .1, col = "dodgerblue3")
      lines(out$homerange)
    }

  } else if (method == "UD") {

    # utilization distribution

    # rasterize 'x'
    res <- ifelse(
      is.null(dots$resolution),
      50,
      dots$resolution
    )
    r <- rast(
      ext(buffer(x, res * 10)), # add 10 cells per side
      resolution = res,
      crs = crs(x)
    )
    kde <- rasterize(x, r, fun = "count", background = 0)
    
    # Gaussian weight matrix
    sigma <- ifelse(  # smoothing parameter in raw units 
      is.null(dots$sigma),
      res * 2,
      dots$sigma
    )
    sigma_cells <- sigma / res(r)[1]  # convert to raster cells
    size <- ifelse(
      is.null(dots$size),
      1 + 2 * ceiling(3 * sigma_cells),
      dots$size
    )
    win <- seq(-floor(size / 2), floor(size / 2), 1)
    gauss <- exp(-(win ^ 2)/(2 * sigma_cells ^ 2))
    kernel <- gauss %*% t(gauss)  # 2D Gaussian
    kernel <- kernel / sum(kernel)  # normalize

    # apply focal filter
    kde_smooth <- focal(
      kde,
      w = kernel,
      fun = sum,
      na.policy = "omit",
      pad = TRUE,
      padValue = 0
    )

    # normalize to PDF
    cell_area <- prod(res(kde_smooth))
    total_mass <- sum(values(kde_smooth), na.rm = TRUE) * cell_area
    kde_smooth <- kde_smooth / total_mass

    # isopleth lines
    p_levels <- seq(0.05, 1, by = 0.05)
    p_levels <- append(p_levels, 0.99, after = length(p_levels) - 1) # add 0.990
    p_levels <- append(p_levels, 0.995, after = length(p_levels) - 1) # add 0.995
    p_levels <- append(p_levels, 0.999, after = length(p_levels) - 1) # add 0.999
    p_levels <- round(p_levels, 3) # round needed to match == 0.95 (e.g.)
    vals <- values(kde_smooth)
    vals[is.na(vals)] <- 0
    ord <- order(vals, decreasing = TRUE)
    vals_sorted <- vals[ord] # decreasing order
    cum_prob <- cumsum(vals_sorted * cell_area)
    thresholds <- sapply(p_levels, function(p) {
      vals_sorted[which(cum_prob >= p)[1]]
    })
    thresholds[length(thresholds)] <- vals_sorted[2]
    isopleth <- lapply(thresholds, \(th) as.contour(kde_smooth, levels = th))
    isopleth <- vect(isopleth)
    isopleth$level <- p_levels

    out <- list(homerange = kde_smooth, isopleth = isopleth)

    if (show_map) {
      plot(
        crop(kde_smooth, ext(isopleth[isopleth$level <= 0.9])),
        col = hcl.colors(100, "Spectral", rev = TRUE)
      )
      plot(
        isopleth,
        "level",
        add = TRUE,
        col = hcl.colors(10, "Gray"),
        legend = FALSE
      )
    }

  }

  return(out)
}
