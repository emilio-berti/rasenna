#' @title Estimate the Concentration of the Von Mises distribution.
#' @param phi, vector of directions.
#' @param phi_hat, vector of relative directions.
#' @details Kappa is calculated using the magnitude of the mean vector (R),
#'  which is the length of the average direction vector relative to the
#'  home range center. Details are in Moorcroft & Lewis (2006), p.142.
#' @return numeric, the value of kappa.
#' @examples
#' data(belcho)
#' xy <- belcho[1:100, c("x", "y")]
#' ltm_kappa(direction(xy), relative_direction(xy))
ltm_kappa <- function(phi, phi_hat) {
  C <- sum(cos(phi - phi_hat), na.rm = TRUE) / length(phi)
  S <- sum(sin(phi - phi_hat), na.rm = TRUE) / length(phi)
  R <- sqrt(C ^ 2 + S ^ 2)
  if (R < 0.53) {
    kappa <- 2*R + R^3 + 5*R^5/6
  } else if (R < 0.85) {
    kappa <- -0.4 + 1.39*R + 0.43/(1 - R)
  } else {
    kappa <- 1 / (R^3 - 4*R^2 + 3*R)
  }
  return (kappa)
}

#' @title Estimate the Mean Distance Between Relocations.
#' @param x matrix of coordinates, one column for x and one for y.
#' @return numeric, the value of mean distance (rho).
#' @examples
#' data(belcho)
#' xy <- belcho[1:100, c("x", "y")]
#' ltm_rho(xy)
ltm_rho <- function(x) {
  if (!is(x, "matrix")) {
    x <- as.matrix(x)
  }
  dx <- x[-1, 1] - x[-nrow(x), 1]
  dy <- x[-1, 2] - x[-nrow(x), 2]
  rho <- mean(sqrt(dx ^ 2 + dy ^ 2))
  return (rho)
}

