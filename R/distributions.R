#' @title Von Mises Probability Density Function
#'
#' @param phi numeric, angle.
#' @param phi_hat numeric, mean angle of the distribution.
#' @param kappa numeric, concetration of the angle distribution.
#'
#' @return Probability \eqn{p(\phi | \hat{\phi}, \kappa)}.
von_mises <- function(
    phi,
    phi_hat,
    kappa
) {
  num <- exp(kappa * cos(phi - phi_hat))
  denom <- 2 * pi * besselI(kappa, 0)
  return (num / denom)
}

#' @title Negative Exponential Probability Density Function
#'
#' @param rho numeric, step length.
#' @param lambda numeric, decay rate, also equal to \eqn{1/\bar{\rho}}.
#'
#' @return Probability \eqn{p(\rho | \lambda)}.
exponential <- function(rho, lambda) {
  return (lambda * exp(-lambda * rho))
}
