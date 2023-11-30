von_mises <- function(
    phi,
    phi_hat,
    kappa
) {
  num <- exp(kappa * cos(phi - phi_hat))
  denom <- 2 * pi * besselI(kappa, 0)
  ans <- return (num / denom)
}

exponential <- function(rho, lambda) {
  return (lambda * exp(-lambda * rho))
}
