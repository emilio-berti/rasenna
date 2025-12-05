test_that("bearing and angle function", {
  data(belcho)

  # reference
  dx <- diff(belcho[["x"]])
  dy <- diff(belcho[["y"]])

  alpha <- atan2(dy, dx)
  alpha <- ifelse(alpha < 0, alpha + 2*pi, alpha)

  theta <- diff(b)
  theta <- (theta + pi) %% (2*pi) - pi

  # Cpp functions  
  b <- bearing(belcho)[["bearing"]]
  b <- b[!is.na(b)]
  t <- turning_angle(belcho)[["turningAngle"]]
  t <- t[!is.na(t)]

  # correct size
  expect_length(b, nrow(belcho) - 1)
  expect_length(t, nrow(belcho) - 2)

  # correct value range
  expect_true(all(b >= 0 & b < 2*pi))
  expect_true(all(t >= -pi & t <= pi))

  # correct values
  expect_equal(alpha, b)
  expect_equal(theta, t)

})
