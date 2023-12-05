test_that("direction is correct", {
  set.seed(1234)
  x <- matrix(rnorm(2e3), ncol = 2)
  dx <- x[-1, 1] - x[-nrow(x), 1]
  dy <- x[-1, 2] - x[-nrow(x), 2]
  dir <- atan2(dy, dx)
  ras <- direction(x)[2:nrow(x)]
  expect_identical(dir, ras)
})

test_that("relative direction", {
  set.seed(1234)
  x <- matrix(rnorm(2e3), ncol = 2)
  x_h <- colMeans(x)
  dx <- x_h[1] - x[, 1]
  dy <- x_h[2] - x[, 2]
  dir <- atan2(dy, dx)
  ras <- relative_direction(x, x_h)
  expect_identical(dir, ras)
})
