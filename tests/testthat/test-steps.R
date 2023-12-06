test_that("multiplication works", {
  set.seed(1234)
  xy <- data.frame(x = rnorm(100), y = rnorm(100))
  dx <- xy[-1, 1] - xy[-nrow(xy), 1]
  dy <- xy[-1, 2] - xy[-nrow(xy), 2]
  dist <- c(NA, sqrt(dx^2 + dy^2))
  expect_identical(dist, step_length(xy))
})
