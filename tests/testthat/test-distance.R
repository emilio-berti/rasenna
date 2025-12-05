test_that("distance function", {
  
  dx <- belcho$x[-1] - belcho$x[-nrow(belcho)]
  dy <- belcho$y[-1] - belcho$y[-nrow(belcho)]
  d <- sqrt(dx ^ 2 + dy ^ 2)
  rcpp_d <- distance(belcho)
  rcpp_d <- rcpp_d[!is.na(rcpp_d)]

  # correct size
  expect_length(rcpp_d, length(d))

  # correct values
  expect_equal(d, rcpp_d)

})
