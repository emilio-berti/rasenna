test_that("resample function", {
  x <- data.frame(
    x = rnorm(11),
    y = rnorm(11),
    time = as.POSIXct(seq_len(11) * 3600, origin = "1970-01-01") # 1 hour intervals
  )
  x$time[4] <- x$time[5] # 4 should be kept with new ID and 5 should be removed
  x$time[9] <- x$time[8] # 9 should be removed and 10 kept with new ID

  ras <- resample(x, ts_col = "time", freq = 1, units = "hours")
  
  # correct row ID
  expect_identical(rownames(ras), as.character(c(seq(1, 4), seq(6, 8), 10, 11)))

  # correct dimensions
  expect_equal(nrow(ras), nrow(x) - 2) 

  # correct track ID
  expect_identical(ras$ID, c(rep(1, 3), rep(2, 4), rep(3, 2)))

})
