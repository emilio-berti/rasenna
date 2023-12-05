test_that("resample is correct", {
  x <- data.frame(
    x = rnorm(11),
    y = rnorm(11),
    time = as.POSIXct(seq_len(11) * 3600, origin = "1970-01-01") # 1 hour intervals
  )
  x$time[4] <- x$time[5] # 4 should be kept with new ID and 5 should be removed
  x$time[9] <- x$time[8] # 9 should be removed and 10 kept with new ID

  ras <- resample(x, ts_col = "time", freq = 1, units = "hours")
  expect_equal(nrow(ras), nrow(x) - 3) #correct dimensions
  expect_identical(rownames(ras), as.character(c(2:4, 6:8, 10:11))) #correct fixes kept
  expect_identical(ras$ID, c(1, 1, 2,   2, 2, 2, 3,   3))
  expect_identical(ras$dt, c(1, 1, NaN, 1, 1, 1, NaN, 1))
})
