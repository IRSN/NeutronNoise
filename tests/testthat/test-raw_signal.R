
context("Raw signal")

test_that("Error", {
  
  expect_error(as_signal(c("oo", "oo"), x_duration = 10))
  
  expect_error(as_signal(1:10, x_duration = -20))
  
  expect_error(as_signal(-1:10, x_duration = 20))
  
  expect_error(as_signal(data.frame(aa = 1:10), x_duration = 20))
  
  expect_error(as_signal(data.frame(TIME = 1:10), x_duration = -20))
  
  expect_error(as_signal(data.frame(TIME = -1:10), x_duration = 20))
})

test_that("Normal", {
  
  xx <- runif(200, 0, 20)
  
  ref <- data.table::data.table(TIME = xx)
  setkey(ref, "TIME")
  class(ref) <- c("signal", "data.table", "data.frame")
  attr(ref, "duration") <- 20
  
  expect_equal(as_signal(xx, x_duration = 20), ref)
  
  expect_equal(as_signal(data.frame(TIME = xx), x_duration = 20), ref)
})

test_that("Normal T0", {
  
  din <- data.table::fread(file.path("data", "ntrac_000.ncap"))
  
  dout <- expect_error(as_signal(din, x_duration = 20, is_t0 = TRUE), NA)
  
  expect_equal(nrow(din), nrow(dout))
  
  expect_true(max(dout$TIME) <= 20 && min(dout$TIME) >= 0)
  
  expect_false(is.unsorted(dout$TIME))
  
})
