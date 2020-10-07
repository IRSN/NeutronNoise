
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


