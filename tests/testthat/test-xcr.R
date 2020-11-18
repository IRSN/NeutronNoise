
context("XCR")

# test_that("Error", {
#   
#   expect_error(as_signal(c("oo", "oo"), x_duration = 10))
#   
#   expect_error(as_signal(1:10, x_duration = -20))
#   
#   expect_error(as_signal(-1:10, x_duration = 20))
#   
#   expect_error(as_signal(data.frame(aa = 1:10), x_duration = 20))
#   
#   expect_error(as_signal(data.frame(TIME = 1:10), x_duration = -20))
#   
#   expect_error(as_signal(data.frame(TIME = -1:10), x_duration = 20))
# })

test_that("Normal", {
  
  din <- data.table::fread(file.path("data", "ntrac_000.ncap"))
  
  set.seed(1)
  
  xout <- expect_error(as_signal(din, x_duration = 1000, is_t0 = TRUE) %>% feynman_hist(samples_widths = c(0.8, 0.9)) %>% xcr(), NA)
  
  expect_equal(xout$samples_width, c(0.8, 0.9))
  
  expect_equal(xout$Y1, c(1.809, 1.809), tolerance = 1e-3)
  
  expect_equal(xout$Y2, c(0.01300760, 0.02086617), tolerance = 1e-3)
  
})


