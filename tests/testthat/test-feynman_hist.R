

test_that("wrong parameters values", {
  
  # x is unsorted
  expect_error(feynman_hist(10:1, samples_widths = 2), "x must be positive and sorted.")
  
  # x is sorted but contains negative values
  expect_error(feynman_hist(-1:10, samples_width = 2), "x must be positive and sorted.")
  
  # Signal duration is a multiple of samples width
  set.seed(1)
  x <- runif(1000, min = 0, max = 10)
  d <- table(head(hist(x, seq(from = 0, to = 10, by = 2), plot = FALSE)$counts, n = -1))
  expect_equal(feynman_hist(sort(x), samples_width = 2), 
               data.frame(samples_width = 2, 
                          nb_samples = 4, 
                          multiplet = as.integer(names(d)), 
                          frequency = as.integer(d)))
  
 
  # Signal duration is not a multiple of samples width
  set.seed(1)
  x <- c(runif(1000, min = 0, max = 10), runif(100, 0.55, 0.999))
  d <- table(head(hist(x, c(seq(from = 0, to = 10, by = 0.77), 10), plot = FALSE)$counts, n = -1))
  expect_equal(feynman_hist(sort(x), samples_width = 0.77), 
               data.frame(samples_width = 0.77, 
                          nb_samples = 12, 
                          multiplet = as.integer(names(d)), 
                          frequency = as.integer(d)))
  
  
})
