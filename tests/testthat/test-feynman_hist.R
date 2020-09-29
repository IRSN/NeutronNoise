

context("Feynman hist")

test_that("wrong parameters values", {
  
  # x is unsorted
  expect_error(feynman_hist(10:1, samples_widths = 2), "x must be positive and sorted.")
  
  # x is sorted but contains negative values
  expect_error(feynman_hist(-1:10, samples_width = 2), "x must be positive and sorted.")
  
  # Signal duration is a multiple of samples width
  set.seed(1)
  x <- runif(1000, min = 0, max = 10)
  d <- table(head(hist(x, seq(from = 0, to = 10, by = 2), plot = FALSE)$counts, n = -1))
  d <- data.frame(samples_width = 2, 
                  nb_samples = 4, 
                  multiplet = as.integer(names(d)), 
                  frequency = as.integer(d))
  class(d) <- c("feynman_hist", "data.frame")
  expect_equal(feynman_hist(sort(x), samples_width = 2), d)
  
 
  # Signal duration is not a multiple of samples width
  set.seed(1)
  x <- c(runif(1000, min = 0, max = 10), runif(100, 0.55, 0.999))
  d <- table(head(hist(x, c(seq(from = 0, to = 10, by = 0.77), 10), plot = FALSE)$counts, n = -1))
  d <- data.frame(samples_width = 0.77, 
                  nb_samples = 12, 
                  multiplet = as.integer(names(d)), 
                  frequency = as.integer(d))
  class(d) <- c("feynman_hist", "data.frame")
  expect_equal(feynman_hist(sort(x), samples_width = 0.77), d)
  
  # Multiple gates widths
  set.seed(1)
  x <- c(runif(1000, min = 0, max = 10), runif(100, 0.55, 0.999))
  d1 <- table(head(hist(x, c(seq(from = 0, to = 10, by = 0.77), 10), plot = FALSE)$counts, n = -1))
  d2 <- table(head(hist(x, c(seq(from = 0, to = 10, by = 0.51), 10), plot = FALSE)$counts, n = -1))
  d3 <- table(head(hist(x, c(seq(from = 0, to = 10, by = 0.212), 10), plot = FALSE)$counts, n = -1))
  d <- data.frame(samples_width = c(rep(0.77, length(d1)), rep(0.51, length(d2)), rep(0.212, length(d3))), 
                  nb_samples = c(rep(sum(d1), length(d1)), rep(sum(d2), length(d2)), rep(sum(d3), length(d3))), 
                  multiplet = c(as.integer(names(d1)), as.integer(names(d2)), as.integer(names(d3))),
                  frequency = c(as.integer(d1), as.integer(d2), as.integer(d3)))
  class(d) <- c("feynman_hist", "data.frame")
  expect_equal(feynman_hist(sort(x), samples_width = c(0.77, 0.51, 0.212)), d)
    
})
