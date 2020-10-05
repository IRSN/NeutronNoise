

# testthat::test_file("tests/testthat/test-feynman_hist.R")

context("Feynman hist")

test_that("wrong parameters values", {
  
  # x is unsorted
  expect_error(feynman_hist(data.frame(TIME = 10:1), samples_widths = 2), "x is empty or not sorted or not positive.")
  
  # x is sorted but contains negative values
  expect_error(feynman_hist(data.frame(TIME = -1:10), samples_width = 2), "x is empty or not sorted or not positive.")
  
  # x is not has wront type
  #expect_error(feynman_hist("abc", samples_widths = 2), "Bad x type")
  
})

test_that("Normal", {
  
  # Signal duration is a multiple of samples width + x as data.frame + x as list
  set.seed(1)
  x <- sort(runif(1000, min = 0, max = 10))
  d <- table(hist(x, seq(from = 0, to = 10, by = 2), plot = FALSE)$counts)
  d <- data.frame(samples_width = 2, 
                  nb_samples = 5, 
                  multiplet = as.integer(names(d)), 
                  frequency = as.integer(d))
  class(d) <- c("feynman_hist", "data.frame")
  xx <- as_signal(x, x_duration = 10)
  expect_equal(feynman_hist(xx, samples_width = 2), d)
  
  # Limited number of samples
  set.seed(1)
  x <- sort(runif(1000, min = 0, max = 10))
  d <- table(head(hist(x, seq(from = 0, to = 10, by = 2), plot = FALSE)$counts, n = 2))
  d <- data.frame(samples_width = 2,
                  nb_samples = 2,
                  multiplet = as.integer(names(d)),
                  frequency = as.integer(d))
  class(d) <- c("feynman_hist", "data.frame")
  xx <- as_signal(x, x_duration = 10)
  expect_equal(feynman_hist(xx, samples_width = 2, max_nb_samples = 2), d)


  # Signal duration is not a multiple of samples width
  set.seed(1)
  x <- sort(c(runif(1000, min = 0, max = 10), runif(100, 0.55, 0.999)))
  d <- table(head(hist(x, c(seq(from = 0, to = 10, by = 0.77), 10), plot = FALSE)$counts, n = -1))
  d <- data.frame(samples_width = 0.77,
                  nb_samples = 12,
                  multiplet = as.integer(names(d)),
                  frequency = as.integer(d))
  class(d) <- c("feynman_hist", "data.frame")
  xx <- as_signal(x, x_duration = 10)
  expect_equal(feynman_hist(xx, samples_width = 0.77), d)

  # Signal duration shorter
  set.seed(1)
  x <- sort(c(runif(1000, min = 0, max = 10), runif(100, 0.55, 0.999)))
  d <- table(head(hist(x[x < 8.5], c(seq(from = 0, to = 8.5, by = 0.77), 10), plot = FALSE)$counts, n = -1))
  d <- data.frame(samples_width = 0.77,
                  nb_samples = 11,
                  multiplet = as.integer(names(d)),
                  frequency = as.integer(d))
  class(d) <- c("feynman_hist", "data.frame")
  xx <- suppressWarnings(as_signal(x, x_duration = 8.5))
  expect_equal(suppressWarnings(feynman_hist(xx, samples_width = 0.77)), d)
    
  
  # Signal duration longer
  set.seed(1)
  x <- sort(c(runif(1000, min = 0, max = 10), runif(100, 0.55, 0.999)))
  d <- table(head(hist(x, c(seq(from = 0, to = 12.3, by = 0.77), 10), plot = FALSE)$counts, n = -1))
  d <- data.frame(samples_width = 0.77,
                  nb_samples = 15,
                  multiplet = as.integer(names(d)),
                  frequency = as.integer(d))
  class(d) <- c("feynman_hist", "data.frame")
  xx <- as_signal(x, x_duration = 12.3)
  expect_equal(feynman_hist(xx, samples_width = 0.77), d)
  
  
  # Multiple gates widths
  set.seed(1)
  x <- sort(c(runif(1000, min = 0, max = 10), runif(100, 0.55, 0.999)))
  d1 <- table(head(hist(x, c(seq(from = 0, to = 10, by = 0.77), 10), plot = FALSE)$counts, n = -1))
  d2 <- table(head(hist(x, c(seq(from = 0, to = 10, by = 0.51), 10), plot = FALSE)$counts, n = -1))
  d3 <- table(head(hist(x, c(seq(from = 0, to = 10, by = 0.212), 10), plot = FALSE)$counts, n = -1))
  d <- data.frame(samples_width = c(rep(0.77, length(d1)), rep(0.51, length(d2)), rep(0.212, length(d3))),
                  nb_samples = c(rep(sum(d1), length(d1)), rep(sum(d2), length(d2)), rep(sum(d3), length(d3))),
                  multiplet = c(as.integer(names(d1)), as.integer(names(d2)), as.integer(names(d3))),
                  frequency = c(as.integer(d1), as.integer(d2), as.integer(d3)))
  class(d) <- c("feynman_hist", "data.frame")
  xx <- as_signal(x, x_duration = 10)
  expect_equal(feynman_hist(xx, samples_width = c(0.77, 0.51, 0.212)), d)
  
})


test_that("With other functions", {
  
  expect_error(as_signal(1:100, x_duration = 100) %>% feynman_hist(samples_width = c(0.77, 0.51, 0.212)), NA)  # No error
  
  expect_error(artificial_signal(duration = 10, 
                                 uncorr_rate = 5, 
                                 hists_rate = 20, 
                                 fission_multiplicity = data.frame(nu=1:2,pdf=c(0.5,0.5)), 
                                 k = 0.9, 
                                 lambda = 5, 
                                 hists_id  = TRUE) %>% feynman_hist(samples_width = c(0.77, 0.51, 0.212)), NA)  # No error
  
})