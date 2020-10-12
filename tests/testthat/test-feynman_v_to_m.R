

# testthat::test_file("tests/testthat/test-feynman_v_to_m.R")

context("Feynman Variance-to-Mean")

# =================================================================================================
#
# =================================================================================================
test_that("wrong parameters values", {
  
  # x is unsorted
  d <- data.frame(TIME = 10:1)
  attr(d, "duration") <- 10
  expect_error(feynman_v2m(d, samples_widths = 2), "x is empty or not sorted or not positive.")
  
  # x is sorted but contains negative values
  d <- data.frame(TIME = -1:10)
  attr(d, "duration") <- 10
  expect_error(feynman_v2m(d, samples_widths = 2), "x is empty or not sorted or not positive.")
  
  # signal duration negative value
  d <- data.frame(TIME = 1:10)
  attr(d, "duration") <- -10
  expect_error(feynman_v2m(d, samples_widths = 2), "Negative or zero value for signal duration.")
  
  # signal duration 0 value
  d <- data.frame(TIME = 1:10)
  attr(d, "duration") <- 0
  expect_error(feynman_v2m(d, samples_widths = 2), "Negative or zero value for signal duration.")
  
})

# =================================================================================================
#
# =================================================================================================
test_that("Samples width multiple of signal duration", {

  library(dplyr)
  
  set.seed(1)
  signal_duration <- 10
  x <- sort(runif(1000, min = 0, max = signal_duration))
  
  ref <- data.table::rbindlist(lapply(c(0.01, 0.05), function(samples_width) {
    d <- hist(x, unique(sort(c(seq(from = 0, to = signal_duration, by = samples_width), signal_duration))), plot = FALSE)$counts
    list(samples_widths = samples_width,
         nb_samples = length(d),
         Y1 = sum(d) / (length(d) * samples_width),
         Y = var(d) / mean(d) - 1)
  }))
  data.table::setDF(ref)
  class(ref) <- c("feynman_v2m", "data.frame")
  
  xx <- as_signal(x, x_duration = signal_duration)
  res <- feynman_v2m(xx, samples_widths = c(0.01, 0.05)) %>% select(samples_widths, nb_samples, Y1, Y)
  
  expect_equal(res, ref)
})

# =================================================================================================
#
# =================================================================================================
test_that("Signal duration not multiple of samples widths", {
  
  library(dplyr)

  set.seed(1)
  signal_duration <- 10
  x <- sort(runif(1000, min = 0, max = signal_duration))
  
  ref <- data.table::rbindlist(lapply(c(0.0123, 0.051), function(samples_width) {
    d <- head(hist(x, unique(sort(c(seq(from = 0, to = signal_duration, by = samples_width), signal_duration))), plot = FALSE)$counts, n = -1)
    list(samples_widths = samples_width,
         nb_samples = length(d),
         Y1 = sum(d) / (length(d) * samples_width),
         Y = var(d) / mean(d) - 1)
  }))
  data.table::setDF(ref)
  class(ref) <- c("feynman_v2m", "data.frame")
  
  xx <- as_signal(x, x_duration = signal_duration)
  res <- feynman_v2m(xx, samples_widths = c(0.0123, 0.051)) %>% select(samples_widths, nb_samples, Y1, Y)
  
  expect_equal(res, ref)
})


# =================================================================================================
#
# =================================================================================================
test_that("Signal duration is shorter", {
  
  library(dplyr)
  
  set.seed(1)
  signal_duration <- 10
  x <- sort(runif(1000, min = 0, max = signal_duration))
  
  signal_duration <- 7.5
  
  ref <- data.table::rbindlist(lapply(c(0.0123, 0.051), function(samples_width) {
    d <- head(hist(x[x < signal_duration], unique(sort(c(seq(from = 0, to = signal_duration, by = samples_width), signal_duration))), plot = FALSE)$counts, n = -1)
    list(samples_widths = samples_width,
         nb_samples = length(d),
         Y1 = sum(d) / (length(d) * samples_width),
         Y = var(d) / mean(d) - 1)
  }))
  data.table::setDF(ref)
  class(ref) <- c("feynman_v2m", "data.frame")
  
  xx <- expect_warning(as_signal(x, x_duration = signal_duration), "x_duration does not cover all the signal.")
  
  res <- expect_warning(feynman_v2m(xx, samples_widths = c(0.0123, 0.051)) %>% select(samples_widths, nb_samples, Y1, Y), 
                        "x_duration does not cover all the signal.")
  
  expect_equal(res, ref)
})


# =================================================================================================
#
# =================================================================================================
test_that("Signal duration is longuer", {
  
  library(dplyr)
  
  set.seed(1)
  signal_duration <- 10
  x <- sort(runif(1000, min = 0, max = signal_duration))
  
  signal_duration <- 12.55
  
  ref <- data.table::rbindlist(lapply(c(0.0123, 0.051), function(samples_width) {
    d <- head(hist(x, unique(sort(c(seq(from = 0, to = signal_duration, by = samples_width), signal_duration))), plot = FALSE)$counts, n = -1)
    list(samples_widths = samples_width,
         nb_samples = length(d),
         Y1 = sum(d) / (length(d) * samples_width),
         Y = var(d) / mean(d) - 1)
  }))
  data.table::setDF(ref)
  class(ref) <- c("feynman_v2m", "data.frame")
  
  xx <- as_signal(x, x_duration = signal_duration)
  
  res <- feynman_v2m(xx, samples_widths = c(0.0123, 0.051)) %>% select(samples_widths, nb_samples, Y1, Y)
  
  expect_equal(res, ref)
})


# =================================================================================================
#
# =================================================================================================
test_that("Limited number of samples", {
  
  library(dplyr)
  
  set.seed(1)
  signal_duration <- 10
  x <- sort(runif(1000, min = 0, max = signal_duration))
  
  ref <- data.table::rbindlist(lapply(c(0.0123, 0.051), function(samples_width) {
    d <- head(hist(x, unique(sort(c(seq(from = 0, to = signal_duration, by = samples_width), signal_duration))), plot = FALSE)$counts, n = 150)
    list(samples_widths = samples_width,
         nb_samples = length(d),
         Y1 = sum(d) / (length(d) * samples_width),
         Y = var(d) / mean(d) - 1)
  }))
  data.table::setDF(ref)
  class(ref) <- c("feynman_v2m", "data.frame")
  
  xx <- as_signal(x, x_duration = signal_duration)
  
  res <- feynman_v2m(xx, samples_widths = c(0.0123, 0.051), max_nb_samples = 150) %>% select(samples_widths, nb_samples, Y1, Y)
  
  expect_equal(res, ref)
})

