
context("Useful")

test_that("lseq", {
  expect_equal(lseq(1, 5, 3), c(1, 2.236068, 5))
})


test_that("stop_if", {
  expect_error(stop_if(TRUE, "error"))
})