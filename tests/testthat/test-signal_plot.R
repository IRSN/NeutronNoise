

context("Signal plot")

test_that("Normal", {
  
 expect_equal(class(artificial_signal(duration = 1000, uncorr_rate = 5) %>% plot()), 
              c("gg", "ggplot"))
    
})
