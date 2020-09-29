

context("Feynman plot")

test_that("Normal", {
  
 expect_equal(class(artificial_signal(duration = 1000, uncorr_rate = 5) %>% feynman_hist(0.2) %>% plot()), 
              c("gg", "ggplot"))
    
})
