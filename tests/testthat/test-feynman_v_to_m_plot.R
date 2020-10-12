

context("Feynman Variance-to-Mean plot")

test_that("Normal", {
  
 expect_equal(class(artificial_signal(duration = 1000, uncorr_rate = 5) %>% 
                      feynman_v2m(samples_widths = lseq(from = 0.001, to = 10, 50)) %>% plot()), 
              c("gg", "ggplot"))
    
})
