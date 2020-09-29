

context("Artificial signal")

test_that("normal", {
  
  set.seed(1)
  
  expect_equal(artificial_signal(3, uncorr_rate = 5), 
               readRDS(file.path("test-artificial_signal_data", "000.rds")))
    
  set.seed(1)
  
  expect_equal(artificial_signal(3, uncorr_rate = 5, hists_id = TRUE),
               readRDS(file.path("test-artificial_signal_data", "001.rds")))
  
  
  set.seed(1)
  
  expect_equal(artificial_signal(3, hists_rate = 5, fission_multiplicity = data.frame(nu=2:3, pdf=c(0.2,0.6))),
               readRDS(file.path("test-artificial_signal_data", "005.rds")))
  
  set.seed(1)
  
  expect_equal(artificial_signal(3, hists_rate = 5, fission_multiplicity = data.frame(nu=2:3, pdf=c(0.2,0.6)), hists_id = TRUE),
               readRDS(file.path("test-artificial_signal_data", "006.rds")))
  
  set.seed(1)
  
  expect_equal(artificial_signal(3, uncorr_rate = 3, hists_rate = 5, fission_multiplicity = data.frame(nu=2:3, pdf=c(0.2,0.6))),
               readRDS(file.path("test-artificial_signal_data", "010.rds")))
  
  set.seed(1)
  
  expect_equal(artificial_signal(3, uncorr_rate = 3, hists_rate = 5, fission_multiplicity = data.frame(nu=2:3, pdf=c(0.2,0.6)), hists_id = TRUE),
               readRDS(file.path("test-artificial_signal_data", "011.rds")))
})



