
# NeutronNoise <img src="man/figures/sticker.png" align="right" />

<!-- badges: start -->
[![Build Status](https://travis-ci.org/IRSN/NeutronNoise.svg?branch=master)](https://travis-ci.org/IRSN/NeutronNoise)
[![codecov](https://codecov.io/gh/IRSN/NeutronNoise/branch/master/graph/badge.svg)](https://codecov.io/gh/IRSN/NeutronNoise)
[![R build status](https://github.com/IRSN/NeutronNoise/workflows/R-CMD-check/badge.svg)](https://github.com/IRSN/NeutronNoise/actions)
<!-- badges: end -->

#   Tools for neutron noise analysis

Installation
------------

```r
# Install devtools, if you haven't already.
install.packages("devtools")

devtools::install_github("IRSN/NeutronNoise")
```

Features
--------

- Multi-Thread Feynman histogram calculation.

- Artificial signal generation.


Examples
--------
```r

# Generate artificial signal of duration 1000 s with about 5 uncorrelated detections/s and plot it
artificial_signal(duration = 1000, uncorr_rate = 5) %>% plot()

```

