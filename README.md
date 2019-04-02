# BayesPD

## Installation

``` r
devtools::install_github("Kay-Xingchi/BayesPD", force = TRUE)
```

## Example

``` r
library(BayesPD)

beta_sample_comparison(sample_size = 9000, beta_a1 = 50:60, beta_b1 = 40:50, beta_a2 = 31, beta_b2 = 21, plot = TRUE)
beta_sample_comparison(sample_size = 5000, beta_a1 = 58, beta_b1 = 44, beta_a2 = 30:40, beta_b2 = 20:30, plot = TRUE)

posterior_predictive_sample_comparison(sample_size = 5000, gamma_a1 = 237, gamma_b1 = 20, gamma_a2 = 12*(1:100)+113, gamma_b2 = 13+(1:100), tildeya_smaller_than_tildeyb = FALSE, plot = TRUE)
```