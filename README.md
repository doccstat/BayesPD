# BayesPD

## Installation

``` r
devtools::install_github("Kay-Xingchi/BayesPD", build_opts = c("--no-resave-data", "--no-manual"), force = TRUE, build_vignettes = TRUE)
```

## Examples

``` r
library(BayesPD)

vector_check(object = c(1)) == FALSE

vector_check(object = c(1), length = 1) == TRUE

vector_check(object = c(1, 1)) == TRUE

vector_check(object = c(1, 2), length = 2) == TRUE

vector_check(object = c(1, 1), length = 1) == FALSE

beta_sample_comparison(sample_size = 5000, beta_a1 = 58, beta_b1 = 44, beta_a2 = 31, beta_b2 = 21)

beta_sample_comparison(sample_size = 6000, beta_a1 = c(58, 58), beta_b1 = c(44, 50), beta_a2 = 31, beta_b2 = 21)

beta_sample_comparison(sample_size = 7000, beta_a1 = 58, beta_b1 = 44, beta_a2 = c(31, 31), beta_b2 = c(21, 30))

beta_sample_comparison(sample_size = 8000, beta_a1 = 58, beta_b1 = 44, beta_a2 = 31, beta_b2 = 21, theta1_smaller_than_theta2 = FALSE)

beta_sample_comparison(sample_size = 8000, beta_a1 = 58, beta_b1 = 44, beta_a2 = 31, beta_b2 = 21, theta1_smaller_than_theta2 = FALSE, plot = TRUE)

beta_sample_comparison(sample_size = 9000, beta_a1 = 50:60, beta_b1 = 40:50, beta_a2 = 31, beta_b2 = 21, plot = TRUE)

beta_sample_comparison(sample_size = 5000, beta_a1 = 58, beta_b1 = 44, beta_a2 = 30:40, beta_b2 = 20:30, plot = TRUE)

gamma_poisson_ppd(sample_size = 5000, gamma_a1 = 2, gamma_b1 = 1, y1 = menchild30bach, gamma_a2 = 2, gamma_b2 = 1, y2 = menchild30nobach, using_MCMC = FALSE)

gamma_poisson_ppd(sample_size = 5000, gamma_a1 = 2, gamma_b1 = 1, y1 = menchild30bach, gamma_a2 = 2, gamma_b2 = 1, y2 = menchild30nobach, using_MCMC = TRUE)

gamma_poisson_ppd(sample_size = 5000, gamma_a1 = 2, gamma_b1 = 1, y1 = menchild30bach, gamma_a2 = 2, gamma_b2 = 1, y2 = menchild30nobach, confidence_interval = c(0.025, 0.975), using_MCMC = FALSE, poisson_fitting_mean = 1.4)

gibbs_effective(sample_size = 30000, x = divorce$V1, y = divorce$V2, tau_beta_square = 16, tau_c_square = 16)

normal_gamma_conjugate_family(sample_size = 10000, mu_0 = 5, sigma_0_sequare = 4, kappa_0 = 1, nu_0 = 2, y.1 = school1, y.2 = school2, y.3 = school3, confidence_interval = c(0.025, 0.975))

posterior_predictive_sample_comparison(sample_size = 5000, gamma_a1 = 237, gamma_b1 = 20, gamma_a2 = 12*(1:100)+113, gamma_b2 = 13+(1:100), tildeya_smaller_than_tildeyb = FALSE, plot = TRUE)

sensitivity_analysis(sample_size = 10000, y_bar.1 = 75.2, standard_deviation.1 = 7.3, y_bar.2 = 77.5, standard_deviation.2 = 8.1, mu_0 = 75, sigma_0_square = 100, n = 16, kappa_0 = c(1,2,4,8,16,32), nu_0 = c(1,2,4,8,16,32))

two_prior_sensitivity(sample_size = 5000, gamma_theta_a = 2, gamma_theta_b = 1, y.1 = menchild30bach, y.2 = menchild30nobach, gamma_gamma_a = 2^(seq(3, 7)), gamma_gamma_b = 2^(seq(3, 7)))
```

For better examples with background and explanations, please refer to the vignettes by running

``` r
browseVignettes("BayesPD")
```