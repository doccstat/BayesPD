
gamma_poisson_ppd <- function(sample_size = sample_size, gamma_a1 = gamma_a1, gamma_b1 = gamma_b1, y1 = y1, gamma_a2 = gamma_a2, gamma_b2 = gamma_b2, y2 = y2, using_MCMC = FALSE, plot = FALSE) {

	if(using_MCMC) {
		theta1 <- rgamma(sample_size, gamma_a1, gamma_b1)
		y_tilde1 <- rpois(sample_size, theta1)
		theta2 <- rgamma(sample_size, gamma_a2, gamma_b2)
		y_tilde2 <- rpois(sample_size, theta2)
	} 
}

