
gamma_poisson_ppd <- function(sample_size = sample_size, gamma_a1 = gamma_a1, gamma_b1 = gamma_b1, y1 = y1, gamma_a2 = gamma_a2, gamma_b2 = gamma_b2, y2 = y2, using_MCMC = FALSE, plot = FALSE) {

	if(using_MCMC) {
		theta1 <- rgamma(sample_size, gamma_a1, gamma_b1)
		y_tilde1 <- rpois(sample_size, theta1)
		theta2 <- rgamma(sample_size, gamma_a2, gamma_b2)
		y_tilde2 <- rpois(sample_size, theta2)
	} else {
		n1 <- length(y1)
		p1 <- (gamma_b1 + n1) / (gamma_b1 + n1 + 1)
		n2 <- length(y2)
		p2 <- (gamma_b2 + n2) / (gamma_b2 + n2 + 1)

		y_tilde1 <- rnbinom(sample_size, sum(y1) + gamma_a1, p1)
		y_tilde2 <- rnbinom(sample_size, sum(y2) + gamma_a2, p2)
	}
}

