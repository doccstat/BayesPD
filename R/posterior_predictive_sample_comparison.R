#' Poisson model with gamma prior, compare the two parameters
#'
#' @param sample_size How many samples to produce.
#' @param gamma_a1 First parameter of theta1.
#' @param gamma_b1 Second parameter of theta1.
#' @param gamma_a2 First parameter of theta2.
#' @param gamma_b2 Second parameter of theta2.
#' @param tildeya_smaller_than_tildeyb How to make the comparison. If FALSE, compute theta1 > theta2.
#' @param plot Whether to plot the figure showing the tendency of probability change.
#'
#' @return The probability vector or value
#' @export
#'
#' @examples 
#' posterior_predictive_sample_comparison(sample_size = 5000, gamma_a1 = 237, 
#' gamma_b1 = 20, gamma_a2 = 12*(1:100)+113, gamma_b2 = 13+(1:100), 
#' tildeya_smaller_than_tildeyb = FALSE, plot = TRUE)
posterior_predictive_sample_comparison <- function(sample_size = sample_size, gamma_a1 = gamma_a1, gamma_b1 = gamma_b1, gamma_a2 = gamma_a2, gamma_b2 = gamma_b2, tildeya_smaller_than_tildeyb = TRUE, plot = FALSE) {

	# Sample size must be positive integer.
	if(sample_size %% 1 != 0 || sample_size <= 0) {
		stop("Error: sample size should be positive integer!")
	}

	n <- 1

	# if every parameter is a real number, we don't need to set n again.
	if(vector_check(gamma_a1, 1) && vector_check(gamma_b1, 1) && vector_check(gamma_a2, 1) && vector_check(gamma_b2, 1)) {
		if(tildeya_smaller_than_tildeyb) {
			probability <- mean(stats::rpois(sample_size, stats::rgamma(sample_size, gamma_a1, gamma_b1)) < stats::rpois(sample_size, stats::rgamma(sample_size, gamma_a2, gamma_b2)))
		} else {
			probability <- mean(stats::rpois(sample_size, stats::rgamma(sample_size, gamma_a1, gamma_b1)) > stats::rpois(sample_size, stats::rgamma(sample_size, gamma_a2, gamma_b2)))
		}
	}

	if(vector_check(gamma_a1)) {
		if(vector_check(gamma_a2) || vector_check(gamma_b2)) {
			stop("Error: gamma_1 and gamma_2 can not be vectors at the same time!")
		}
		if(gamma_a2 <= 0 || gamma_b2 <= 0) {
			stop("Error: gamma_a2 and gamma_b2 should be positive!")
		}
		
		if(!vector_check(gamma_b1, length(gamma_a1))) {
			stop("Error: the dimensions of gamma_a1 and gamma_b1 aren't the same!")
		}
		# Remove all invalid pairs.
		gamma_b1 <- gamma_b1[gamma_a1 > 0]
		gamma_a1 <- gamma_a1[gamma_a1 > 0]
		gamma_a1 <- gamma_a1[gamma_b1 > 0]
		gamma_b1 <- gamma_b1[gamma_b1 > 0]
		if(!vector_check(gamma_a1) && !vector_check(gamma_b1, 1)) {
			stop("No valid pairs in the vector gamma_1.")
		}

		n <- length(gamma_a1)
		theta2 <- stats::rgamma(sample_size, gamma_a2, gamma_b2)
		tildeyb <- stats::rpois(sample_size, theta2)
		probability <- rep(0, n)
		if(tildeya_smaller_than_tildeyb) {
			for (i in 1:n) {
				theta1 <- stats::rgamma(sample_size, gamma_a1[i], gamma_b1[i])
				tildeya <- stats::rpois(sample_size, theta1)
				probability[i] <- mean(tildeyb < tildeya)
			}
		} else {
			for (i in 1:n) {
				theta1 <- stats::rgamma(sample_size, gamma_a1[i], gamma_b1[i])
				tildeya <- stats::rpois(sample_size, theta1)
				probability[i] <- mean(tildeyb > tildeya)
			}
		}
	}

	if(vector_check(gamma_a2)) {
		if(vector_check(gamma_a1) || vector_check(gamma_b1)) {
			stop("Error: gamma_1 and gamma_2 can not be vectors at the same time!")
		}
		if(gamma_a1 <= 0 || gamma_b1 <= 0) {
			stop("Error: gamma_a1 and gamma_b1 should be positive!")
		}
		
		if(!vector_check(gamma_b2, length(gamma_a2))) {
			stop("Error: the dimensions of gamma_a2 and gamma_b2 aren't the same!")
		}

		# Remove all invalid pairs.
		gamma_b2 <- gamma_b2[gamma_a2 > 0]
		gamma_a2 <- gamma_a2[gamma_a2 > 0]
		gamma_a2 <- gamma_a2[gamma_b2 > 0]
		gamma_b2 <- gamma_b2[gamma_b2 > 0]
		if(!vector_check(gamma_a2) && !vector_check(gamma_b2, 1)) {
			stop("No valid pairs in the vector gamma_2.")
		}
		n <- length(gamma_a2)
		theta1 <- stats::rgamma(sample_size, gamma_a1, gamma_b1)
		tildeya <- stats::rpois(sample_size, theta1)
		probability <- rep(0, n)
		if(tildeya_smaller_than_tildeyb) {
			for (i in 1:n) {
				theta2 <- stats::rgamma(sample_size, gamma_a2[i], gamma_b2[i])
				tildeyb <- stats::rpois(sample_size, theta2)
				probability[i] <- mean(tildeya < tildeyb)
			}
		} else {
			for (i in 1:n) {
				theta2 <- stats::rgamma(sample_size, gamma_a2[i], gamma_b2[i])
				tildeyb <- stats::rpois(sample_size, theta2)
				probability[i] <- mean(tildeya > tildeyb)
			}
		}
	}
	
	if(plot) {
		if(n == 1) {
			writeLines("Warning: Whats the point of plotting one single point?")
		}
		plot(1:n, probability)
	}

	return(probability)

}
