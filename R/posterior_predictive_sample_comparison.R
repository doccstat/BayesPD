#' Poisson model with gamma prior, compare the two variables
#'
#' @param sample_size How many samples to produce.
#' @param gamma_a1 First parameter of theta1.
#' @param gamma_b1 Second parameter of theta1.
#' @param gamma_a2 First parameter of theta2.
#' @param gamma_b2 Second parameter of theta2.
#' @param theta1_smaller_than_theta2 How to make the comparison. If FALSE, compute theta1 > theta2.
#' @param plot Whether to plot the figure showing the tendency of probability change.
#'
#' @return The probability vector or value
#' @export
#'
#' @examples posterior_predictive_sample_comparison(sample_size = 5000, gamma_a1 = 12*(1:100)+113, gamma_b1 = 13+(1:100), gamma_a2 = 237, gamma_b2 = 20, theta1_smaller_than_theta2 = FALSE, plot = TRUE)
posterior_predictive_sample_comparison <- function(sample_size = sample_size, gamma_a1 = gamma_a1, gamma_b1 = gamma_b1, gamma_a2 = gamma_a2, gamma_b2 = gamma_b2, theta1_smaller_than_theta2 = TRUE, plot = FALSE) {

	if(sample_size %% 1 != 0 || sample_size <= 0) {
		stop("Error: sample size should be positive integer!")
	}

	if(is.vector(gamma_a1) && length(gamma_a1) > 1) {
		if(!is.numeric(gamma_a2) || !is.numeric(gamma_b2)) {
			stop("Error: gamma_1 and gamma_2 can not be vectors at the same time!")
		}
		if(gamma_a2 <= 0 || gamma_b2 <= 0) {
			stop("Error: gamma_a2 and gamma_b2 should be positive!")
		}
		n <- length(gamma_a1)
		if(!is.vector(gamma_b1) || length(gamma_b1) != n) {
			stop("Error: the dimensions of gamma_a1 and gamma_b1 aren't the same!")
		}
		theta2 <- rgamma(sample_size, gamma_a2, gamma_b2)
		tildeyb <- rpois(sample_size, theta2)
		probability <- rep(0, n)
		if(theta1_smaller_than_theta2) {
			for (i in 1:n) {
				theta1 <- rgamma(sample_size, gamma_a1[i], gamma_b1[i])
				tildeya <- rpois(sample_size, theta1)
				probability[i] <- mean(tildeyb < tildeya)
			}
		} else {
			for (i in 1:n) {
				theta1 <- rgamma(sample_size, gamma_a1[i], gamma_b1[i])
				tildeya <- rpois(sample_size, theta1)
				probability[i] <- mean(tildeyb > tildeya)
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