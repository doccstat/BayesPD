#' Sampling beta distributions and compare the two variables
#'
#' @param sample_size How many samples to produce.
#' @param beta_a1 First parameter of theta1.
#' @param beta_b1 Second parameter of theta1.
#' @param beta_a2 First parameter of theta2.
#' @param beta_b2 Second parameter of theta2.
#' @param theta1_smaller_than_theta2 How to make the comparison. If FALSE, compute theta1 > theta2
#' @param plot Whether to plot the figure showing the tendency of probability change
#'
#' @return the probability vector or value
#' @export
#'
#' @examples beta_sample_comparison(5000, 58, 44, 31, 21)
beta_sample_comparison <- function(sample_size = sample_size, beta_a1 = beta_a1, beta_b1 = beta_b1, beta_a2 = beta_a2, beta_b2 = beta_b2, theta1_smaller_than_theta2 = TRUE, plot = FALSE) {

	if(!is.integer(sample_size) || !is.positive(sample_size)) {
		stop("Error: sample size should be positive integer!")
	}

	# TODO: check the positive in the vectors

	if(is.vector(beta_a1)) {
		if(!is.numeric(beta_a2) || !is.numeric(beta_b2)) {
			stop("Error: beta_1 and beta_2 can not be vectors at the same time!")
		}
		if(beta_a2 <= 0 || beta_b2 <= 0) {
			stop("Error: beta_a2 and beta_b2 should be positive!")
		}
		n <- length(beta_a1)
		if(!is.vector(beta_b1) || length(beta_b1) != n) {
			stop("Error: the dimensions of beta_a1 and beta_b1 aren't the same!")
		}
		theta2 <- rbeta(sample_size, beta_a2, beta_b2)
		probability <- rep(0, n)
		for (i in 1:n) {
			probability[i] <- mean(rbeta(sample_size, beta_a1[i], beta_b1[i]) < theta2)
		}
	}

	if(is.vector(beta_a2)) {
		if(!is.numeric(beta_a1) || !is.numeric(beta_b1)) {
			stop("Error: beta_1 and beta_2 can not be vectors at the same time!")
		}
		if(beta_a1 <= 0 || beta_b1 <= 0) {
			stop("Error: beta_a1 and beta_b1 should be positive!")
		}
		n <- length(beta_a2)
		if(!is.vector(beta_b2) || length(beta_b2) != n) {
			stop("Error: the dimensions of beta_a2 and beta_b2 aren't the same!")
		}
		theta1 <- rbeta(sample_size, beta_a1, beta_b1)
		probability <- rep(0, n)
		for (i in 1:n) {
			probability[i] <- mean(theta1 < rbeta(sample_size, beta_a2[i], beta_b2[i]))
		}
	}

	if(plot) {
		if(n == 1) {
			writeLines("Warning: Whats the point of plot one single point?")
		}
		plot(1:n, probability)
	}
	return(probability)
}
