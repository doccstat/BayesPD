#' Sampling beta distributions and compare the two variables
#'
#' @param sample_size How many samples to produce.
#' @param beta_a1 First parameter of theta1.
#' @param beta_b1 Second parameter of theta1.
#' @param beta_a2 First parameter of theta2.
#' @param beta_b2 Second parameter of theta2.
#' @param theta1_smaller_than_theta2 How to make the comparison. If FALSE, compute theta1 > theta2.
#' @param plot Whether to plot the figure showing the tendency of probability change.
#'
#' @return The probability vector or value. If the variables supplied consists of non-positive numbers, the program will ignore that pair of alpha and beta at the same time.
#' @export
#'
#' @examples 
#' beta_sample_comparison(sample_size = 5000, beta_a1 = 58, beta_b1 = 44, 
#' beta_a2 = 31, beta_b2 = 21)
#'
#' beta_sample_comparison(sample_size = 6000, beta_a1 = c(58, 58), beta_b1 = c(44, 50), 
#' beta_a2 = 31, beta_b2 = 21)
#'
#' beta_sample_comparison(sample_size = 7000, beta_a1 = 58, beta_b1 = 44, 
#' beta_a2 = c(31, 31), beta_b2 = c(21, 30))
#'
#' beta_sample_comparison(sample_size = 8000, beta_a1 = 58, beta_b1 = 44, 
#' beta_a2 = 31, beta_b2 = 21, theta1_smaller_than_theta2 = FALSE)
#'
#' beta_sample_comparison(sample_size = 8000, beta_a1 = 58, beta_b1 = 44, 
#' beta_a2 = 31, beta_b2 = 21, theta1_smaller_than_theta2 = FALSE, plot = TRUE)
#'
#' beta_sample_comparison(sample_size = 9000, beta_a1 = 50:60, beta_b1 = 40:50, 
#' beta_a2 = 31, beta_b2 = 21, plot = TRUE)
#'
#' beta_sample_comparison(sample_size = 5000, beta_a1 = 58, beta_b1 = 44, 
#' beta_a2 = 30:40, beta_b2 = 20:30, plot = TRUE)
beta_sample_comparison <- function(sample_size, beta_a1, beta_b1, beta_a2, beta_b2, theta1_smaller_than_theta2 = TRUE, plot = FALSE) {

	if(sample_size %% 1 != 0 || sample_size <= 0) {
		stop("Error: sample size should be positive integer!")
	}

	n <- 1

	if(vector_check(beta_a1, 1) && vector_check(beta_b1, 1) && vector_check(beta_a2, 1) && vector_check(beta_b2, 1)) {
		if(theta1_smaller_than_theta2) {
			probability <- mean(stats::rbeta(sample_size, beta_a1, beta_b1) < stats::rbeta(sample_size, beta_a2, beta_b2))
		} else {
			probability <- mean(stats::rbeta(sample_size, beta_a1, beta_b1) > stats::rbeta(sample_size, beta_a2, beta_b2))
		}
	}

	if(vector_check(beta_a1)) {
		if(vector_check(beta_a2) || vector_check(beta_b2)) {
			stop("Error: beta_1 and beta_2 can not be vectors at the same time!")
		}
		if(beta_a2 <= 0 || beta_b2 <= 0) {
			stop("Error: beta_a2 and beta_b2 should be positive!")
		}
		
		if(!vector_check(beta_b1, length(beta_a1))) {
			stop("Error: the dimensions of beta_a1 and beta_b1 aren't the same!")
		}
		beta_b1 <- beta_b1[beta_a1 > 0]
		beta_a1 <- beta_a1[beta_a1 > 0]
		beta_a1 <- beta_a1[beta_b1 > 0]
		beta_b1 <- beta_b1[beta_b1 > 0]
		if(!vector_check(beta_a1) && !vector_check(beta_b1, 1)) {
			stop("No valid pairs in the vector beta_1.")
		}

		n <- length(beta_a1)
		theta2 <- stats::rbeta(sample_size, beta_a2, beta_b2)
		probability <- rep(0, n)
		if(theta1_smaller_than_theta2) {
			for (i in 1:n) {
				probability[i] <- mean(stats::rbeta(sample_size, beta_a1[i], beta_b1[i]) < theta2)
			}
		} else {
			for (i in 1:n) {
				probability[i] <- mean(stats::rbeta(sample_size, beta_a1[i], beta_b1[i]) > theta2)
			}
		}
	}

	if(vector_check(beta_a2)) {
		if(vector_check(beta_a1) || vector_check(beta_b1)) {
			stop("Error: beta_1 and beta_2 can not be vectors at the same time!")
		}
		if(beta_a1 <= 0 || beta_b1 <= 0) {
			stop("Error: beta_a1 and beta_b1 should be positive!")
		}
		if(!vector_check(beta_b2, length(beta_a2))) {
			stop("Error: the dimensions of beta_a2 and beta_b2 aren't the same!")
		}
		beta_b2 <- beta_b2[beta_a2 > 0]
		beta_a2 <- beta_a2[beta_a2 > 0]
		beta_a2 <- beta_a2[beta_b2 > 0]
		beta_b2 <- beta_b2[beta_b2 > 0]
		if(!vector_check(beta_a2) && !vector_check(beta_b2, 1)) {
			stop("No valid pairs in the vector beta_2.")
		}

		n <- length(beta_a2)
		theta1 <- stats::rbeta(sample_size, beta_a1, beta_b1)
		probability <- rep(0, n)
		if(theta1_smaller_than_theta2) {
			for (i in 1:n) {
				probability[i] <- mean(theta1 < stats::rbeta(sample_size, beta_a2[i], beta_b2[i]))
			}
		} else {
			for (i in 1:n) {
				probability[i] <- mean(theta1 > stats::rbeta(sample_size, beta_a2[i], beta_b2[i]))
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
