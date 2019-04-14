#' Title Analyze the effect of prior data on the model
#'
#' @param sample_size How many samples to produce.
#' @param y_bar.1 Mean value of the first group of data.
#' @param standard_deviation.1 Standard deviation of the first group of data.
#' @param y_bar.2 Mean value of the second group of data.
#' @param standard_deviation.2 Standard deviation of the second group of data.
#' @param mu_0 Parameter.
#' @param sigma_0_square Parameter.
#' @param n Parameter.
#' @param kappa_0 Parameter.
#' @param nu_0 Parameter.
#' @param theta.1_smaller_than_theta.2 How to make the comparison. If FALSE, compute theta1 > theta2.
#' @param plot Whether to plot the figure showing the tendency of probability change.
#'
#' @return The sensitivity analysis.
#' @export
#'
#' @examples sensitivity_analysis(sample_size = 10000, y_bar.1 = 75.2, standard_deviation.1 = 7.3, y_bar.2 = 77.5, standard_deviation.2 = 8.1, mu_0 = 75, sigma_0_square = 100, n = 16, kappa_0 = c(1,2,4,8,16,32), nu_0 = c(1,2,4,8,16,32)) # sigma_0_square = 4?
sensitivity_analysis <- function(sample_size, y_bar.1, standard_deviation.1, y_bar.2, standard_deviation.2, mu_0, sigma_0_square, n, kappa_0, nu_0, theta.1_smaller_than_theta.2 = TRUE, plot = TRUE) {

	if(sample_size %% 1 != 0 || sample_size <= 0) {
		stop("Error: sample size should be positive integer!")
	}
	if(!vector_check(y_bar.1, 1) || !vector_check(standard_deviation.1, 1) || !vector_check(y_bar.2, 1) || !vector_check(standard_deviation.2, 1) || !vector_check(mu_0, 1) || !vector_check(sigma_0_square, 1) || !vector_check(n, 1)) {
		stop("Error: only single numerical values allowed for the parameters: y_bar.1, standard_deviation.1, y_bar.2, standard_deviation.2, mu_0, sigma_0_square.")
	}
	if(length(kappa_0) != length(nu_0)) {
		stop("The length of vectors are not the same for kappa_0 and nu_0!")
	}

	probability <- rep(0,length(kappa_0))

	for(i in 1:length(kappa_0)) {
		# Compute posterior parameter kappa_n
		kappa_n <- kappa_0[i] + n
		mu_n.1 <- (kappa_0[i] * mu_0 + n * y_bar.1) / kappa_n
		nu_n <- nu_0[i] + n
		sigma_n_square.1 <- (1 / nu_n) * (nu_0[i] * sigma_0_square + (n - 1) * standard_deviation.1^2 + ((kappa_0[i] * n) / kappa_n) * (y_bar.1 - mu_0)^2)
		# MCMC for posterior
		sigma_square_inverse.1 <- stats::rgamma(sample_size, nu_n / 2, nu_n * sigma_n_square.1 / 2)
		sigma_square.1 <-  1 / sigma_square_inverse.1
		theta.1 <- stats::rnorm(sample_size, mu_n.1, sqrt(sigma_square.1 / kappa_n))

		# Compute posterior parameter mu_n
		mu_n.2 <- (kappa_0[i] * mu_0 + n * y_bar.2) / kappa_n
		nu_n <- nu_0[i] + n
		sigma_n_square.2 <- (1 / nu_n) * (nu_0[i] * sigma_0_square + (n - 1) * standard_deviation.2^2 + ((kappa_0[i] * n) / kappa_n) * (y_bar.2 - mu_0)^2)
		# MCMC for posterior
		sigma_square_inverse.2 <- stats::rgamma(sample_size, nu_n / 2, nu_n * sigma_n_square.2 / 2)
		sigma_square.2 <-  1 / sigma_square_inverse.2
		theta.2 <- stats::rnorm(sample_size, mu_n.2, sqrt(sigma_square.2 / kappa_n))

		if(theta.1_smaller_than_theta.2) {
			probability[i] <- mean(theta.1 < theta.2)
		} else {
			probability[i] <- mean(theta.1 > theta.2)
		}
	}

	if(plot) {
		plot(kappa_0, probability, main = "Sensitivity analysis", xlab = "kappa_0 = nu_0")
	}
	
}
