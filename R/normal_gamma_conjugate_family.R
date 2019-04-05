#' Title Univariate normal distribution with normal-gamma conjugate distribution
#'
#' @param sample_size How many samples to produce.
#' @param mu_0 The mean of normal distribution of theta given sigma squared.
#' @param sigma_0_sequare Sample variance.
#' @param kappa_0 The number of prior observations.
#' @param nu_0 The number of samples of prior observations.
#' @param y_1 Data set number 1.
#' @param y_2 Data set number 2.
#' @param y_3 Data set number 3.
#' @param confidence_interval Confidence interval to be calculated.
#'
#' @return The comparison of three data sets.
#' @export
#'
#' @examples normal_gamma_conjugate_family(sample_size = 10000, mu_0 = 5, sigma_0_sequare = 4, kappa_0 = 1, nu_0 = 2, y_1 = c(2.11,9.75,13.88,11.3,8.93,15.66,16.38,4.54,8.86,11.94,12.47,11.11,11.65,14.53,9.61,7.38,3.34,9.06,9.45,5.98,7.44,8.5,1.55,11.45,9.73), confidence_interval = c(0.025, 0.975))
normal_gamma_conjugate_family <- function(sample_size, mu_0, sigma_0_sequare, kappa_0, nu_0, y_1, y_2, y_3, confidence_interval) {
	y_bar.1 <- mean(y_1)
	y_var.1 <- var(y_1)
	y_sd.1 <- sd(y_1)
	length.1 <- length(y_1)
	kappa_n.1 <- kappa_0 + length.1
	mu_n.1 <- (kappa_0 * mu_0 + length.1 * y_bar.1) / kappa_n.1
	nu_n.1 <- nu_0 + length.1
	sigma_n_square.1 <- (1 / nu_n.1) * (nu_0 * sigma_0_sequare + (length.1 - 1) * y_var.1 + ((kappa_0 * length.1) / kappa_n.1) * (y_bar.1 - mu_0)^2)
	# sampling from the posterior distribution
	sigma_square_inverse.1 <- rgamma(sample_size, nu_n.1 / 2, nu_n.1*sigma_n_square.1 / 2)
	sigma_square.1 <- 1 / sigma_square_inverse.1
	theta.1 <- rnorm(sample_size, mu_n.1, sqrt(sigma_square.1 / kappa_n.1))

	theta_bar.1 <- mean(theta.1)
	confidence_interval_theta.1 <- quantile(theta.1, confidence_interval)

	sigma_bar.1 <- mean(sqrt(sigma_square.1))
	confidence_interval_sigma_bar.1 <- quantile(sqrt(sigma_square.1), confidence_interval)
	# MCMC for posterior predictive
	y_tilde.1 <- rnorm(sample_size, theta.1, sqrt(sigma_square.1))


	y_bar.2 <- mean(y_2)
	y_var.2 <- var(y_2)
	y_sd.2 <- sd(y_2)
	length.2 <- length(y_2)
	kappa_n.2 <- kappa_0 + length.2
	mu_n.2 <- (kappa_0 * mu_0 + length.2 * y_bar.2) / kappa_n.2
	nu_n.2 <- nu_0 + length.2
	sigma_n_square.2 <- (1 / nu_n.2) * (nu_0 * sigma_0_sequare + (length.2 - 1) * y_var.2 + ((kappa_0 * length.2) / kappa_n.2) * (y_bar.2 - mu_0)^2)
	# sampling from the posterior distribution
	sigma_square_inverse.2 <- rgamma(sample_size, nu_n.2 / 2, nu_n.2*sigma_n_square.2 / 2)
	sigma_square.2 <- 1 / sigma_square_inverse.2
	theta.2 <- rnorm(sample_size, mu_n.2, sqrt(sigma_square.2 / kappa_n.2))

	theta_bar.2 <- mean(theta.2)
	confidence_interval_theta.2 <- quantile(theta.2, confidence_interval)

	sigma_bar.2 <- mean(sqrt(sigma_square.2))
	confidence_interval_sigma_bar.2 <- quantile(sqrt(sigma_square.2), confidence_interval)
	# MCMC for posterior predictive
	y_tilde.2 <- rnorm(sample_size, theta.2, sqrt(sigma_square.2))


	y_bar.3 <- mean(y_3)
	y_var.3 <- var(y_3)
	y_sd.3 <- sd(y_3)
	length.3 <- length(y_3)
	kappa_n.3 <- kappa_0 + length.3
	mu_n.3 <- (kappa_0 * mu_0 + length.3 * y_bar.3) / kappa_n.3
	nu_n.3 <- nu_0 + length.3
	sigma_n_square.3 <- (1 / nu_n.3) * (nu_0 * sigma_0_sequare + (length.3 - 1) * y_var.3 + ((kappa_0 * length.3) / kappa_n.3) * (y_bar.3 - mu_0)^2)
	# sampling from the posterior distribution
	sigma_square_inverse.3 <- rgamma(sample_size, nu_n.3 / 2, nu_n.3*sigma_n_square.3 / 2)
	sigma_square.3 <- 1 / sigma_square_inverse.3
	theta.3 <- rnorm(sample_size, mu_n.3, sqrt(sigma_square.3 / kappa_n.3))

	theta_bar.3 <- mean(theta.3)
	confidence_interval_theta.3 <- quantile(theta.3, confidence_interval)

	sigma_bar.3 <- mean(sqrt(sigma_square.3))
	confidence_interval_sigma_bar.3 <- quantile(sqrt(sigma_square.3), confidence_interval)
	# MCMC for posterior predictive
	y_tilde.3 <- rnorm(sample_size, theta.3, sqrt(sigma_square.3))


	mean(theta.1 < theta.2 & theta.2 < theta.3)
	mean(theta.1 < theta.3 & theta.3 < theta.2)
	mean(theta.2 < theta.1 & theta.1 < theta.3)
	mean(theta.2 < theta.3 & theta.3 < theta.1)
	mean(theta.3 < theta.1 & theta.1 < theta.2)
	mean(theta.3 < theta.2 & theta.2 < theta.1)

	mean(ypred.1 < ypred.2 & ypred.2 < ypred.3)
	mean(ypred.1 < ypred.3 & ypred.3 < ypred.2)
	mean(ypred.2 < ypred.1 & ypred.1 < ypred.3)
	mean(ypred.2 < ypred.3 & ypred.3 < ypred.1)
	mean(ypred.3 < ypred.1 & ypred.1 < ypred.2)
	mean(ypred.3 < ypred.2 & ypred.2 < ypred.1)


	mean(theta.1 > theta.2 & theta.1 > theta.3)
	mean(ypred.1 > ypred.2 & ypred.1 > ypred.3)
}
