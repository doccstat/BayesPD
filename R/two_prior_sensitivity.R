#' Title Analysis the sensitivity of changing the parameters of Gamma distribution, based on the dependency of two Poisson distributions parameters.
#'
#' @param sample_size How many samples to produce.
#' @param gamma_theta_a The first parameter of Gamma distribution of \eqn{\theta}.
#' @param gamma_theta_b The second parameter of Gamma distribution of \eqn{\theta}.
#' @param y.1 The first group of data.
#' @param y.2 The second group of data.
#' @param gamma_gamma_a The first parameter of Gamma distribution of \eqn{\gamma}.
#' @param gamma_gamma_b The second parameter of Gamma distribution of \eqn{\gamma}.
#'
#' @return The expectation of the difference between parameters of Poisson distributions given the data.
#' @export
#'
#' @examples
#' two_prior_sensitivity(sample_size = 5000, gamma_theta_a = 2, gamma_theta_b = 1,
#' y.1 = menchild30bach, y.2 = menchild30nobach,
#' gamma_gamma_a = 2^(seq(3, 7)), gamma_gamma_b = 2^(seq(3, 7)))
two_prior_sensitivity <- function(sample_size, gamma_theta_a, gamma_theta_b, y.1, y.2, gamma_gamma_a, gamma_gamma_b) {
	# Since the two vectors can have different length, we compute each of them.
	n.1 <- length(y.1)
	n.2 <- length(y.2)
	# Create a vector to reduce memory cost.

	plot(gamma_gamma_a, expectation)

	return(expectation)
}
