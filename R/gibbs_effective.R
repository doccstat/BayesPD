#' Title Analysis of the mixing efficiency of Gibbs sampling.
#'
#' @param x The data set need to be analyzed.
#' @param y The indicator vector of the data x.
#' @param sample_size The sample size used in analyzing the mixing of Gibbs sampling.
#' @param tau_beta_square Variance of the normal distribution of beta.
#' @param tau_c_square Variance of the normal distribution of c.
#' @param confidence_interval Confidence interval to be calculated.
#'
#' @return Confidence interval of beta, and the probability of beta being positive given \eqn{\boldsymbol{x}} and \eqn{\boldsymbol{y}}.
#' @export
#'
#' @examples
#' gibbs_effective(sample_size = 30000, x = divorce$V1, y = divorce$V2, 
#' tau_beta_square = 16, tau_c_square = 16)
gibbs_effective <- function(sample_size, x, y, tau_beta_square, tau_c_square, confidence_interval = c(0.025, 0.975)) {
	data_size <- length(y)
	# The initial values to start Gibbs sampling
	beta <- stats::rnorm(1, mean = 0, sd = sqrt(tau_beta_square))
	c <- stats::rnorm(1, mean = 0, sd = sqrt(tau_c_square))
	z <- matrix(0, 1, data_size)
	# Each z can be calculated by the formula given.
	for(j in 1:data_size) {
		# Since y consists only of 1s and 0s, we can combine them together in one equation.
		z[1, j] <- y[j] * MCMCglmm::rtnorm(1, mean = beta*x[j], sd = 1, lower = c) + (1 - y[j]) * MCMCglmm::rtnorm(1, mean = beta*x[j], sd = 1, upper = c)
	}
	
}
