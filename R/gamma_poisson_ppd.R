#' Title Poisson model of parameter theta, given gamma prior and the data. Sampling the posterior predictive distribution.
#'
#' @param sample_size How many samples to produce.
#' @param gamma_a1 First parameter of theta1.
#' @param gamma_b1 Second parameter of theta1.
#' @param y1 Given data of the Possion distribution with gamma prior for the parameter theta1.
#' @param gamma_a2 First parameter of theta2.
#' @param gamma_b2 Second parameter of theta2.
#' @param y2 Given data of the Possion distribution with gamma prior for the parameter theta2.
#' @param confidence_interval Confidence interval to be calculated.
#' @param using_MCMC Whether to use MCMC to sampling the posterior predictive distribution.
#'
#' @return Sampling of the posterior predictive distribution.
#' @export
#'
#' @examples gamma_poisson_ppd(sample_size = 5000, gamma_a1 = 2, gamma_b1 = 1, y1 = c(1,0,0,1,2,2,1,5,2,0,0,0,0,0,0,1,1,1,0,0,0,1,1,2,1,3,2,0,0,3,0,0,0,2,1,0,2,1,0,0,1,3,0,1,1,0,2,0,0,2,2,1,3,0,0,0,1,1), gamma_a2 = 2, gamma_b2 = 1, y2 = c(2,2,1,1,2,2,1,2,1,0,2,1,1,2,0,2,2,0,2,1,0,0,3,6,1,6,4,0,3,2,0,1,0,0,0,3,0,0,0,0,0,1,0,4,2,1,0,0,1,0,3,2,5,0,1,1,2,1,2,1,2,0,0,0,2,1,0,2,0,2,4,1,1,1,2,0,1,1,1,1,0,2,3,2,0,2,1,3,1,3,2,2,3,2,0,0,0,1,0,0,0,1,2,0,3,3,0,1,2,2,2,0,6,0,0,0,2,0,1,1,1,3,3,2,1,1,0,1,0,0,2,0,2,0,1,0,2,0,0,2,2,4,1,2,3,2,0,0,0,1,0,0,1,5,2,1,3,2,0,2,1,1,3,0,5,0,0,2,4,3,4,0,0,0,0,0,0,2,2,0,0,2,0,0,1,1,0,2,1,3,3,2,2,0,0,2,3,2,4,3,3,4,0,3,0,1,0,1,2,3,4,1,2,6,2,1,2,2), using_MCMC = FALSE)
#' gamma_poisson_ppd(sample_size = 5000, gamma_a1 = 2, gamma_b1 = 1, y1 = c(1,0,0,1,2,2,1,5,2,0,0,0,0,0,0,1,1,1,0,0,0,1,1,2,1,3,2,0,0,3,0,0,0,2,1,0,2,1,0,0,1,3,0,1,1,0,2,0,0,2,2,1,3,0,0,0,1,1), gamma_a2 = 2, gamma_b2 = 1, y2 = c(2,2,1,1,2,2,1,2,1,0,2,1,1,2,0,2,2,0,2,1,0,0,3,6,1,6,4,0,3,2,0,1,0,0,0,3,0,0,0,0,0,1,0,4,2,1,0,0,1,0,3,2,5,0,1,1,2,1,2,1,2,0,0,0,2,1,0,2,0,2,4,1,1,1,2,0,1,1,1,1,0,2,3,2,0,2,1,3,1,3,2,2,3,2,0,0,0,1,0,0,0,1,2,0,3,3,0,1,2,2,2,0,6,0,0,0,2,0,1,1,1,3,3,2,1,1,0,1,0,0,2,0,2,0,1,0,2,0,0,2,2,4,1,2,3,2,0,0,0,1,0,0,1,5,2,1,3,2,0,2,1,1,3,0,5,0,0,2,4,3,4,0,0,0,0,0,0,2,2,0,0,2,0,0,1,1,0,2,1,3,3,2,2,0,0,2,3,2,4,3,3,4,0,3,0,1,0,1,2,3,4,1,2,6,2,1,2,2), using_MCMC = TRUE)
#' gamma_poisson_ppd(sample_size = 5000, gamma_a1 = 2, gamma_b1 = 1, y1 = c(1,0,0,1,2,2,1,5,2,0,0,0,0,0,0,1,1,1,0,0,0,1,1,2,1,3,2,0,0,3,0,0,0,2,1,0,2,1,0,0,1,3,0,1,1,0,2,0,0,2,2,1,3,0,0,0,1,1), gamma_a2 = 2, gamma_b2 = 1, y2 = c(2,2,1,1,2,2,1,2,1,0,2,1,1,2,0,2,2,0,2,1,0,0,3,6,1,6,4,0,3,2,0,1,0,0,0,3,0,0,0,0,0,1,0,4,2,1,0,0,1,0,3,2,5,0,1,1,2,1,2,1,2,0,0,0,2,1,0,2,0,2,4,1,1,1,2,0,1,1,1,1,0,2,3,2,0,2,1,3,1,3,2,2,3,2,0,0,0,1,0,0,0,1,2,0,3,3,0,1,2,2,2,0,6,0,0,0,2,0,1,1,1,3,3,2,1,1,0,1,0,0,2,0,2,0,1,0,2,0,0,2,2,4,1,2,3,2,0,0,0,1,0,0,1,5,2,1,3,2,0,2,1,1,3,0,5,0,0,2,4,3,4,0,0,0,0,0,0,2,2,0,0,2,0,0,1,1,0,2,1,3,3,2,2,0,0,2,3,2,4,3,3,4,0,3,0,1,0,1,2,3,4,1,2,6,2,1,2,2), confidence_interval = c(0.025, 0.975), poisson_fitting_mean = 1.4)
gamma_poisson_ppd <- function(sample_size, gamma_a1, gamma_b1, y1, gamma_a2, gamma_b2, y2, confidence_interval = NULL, using_MCMC = FALSE, poisson_fitting_mean = NULL) {

	if(sample_size %% 1 != 0 || sample_size <= 0) {
		stop("Error: sample size should be positive integer!")
	}
	if(!vector_check(gamma_a1, 1) || !vector_check(gamma_b1, 1) || !vector_check(gamma_a2, 1) || !vector_check(gamma_b2, 1)) {
		stop("Error: only single numerical values allowed for the parameters of gamma_1 and gamma_2.")
	}
	if(!vector_check(y1) && !vector_check(y1, 1)) {
		stop("Error: y1 should be a vector!")
	}
	if(!vector_check(y2) && !vector_check(y2, 1)) {
		stop("Error: y2 should be a vector!")
	}
	if(using_MCMC) {
		# Sampling from the prior distribution.
		theta1 <- rgamma(sample_size, gamma_a1, gamma_b1)
		# MCMC of the posterior predictive of Poisson distribution
		y_tilde1 <- rpois(sample_size, theta1)
		# Sampling from the prior distribution.
		theta2 <- rgamma(sample_size, gamma_a2, gamma_b2)
		# MCMC of the posterior predictive of Poisson distribution
		y_tilde2 <- rpois(sample_size, theta2)
	} else {
		n1 <- length(y1)
		# Since the posterior predictive distribution of a Poisson model with Gamma prior is Negative Binomial distribution, only need to compute the two parameters of the Negative Binomial distribution.
		
		p1 <- (gamma_b1 + n1) / (gamma_b1 + n1 + 1)
		n2 <- length(y2)
		p2 <- (gamma_b2 + n2) / (gamma_b2 + n2 + 1)

		# Sampling from the Negative Binomial posterior predictive distribution.
		y_tilde1 <- rnbinom(sample_size, sum(y1) + gamma_a1, p1)
		y_tilde2 <- rnbinom(sample_size, sum(y2) + gamma_a2, p2)
	}

	plot(table(y_tilde1), type = "h", lwd = 1, main = "y_tilde1")
	plot(table(y_tilde2), type = "h", lwd = 1, main = "y_tilde2")

	# Parameter theta in a Poisson model given Gamma prior is again Gamma distribution.
	theta1_posterior <- rgamma(sample_size, sum(y1) + gamma_a1, n1 + gamma_b1)
	theta2_posterior <- rgamma(sample_size, sum(y2) + gamma_a2, n2 + gamma_b2)

	quantile(theta2_posterior - theta1_posterior, confidence_interval)
	quantile(y_tilde2 - y_tilde1, confidence_interval)

	plot(0:max(y1)+0.2, dpois(0:max(y1), poisson_fitting_mean), type="h", col="red")
	points(table(y1)/n2)

	plot(0:max(y2)+0.2, dpois(0:max(y2), poisson_fitting_mean), type="h", col="red")
	points(table(y2)/n2)

	zeroes = rep(NA, sample_size)
	ones = rep(NA, sample_size)
	for(i in 1:sample_size) {
		y = rpois(218, theta2_posterior[i])
		zeroes[i] = sum(y==0)
		ones[i] = sum(y==1)
	}
	plot(zeroes, ones, xlim=c(min(zeroes),max(zeroes)), ylim=c(min(ones),max(ones)), pch='.')
	points(sum(y2==0), sum(y2==1), col="red")
}

