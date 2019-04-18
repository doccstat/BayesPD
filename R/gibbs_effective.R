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
	
}
