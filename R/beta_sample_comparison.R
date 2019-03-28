#' Sampling beta distributions and compute the probability of theta1 < theta2
#'
#' @param sample_size how many samples to produce
#' @param beta_a1 first parameter of theta1
#' @param beta_b1 second parameter of theta1
#' @param beta_a2 first parameter of theta2
#' @param beta_b2 second parameter of theta2
#'
#' @return the probability of theta1 < theta2
#' @export
#'
#' @examples beta_sample_comparison(5000, 58, 44, 31, 21)
beta_sample_comparison <- function(sample_size, beta_a1, beta_b1, beta_a2, beta_b2) {
	theta1 <- rbeta(sample_size,beta_a1,beta_b1)
	theta2 <- rbeta(sample_size,beta_a2,beta_b2)
	return(sum(theta1 < theta2) / sample_size)
}
