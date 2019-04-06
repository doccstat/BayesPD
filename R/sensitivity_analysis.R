#' Title
#'
#' @param sample_size
#' @param y_bar.1
#' @param standard_deviation.1
#' @param y_bar.2
#' @param standard_deviation.2
#' @param mu_0
#' @param sigma_0_square
#' @param n
#' @param kappa_0
#' @param nu_0
#'
#' @return
#' @export
#'
#' @examples sensitivity_analysis(sample_size = 10000, y_bar.1 = 75.2, standard_deviation.1 = 7.3, y_bar.2 = 77.5, standard_deviation.2 = 8.1, mu_0 = 75, sigma_0_square = 100, n = 16, kappa_0 = c(1,2,4,8,16,32), nu_0 = c(1,2,4,8,16,32)) # sigma_0_square = 4?
sensitivity_analysis <- function(sample_size, y_bar.1, standard_deviation.1, y_bar.2, standard_deviation.2, mu_0, sigma_0_square, n, kappa_0, nu_0) {
	probability <- rep(0,length(kappa_0))

	for(i in 1:length(kappa_0)) {
}
