#' Soft-tresholding function
#'
#' @param a scalar to be tresholded
#' @param lambda threshold value, should be non-negative
#'
#' @return Soft-tresholded value of a
#' @export
#'
#' @examples
#' soft(3, 1)
soft <- function(a, lambda) {
	sign(a) * max(abs(a) - lambda, 0)
}
