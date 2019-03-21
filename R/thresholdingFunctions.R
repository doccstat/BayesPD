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


#' Hard-thresholding function
#'
#' @param a scalar to be tresholded
#' @param lambda threshold value, should be non-negative
#'
#' @return Hard-tresholded value of a
#' @export
#'
#' @examples
#' hard(3,1)
hard <- function(a, lambda) {
  if(abs(a) > lambda) {
    a
  } else {
    0
  }
}
