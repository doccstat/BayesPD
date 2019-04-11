#' Title Check if the object is a vector or not
#'
#' @param object Vector to be tested.
#' @param length Expected length.
#'
#' @return True or False. Vector of length 1 is not treated as a vector, thus returning false, unless the specified length is 1.
#' @export
#'
#' @examples 
#' vector_check(object = c(1)) == FALSE
#' vector_check(object = c(1), length = 1) == TRUE
#' vector_check(object = c(1, 1)) == TRUE
#' vector_check(object = c(1, 2), length = 2) == TRUE
#' vector_check(object = c(1, 1), length = 1) == FALSE
vector_check <- function(object, length = NULL) {
	if(is.null(object)) { stop("The variable provided is null.") }
	if(is.null(length) && is.vector(object) && length(object) > 1) { return(TRUE) }
	if(!is.null(length) && is.vector(object) && length(object) == length) { return(TRUE) }
	return(FALSE)
}
