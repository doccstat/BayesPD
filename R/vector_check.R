
vector_check <- function(object, length = NULL) {
	if(is.null(object)) { stop("The variable provided is null.") }
	if(is.null(length) && is.vector(object) && length(object) > 1) { return(TRUE) }
	if(!is.null(length) && is.vector(object) && length(object) == length) { return(TRUE) }
	return(FALSE)
}
