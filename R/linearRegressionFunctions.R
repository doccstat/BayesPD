calculateB <- function(X, Y) {
	solve(crossprod(X), crossprod(X, Y))
}

#' Title
#'
#' @param xtrain
#' @param ytrain
#' @param xtest
#'
#' @return
#' @export
#'
#' @examples
predictY <- function(xtrain, ytrain, xtest) {
	Bhat <- calculateB(xtrain, ytrain)
	xtest %*% Bhat
}


#' Title
#'
#' @param xtrain
#' @param ytrain
#' @param xtest
#'
#' @return
#' @export
#'
#' @examples
lasso <- function(xtrain, ytrain, xtest) {
  out <- glmnet(xtrain, ytrain)
  return(out)
}
