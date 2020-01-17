#' plogis
#'
#' @param x value at which the logisitc functio is to be evaluated
#' @return density of logistic function at point x
#' @keywords Internal

logistic <- function(x) plogis(x)

#' neg_loglik
#'
#' @descriptin calculates the negative log-likelihood of the logistic model
#' @param coefs parameter vector
#' @param design design matrix for the logistic regression
#' @param response response vector
#' @return the negative log-likelhood of the passed parameter given the passed
#' data
#' @export
neg_loglik <- function(coefs, design, response) {
  probabilities <- logistic(design %*% coefs)
  - sum(response * log(probabilities) + (1 - response) * log(1 - probabilities))
}

#' neg_loglik_deriv
#' @description calculates the derivative of the negative log-likelihood of the
#' logistic model
#' @param coefs parameter vector
#' @param design design matrix for the logistic regression
#' @param response response vector
#' @rdname neg_loglik
#' @keywords Internal
neg_loglik_deriv <- function(coefs, design, response) {
  probabilities <- logistic(design %*% coefs)
  - t(response - probabilities) %*% design
}


