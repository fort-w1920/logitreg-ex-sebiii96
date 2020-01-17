#' @import checkmate
#'
NULL

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
#'
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

#' fit_logitreg
#'
#' @description fits the logistic regression
#' @inheritParams neg_loglik
#' @return a list with the entries:
#' - coefficients
#' - fitted
#' - data: a list that contains the response and the design-matrix
#' @export

# note that we do not have to specify the additional parameters of optim
# explicitely as function arguments as they can be passed to the ... argument
# (even if they are specified explicitely in the optim function)
fit_logitreg <- function(response, design, par = NULL,  ...) {
  checked_inputs <- check_inputs_fit_logitreg(response, design, par)
  response <- checked_inputs[["response"]]
  design <- checked_inputs[["design"]]
  par <- checked_inputs[["par"]]

  coefficients <- optim(par = par, fn = neg_loglik, gr = neg_loglik_deriv,
    design = design, response = response, ...)$par

  fitted <- logistic(design %*% coefficients)

  list(
    "coefficients" = coefficients,
    "fitted" = fitted,
    "data" = list("response" = response, "design" = design)
  )
}

#' check_inputs_fit_logitreg
#'
#' @inheritParams neg_logik
#' @description does the input checking for the fit_logitreg function
#' @return a list that contains respons as a
check_inputs_fit_logitreg <- function(response, design, par) {
  assert_numeric(response, min.len = 1, all.missing = FALSE, finite = TRUE)

  assert(
    check_matrix(design, mode = "numeric", min.cols = 1, all.missing = FALSE),
    check_true(NROW(design) == length(response)),
    combine = "and"
  )

  if (is.null(par)) {
    par <- rep(0, times = NCOL(design))
  } else {
    assert_numeric(par, finite = TRUE, any.missing = FALSE, len = NROW(design))
  }

  missing_response <- is.na(response)
  missing_design <- apply(X = design, MARGIN = 1,
    FUN = function(x) sum(is.na(x)) > 0)
  missing <- as.logical(missing_response + missing_design)

  response <- response[!missing]
  design <- design[!missing,, drop = FALSE]
  assert_true(any(dim(design) > 0) && NROW(design) >= NCOL(design))

  list(
    "response" = response,
    "design" = design,
    "par" = par
  )
}
