#' Predict method for ridgereg objects
#'
#' Generate predictions from a fitted \code{ridgereg} model.
#'
#'
#' @param object A \code{ridgereg} model object returned by \code{ridgereg()}.
#' @param newdata Optional data frame with new predictor values. If omitted,
#'   the in-sample fitted values are returned.
#' @param ... Unused; included for S3 method compatibility.
#'
#' @return A numeric vector of predictions.
#'
#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$fitted.values)
  }

  X_new <- stats::model.matrix(object$formula, newdata)

  as.vector(X_new %*% object$coefficients)
}
