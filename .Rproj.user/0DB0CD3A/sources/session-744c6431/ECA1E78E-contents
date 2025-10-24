#' @export
predict.ridgereg_qr <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$fitted.values)
  }

  X_new <- model.matrix(object$terms, newdata)
  as.matrix(X_new %*% object$coefficients)
}
