#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$fitted.values)
  }

  X_new <- model.matrix(object$terms, newdata)
  as.vector(X_new %*% object$coefficients)
}
