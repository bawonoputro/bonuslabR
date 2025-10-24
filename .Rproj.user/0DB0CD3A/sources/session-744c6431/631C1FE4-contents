#' Ridge Regression using QR decomposition
#'
#' @param formula A formula object
#' @param data A data frame
#' @param lambda Regularization parameter
#' @return An object of class "ridgereg_qr"
#' @export
#' @importFrom stats model.frame model.response terms model.matrix
#' @examples
#' data(iris)
#' model <- ridgereg_qr(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris, lambda = 0.1)
#' print(model)
#' predict(model, iris)
#' coef(model)
ridgereg_qr <- function(formula, data, lambda = 1) {
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  tf <- terms(formula, data = data)
  X <- model.matrix(tf, data = mf)

  # Remove intercept for regularization
  X_no_intercept <- X[, -1, drop = FALSE]

  # Normalize predictors
  X_scaled <- scale(X_no_intercept)
  y_center <- y - mean(y)

  # QR decomposition
  qrX <- qr(X_scaled)
  Q <- qr.Q(qrX)
  R <- qr.R(qrX)

  # Ridge solution using QR
  p <- ncol(X_scaled)
  ridge_matrix <- R %*% t(R) + lambda * diag(p)
  beta <- solve(ridge_matrix, t(X_scaled) %*% y_center)

  # Intercept
  intercept <- mean(y) - colMeans(X_no_intercept) %*% beta
  coefficients <- c(intercept, beta)

  coef_names <- colnames(X)
  names(coefficients) <- coef_names

  fitted <- X %*% coefficients
  structure(list(
    coefficients = coefficients,
    fitted.values = fitted,
    formula = formula,
    terms = tf,
    lambda = lambda,
    X = X_scaled,
    y = y
  ), class = "ridgereg_qr")
}
