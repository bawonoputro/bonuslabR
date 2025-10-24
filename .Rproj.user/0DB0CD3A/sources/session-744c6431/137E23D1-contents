#' Ridge Regression
#'
#' @param formula A formula object
#' @param data A data frame
#' @param lambda Regularization parameter
#' @return An object of class "ridgereg"
#' @export
#' @importFrom stats model.frame model.response terms model.matrix
#' @examples
#' data(iris)
#' model <- ridgereg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris, lambda = 0.1)
#' print(model)
#' predict(model, iris)
#' coef(model)
ridgereg <- function(formula, data, lambda = 1) {
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  tf <- terms(formula, data = data)
  X <- model.matrix(tf, data = mf)

  # Remove intercept column for regularization
  X_no_intercept <- X[, -1, drop = FALSE]

  # Normalize predictors (center and scale)
  X_scaled <- scale(X_no_intercept)
  y_center <- y - mean(y)

  # Ridge regression coefficients
  XtX <- t(X_scaled) %*% X_scaled
  beta <- solve(XtX + lambda * diag(ncol(X_scaled))) %*% t(X_scaled) %*% y_center

  # Intercept
  intercept <- mean(y) - colMeans(X_no_intercept) %*% beta
  coefficients <- c(intercept, beta)

  # Naming
  coef_names <- colnames(X)
  names(coefficients) <- coef_names

  # Fitted values
  fitted <- X %*% coefficients

  structure(list(
    coefficients = coefficients,
    fitted.values = fitted,
    formula = formula,
    terms = tf,
    lambda = lambda,
    X = X_scaled,
    y = y
  ), class = "ridgereg")
}
