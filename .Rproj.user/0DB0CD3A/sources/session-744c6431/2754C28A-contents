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
  y  <- model.response(mf)
  tf <- terms(formula, data = data)
  X  <- model.matrix(tf, data = mf)

  X_no_intercept <- X[, -1, drop = FALSE]


  x_means <- colMeans(X_no_intercept)
  x_sds   <- apply(X_no_intercept, 2, sd)
  x_sds[x_sds == 0] <- 1

  X_scaled <- scale(
    X_no_intercept,
    center = x_means,
    scale  = x_sds
  )

  y_mean   <- mean(y)
  y_center <- y - y_mean


  XtX <- t(X_scaled) %*% X_scaled
  Xty <- t(X_scaled) %*% y_center

  p <- ncol(X_scaled)
  beta_scaled <- solve(XtX + lambda * diag(p), Xty)

  beta_unscaled <- as.vector(beta_scaled / x_sds)
  names(beta_unscaled) <- colnames(X_no_intercept)

  intercept <- y_mean - sum(beta_unscaled * x_means)

  coefficients <- c(`(Intercept)` = intercept, beta_unscaled)

  fitted_vals <- X %*% coefficients

  structure(
    list(
      coefficients   = coefficients,
      fitted.values  = fitted_vals,
      formula        = formula,
      terms          = tf,
      lambda         = lambda,
      x_means        = x_means,
      x_sds          = x_sds,
      y_mean         = y_mean
    ),
    class = "ridgereg"
  )
}

