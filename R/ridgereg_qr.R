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

  qrX <- qr(X_scaled)
  Q   <- qr.Q(qrX)
  R   <- qr.R(qrX)

  XtX <- t(R) %*% R
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
    class = "ridgereg_qr"
  )
}

