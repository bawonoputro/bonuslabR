library(testthat)
library(MASS)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

#for ridgereg
test_that("ridgereg predictions are close to lm.ridge", {
  data(iris)
  formula <- Petal.Length ~ Sepal.Length + Sepal.Width
  lambda <- 1

  mod_custom <- ridgereg(formula, iris, lambda)
  mod_mass <- lm.ridge(formula, iris, lambda = lambda, scale = TRUE)

  X <- model.matrix(formula, iris)
  fitted_mass <- X %*% coef(mod_mass)

  error <- rmse(mod_custom$fitted.values, fitted_mass)

  # Acceptable error threshold (adjust if needed)
  expect_lt(error, 0.5)
})

test_that("ridgereg predictions are close to lm.ridge", {
  data(iris)
  formula <- Petal.Length ~ Sepal.Length + Sepal.Width
  lambda <- 1

  mod_custom <- ridgereg(formula, iris, lambda)
  mod_mass <- lm.ridge(formula, iris, lambda = lambda, scale = TRUE)

  X <- model.matrix(formula, iris)
  fitted_mass <- X %*% coef(mod_mass)

  expect_equal(mod_custom$fitted.values, fitted_mass, tolerance = 1e-1)
})

# For QR

test_that("ridgereg_qr returns similar predictions to ridgereg", {
  data(iris)
  formula <- Petal.Length ~ Sepal.Length + Sepal.Width
  lambda <- 1

  mod_ls <- ridgereg(formula, iris, lambda)
  mod_qr <- ridgereg_qr(formula, iris, lambda)

  error <- rmse(mod_ls$fitted.values, mod_qr$fitted.values)

  # Acceptable error threshold
  expect_lte(error, 0.02)
})

test_that("predict.ridgereg_qr works correctly", {
  data(iris)
  formula <- Petal.Length ~ Sepal.Length + Sepal.Width
  lambda <- 1

  mod_qr <- ridgereg_qr(formula, iris, lambda)
  preds <- predict(mod_qr, iris)

  expect_equal(preds, mod_qr$fitted.values)
})

test_that("coef.ridgereg_qr returns correct coefficients", {
  data(iris)
  formula <- Petal.Length ~ Sepal.Length + Sepal.Width
  lambda <- 1

  mod_qr <- ridgereg_qr(formula, iris, lambda)
  coefs <- coef(mod_qr)

  expect_equal(length(coefs), 3)  # Intercept + 2 predictors
})
