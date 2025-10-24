#' @export
print.ridgereg <- function(x, ...) {
  cat("Call:\n")
  print(x$formula)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}
