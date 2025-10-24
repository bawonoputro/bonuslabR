#' @export
print.ridgereg_qr <- function(x, ...) {
  cat("Call:\n")
  print(x$formula)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}
