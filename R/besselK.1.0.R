#' Derivative of bessel K wrt nu
#'
#' Derivative of bessel function of the second kind wrt the order variable.
#' @param x Positive numeric.
#' @param nu numeric; The order (maybe fractional and negative) of the corresponding Bessel function.
#' @param expon.scaled.int integer of either 1 or 2.
#'   If 1, the outputs corresponds to K(x;nu).
#'   If 2, the outputs corresponds to exp(x) K(x;nu) to avoid underflow.
#' @param h positive small numeric.
#' @keywords besselK
#' @examples
#' z <- 0.2
#' nu <- 0.9
#' exp(z) * besselK.1.0(z, nu)
#' besselK.1.0(z, nu, 2)
#' @export
besselK.1.0 <- function(x, nu, expon.scaled.int = 1, h = 1e-5)
	(.Internal(besselK(x,nu+h,expon.scaled.int)) - .Internal(besselK(x,nu-h,expon.scaled.int)))/2/h
