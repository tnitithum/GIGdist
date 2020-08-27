#' E(X^m logX) of GIG rv rv for small chi
#'
#' Moment with logX factor of GIG random variable for small \code(chi).
#' @inheritParams EX_GIG
#' @return vector with same length as \code{omega}.
#' @keywords expectation
#' @family expectations
#' @examples
#' m <- 1
#' lam <- 0.1
#' chi <- 2e-50
#' psi <- 1
#' log_moments_GIG(m, lam, chi, psi)
#' log_moments_GIG_smallchi(m, lam, chi, psi)
#' @export
log_moments_GIG_smallchi <- function(m, lam, chi, psi, omega=sqrt(chi*psi), eta=sqrt(chi/psi)){
	if(lam+m!=0)
		X <- log(eta) + sign(lam+m)*(digamma(abs(lam+m))-log(omega/2)) else
	# if(lam+m==0)
		X <- log(eta)
	return(moments_GIG_smallchi(m, lam, omega=omega, eta=eta) * X)
}
