#' E(X) of GIG random variable (for small chi)
#'
#' Approximation of the Expected value of GIG random variable for small chi.
#' @inheritParams EX_GIG
#' @return vector with same length as \code{omega}.
#' @keywords expectation
#' @family expectations
#' @examples
#' EX_GIG(0.2, 1e-200, 1)
#' EX_GIG_smallchi(0.2, 1e-200, 1)
#' @export
EX_GIG_smallchi <- function(lam, chi, psi,
                            omega=sqrt(chi*psi), eta=sqrt(chi/psi)){
  eulerConst <- -digamma(1)
  if(lam < -1)
    # return(-chi/{2+2*lam}) else
    return(-omega*eta/{2+2*lam}) else
  if(lam > -1 & lam<0)
    # return(2^{1+2*lam} * omega^{-2*lam} / psi * gamma(1+lam) / gamma(-lam)) else
    return(2^{1+2*lam} * omega^{-2*lam - 1} * eta * gamma(1+lam) / gamma(-lam)) else
  if(lam>0)
    # return(rep(2*lam/psi,length(chi))) else
    return(2*lam/omega*eta) else
  if(lam==0)
    # return(- 2/psi/{2*eulerConst+log(chi)+log(psi)-log(4)}) else
    return(- 2/omega*eta/{2*eulerConst+2*log(omega)-log(4)}) else
  if(lam == -1)
    # return(-0.5*chi*{2*eulerConst+log(chi)+log(psi)-log(4)})
    return(-0.5*omega*eta*{2*eulerConst+2*log(omega)-log(4)})
}
