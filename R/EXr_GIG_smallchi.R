#' E[1/X] of GIG for small chi
#'
#' Expectation of reciprocal moment of GIG random variable
#' @param lambda Real shape parameter
#' @param chi non-negative shape parameter. If chi=0, it corresponds to VG
#' @param psi non-negative shape parameter. If psi=0, it corresponds to Student's t
#' @param omega alternate parametrisation
#' @param eta alternate parametrisation
#' @return scalar
#' @keywords expectation
#' @examples
#' lambda=0.1
#' chi=c(1e-20,1e-50,1e-200)
#' psi=1
#' EXr_GIG(lambda, chi, psi)
#' EXr_GIG_smallchi(lambda, chi, psi)
#' @export
EXr_GIG_smallchi <- function(lam, chi, psi,
                             omega=sqrt(chi*psi), eta=sqrt(chi/psi)){
  eulerConst <- -digamma(1)
  if(lam<0)
    # return(-2*lam/chi)
    return(-2*lam/omega/eta)
  if(lam>0 & lam<1)
    # return(2^{1-2*lam} * omega^{2*lam} / chi * gamma(1-lam) / gamma(lam))
    return(2^{1-2*lam} * omega^{2*lam} / omega / eta * gamma(1-lam) / gamma(lam))
  if(lam>1)
    # return(rep(0.5*psi/{lam-1},length(chi)))
    return(0.5*omega/eta/{lam-1})
  if(lam==0)
    # return(- 2/chi/{2*eulerConst+log(chi)+log(psi)-log(4)})
    return(- 2/omega/eta/{2*eulerConst+2*log(omega)-log(4)})
  if(lam==1)
    # return(-0.5*psi*{2*eulerConst+log(chi)+log(psi)-log(4)})
    return(-0.5*omega/eta*{2*eulerConst+2*log(omega)-log(4)})
}
