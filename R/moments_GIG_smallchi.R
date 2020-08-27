#' m-th moment of GIG for small chi
#'
#' m-th moment of GIG random variable for small chi
#' @inheritParams EX_GIG
#' @param m Numeric. The moment's order.
#' @return vector with same length as \code{omega}.
#' @keywords expectation
#' @family expectations
#' @examples
#' m=-1
#' lam=0.1
#' chi=c(1e-20,1e-50,1e-200)
#' psi=1
#' momentsGIG(m, lam, chi, psi)
#' momentsGIG(m, lam, 0, psi)
#' @export
# EXm_GIG_smallchi <- function(m, lam, chi, psi,
#                              omega=sqrt(chi*psi), eta=sqrt(chi/psi)){
#   eulerConst <- -digamma(1)
#   if(lam<0)
#     return(-2*lam/chi) else
#   if(lam>0 & lam<1)
#     return(2^{1-2*lam} * omega^{2*lam} / chi * gamma(1-lam) / gamma(lam)) else
#   if(lam>1)
#     return(rep(0.5*psi/{lam-1},length(chi))) else
#   if(lam==0)
#     return(- 2/chi/{2*eulerConst+log(chi)+log(psi)-log(4)}) else
#   if(lam==1)
#     return(-0.5*psi*{2*eulerConst+log(chi)+log(psi)-log(4)})
# }




moments_GIG_smallchi <- function(m, lam, chi, psi,
                             omega=sqrt(chi*psi), eta=sqrt(chi/psi)){
  eulergamma <- -digamma(1)
  if(lam!=0){
    if(lam+m!=0)
      X <- gamma(abs(lam+m))/gamma(abs(lam)) else
        # if(lam+m==0)
        X <- -2*{eulergamma + log(omega/2)}/gamma(abs(lam))
  } else
    # if(lam==0)
    if(lam+m!=0)
      X <- -gamma(abs(lam+m))/2/{eulergamma + log(omega/2)} else
        # if(lam+m==0)
        X <- 1
  X <- eta^m * {omega/2}^{abs(lam)-abs(lam+m)} * X
  return(X)
}
