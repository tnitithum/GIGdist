#' E(X^m logX) of GIG rv
#'
#' Moment with logX factor of GIG random variable.
#' @inheritParams EX_GIG
#' @param m Numeric. The moment's order.
#' @return vector with same length as \code{omega}.
#' @keywords expectation
#' @family expectations
#' @examples
#' m <- 1
#' lam <- 0.1
#' chi <- c(2, 0.2, 2e-20, 2e-200)
#' psi <- 1
#' log_moments_GIG(m, lam, chi, psi)
#' @export
log_moments_GIG <- function(m, lam, chi, psi,
                            omega=sqrt(chi*psi), eta=sqrt(chi/psi)){
  ### calculate boundary for reg_index
  esp_reg <- 5e-306
  max_lam <- max(abs(c(lam+m,lam)))
  reg_boundary <- 2*{gamma(max_lam)*esp_reg*0.5}^{1/max_lam}
  reg_index <- omega > reg_boundary
  output <- double(length(omega))

  ### expectation for reg_indices
  output[reg_index] <- (log(eta[reg_index])*.Internal(besselK(omega[reg_index],lam+m,2)) +
                          besselK.1.0(omega[reg_index],lam+m,2)) / .Internal(besselK(omega[reg_index],lam,2)) * eta[reg_index]^m

  ### expectation for small_chi_indices
  output[!reg_index] <- log_moments_GIG_smallchi(m, lam, omega=omega[!reg_index], eta=eta[!reg_index])
  return(output)
}
