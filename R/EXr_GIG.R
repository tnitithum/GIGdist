#' E(1/X) of GIG random variable
#'
#' Expectation of reciprocal moment of GIG random variable.
#' @inheritParams EX_GIG
#' @return vector with same length as \code{omega}.
#' @keywords expectation
#' @family expectations
#' @examples
#' m <- -1
#' lambda <- 0.1
#' chi <- c(2, 0.2, 2e-20, 2e-200)
#' psi <- 1
#' EXr_GIG(lambda, chi, psi)
#' moments_GIG(m, lam, chi, psi)
#' @export
EXr_GIG <- function(lam, chi, psi,
                    omega=sqrt(chi*psi), eta=sqrt(chi/psi)){
  # calculate boundary for reg_index
  esp_reg <- 5e-306
  max_lam <- max(abs(c(lam-1,lam)))
  reg_boundary <- 2*(gamma(max_lam)*esp_reg*0.5)^(1/max_lam)
  reg_index <- omega > reg_boundary
  output <- double(length(omega))

  # expectation for reg_indices
  output[reg_index] <- .Internal(besselK(omega[reg_index],lam-1,2)) / .Internal(besselK(omega[reg_index],lam,2)) / eta[reg_index]

  # expectation for small_chi_indices
  output[!reg_index] <- EXr_GIG_smallchi(lam, omega=omega[!reg_index], eta=eta[!reg_index])

  return(output)
}
