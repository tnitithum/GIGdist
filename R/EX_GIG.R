#' E(X) of GIG
#'
#' Expectation of GIG random variable.
#' @param lam Real shape parameter \eqn{\lambda}.
#' @param chi Non-negative parameter \eqn{\chi}.
#'   Must be positive if \eqn{\lambda \le 0}.
#'   If \eqn{\chi = 0}, it corresponds to the gamma distribution.
#'   Can input a vector for multiple outputs.
#' @param psi Non-negative parameter \eqn{\psi}.
#'   Must be positive if \eqn{\lambda \ge 0}.
#'   If \eqn{\psi = 0}, it corresponds to the inverse-gamma distribution.
#' @param omega Non-negative scale parameter \eqn{\omega} defined as \eqn{sqrt(\chi * \psi)}.
#'   Can input a vector for multiple outputs (if \code{chi} is not supplied).
#' @param eta Non-negative shape parameter \eqn{\eta} defined as \eqn{sqrt(\chi / \psi)}.
#'   Can input a vector for multiple outputs (if \code{chi} is not supplied) with same length as \code{omega}.
# #' @param esp_reg Boundary condition of besselK in the ratio to avoid overflow.
# #'   If \code{besselK > esp_reg}, it uses the asymptotic version of the expectation \code{EX_GIG_smallchi}.
#' @return vector with same length as \code{omega}.
#' @family expectations
#' @details The parameters \eqn{(\lambda,\chi,\psi)} must satisfy the condition:
#' \itemize{
#'   \item \eqn{if \lambda < 0, \chi \ge 0 and \psi > 0},
#'   \item \eqn{if \lambda = 0, \chi > 0 and \psi > 0},
#'   \item \eqn{if \lambda > 0, \chi > and \psi \ge 0}.
#' }
#' @examples
#' m <- 1
#' lam <- 0.1
#' chi <- c(2, 0.2, 2e-20, 2e-200)
#' psi <- 1
#' EX_GIG(lam, chi, psi)
#' moments_GIG(m, lam, chi, psi)
#' @export
EX_GIG <- function(lam, chi, psi,
                   omega = sqrt(chi*psi), eta = sqrt(chi/psi)
                   ){
  # calculate boundary for reg_index
  esp_reg <- 5e-306
  max_lam <- max(abs(c(lam+1,lam)))
  reg_boundary <- 2*(gamma(max_lam)*esp_reg*0.5)^(1/max_lam)
  reg_index <- omega > reg_boundary
  output <- double(length(omega))

  # expectation for reg_indices
  output[reg_index] <- .Internal(besselK(omega[reg_index],lam+1,2)) / .Internal(besselK(omega[reg_index],lam,2)) * eta[reg_index]

  # expectation for small_chi_indices
  output[!reg_index] <- EX_GIG_smallchi(lam, omega=omega[!reg_index], eta=eta[!reg_index])

  return(output)
}
