m <- 3.2
psi <- 1
smallchi <- c(1e-20, 1e-50, 1e-200, .Machine$double.xmin)
regchi <- c(1, 10, 100, 1000)

moments_GIG_raw <- function(m, lambda, chi, psi,
                            omega=sqrt(chi*psi), eta=sqrt(chi/psi)){
  return(besselK(omega,lambda+m,TRUE)/besselK(omega,lambda,TRUE)*eta^m)
}

# DmomentsGIG_Dm <- function(m, lam, chi, psi, h=1e-5)
#   (moments_GIG(m+h, lam, chi, psi) - moments_GIG(m-h, lam, chi, psi)) / 2 / h
DmomentsGIG_Dm <- function(m, lam, chi, psi,
                           omega=sqrt(chi*psi), eta=sqrt(chi/psi), h=1e-5)
  (moments_GIG(m+h, lam, omega=omega, eta=eta) - moments_GIG(m-h, lam, omega=omega, eta=eta)) / 2 / h
