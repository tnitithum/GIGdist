context("log_moments_GIG tests: smallchi case")

#### load parameters to test ####
if(!exists("smallchi")) source("./tests/testthat/helper.R")
chi <- smallchi
omega <- sqrt(chi*psi)
eta <- sqrt(chi/psi)

#### lambda < -m ####
test_that("log_moments_GIG: lambda < -m", {
	lambda <- -4
	ElogX <- log_moments_GIG(m, lambda, chi, psi)
	ElogX_smallchi <- log_moments_GIG_smallchi(m, lambda, chi, psi)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_smallchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_smallchi[k]))
  }
})

test_that("log_moments_GIG: lambda < -m, input omega & eta", {
  lambda <- -4
  ElogX <- log_moments_GIG(m, lambda, omega=omega, eta=eta)
  ElogX_smallchi <- log_moments_GIG_smallchi(m, lambda, omega=omega, eta=eta)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_smallchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_smallchi[k]))
  }
})

#### lambda == -m ####
test_that("log_moments_GIG: lambda == -m", {
  lambda <- -m
  ElogX <- log_moments_GIG(m, lambda, chi, psi)
  ElogX_smallchi <- log_moments_GIG_smallchi(m, lambda, chi, psi)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_smallchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_smallchi[k]))
  }
})

test_that("log_moments_GIG: lambda == -m, input omega & eta", {
  lambda <- -m
  ElogX <- log_moments_GIG(m, lambda, omega=omega, eta=eta)
  ElogX_smallchi <- log_moments_GIG_smallchi(m, lambda, omega=omega, eta=eta)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_smallchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_smallchi[k]))
  }
})

#### lambda in (-m,0) ####
test_that("log_moments_GIG: lambda in (-m,0)", {
  lambda <- -m/2
  ElogX <- log_moments_GIG(m, lambda, chi, psi)
  ElogX_smallchi <- log_moments_GIG_smallchi(m, lambda, chi, psi)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_smallchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_smallchi[k]))
  }
})

test_that("log_moments_GIG: lambda in (-m,0), input omega & eta", {
  lambda <- -m/2
  ElogX <- log_moments_GIG(m, lambda, omega=omega, eta=eta)
  ElogX_smallchi <- log_moments_GIG_smallchi(m, lambda, omega=omega, eta=eta)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_smallchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_smallchi[k]))
  }
})

#### lambda == 0 ####
test_that("log_moments_GIG: lambda == 0", {
  lambda <- 0
  ElogX <- log_moments_GIG(m, lambda, chi, psi)
  ElogX_smallchi <- log_moments_GIG_smallchi(m, lambda, chi, psi)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_smallchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_smallchi[k]))
  }
})

test_that("log_moments_GIG: lambda == 0, input omega & eta", {
  lambda <- 0
  ElogX <- log_moments_GIG(m, lambda, omega=omega, eta=eta)
  ElogX_smallchi <- log_moments_GIG_smallchi(m, lambda, omega=omega, eta=eta)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_smallchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_smallchi[k]))
  }
})

#### lambda > 0 ####
test_that("log_moments_GIG: lambda > 0", {
  lambda <- 4
  ElogX <- log_moments_GIG(m, lambda, chi, psi)
  ElogX_smallchi <- log_moments_GIG_smallchi(m, lambda, chi, psi)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_smallchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_smallchi[k]))
  }
})

test_that("log_moments_GIG: lambda > 0, input omega & eta", {
  lambda <- 4
  ElogX <- log_moments_GIG(m, lambda, omega=omega, eta=eta)
  ElogX_smallchi <- log_moments_GIG_smallchi(m, lambda, omega=omega, eta=eta)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_smallchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_smallchi[k]))
  }
})
