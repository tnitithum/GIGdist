context("moments_GIG tests: smallchi case")

#### load parameters to test ####
if(!exists("smallchi")) source("./tests/testthat/helper.R")
chi <- smallchi
omega <- sqrt(chi*psi)
eta <- sqrt(chi/psi)

#### lambda < 0 ####
test_that("moments_GIG: lambda < 0", {
	lambda <- -4
	EXm <- moments_GIG(m, lambda, chi, psi)
	EXm_smallchi <- moments_GIG_smallchi(m, lambda, chi, psi)

  expect_is(EXm, "numeric")
  expect_is(EXm_smallchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_smallchi[k])
  }
})

test_that("moments_GIG: lambda < 0, input omega & eta", {
  lambda <- -4
  EXm <- moments_GIG(m, lambda, omega=omega, eta=eta)
  EXm_smallchi <- moments_GIG_smallchi(m, lambda, omega=omega, eta=eta)

  expect_is(EXm, "numeric")
  expect_is(EXm_smallchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_smallchi[k])
  }
})

#### lambda == 0 ####
test_that("moments_GIG: lambda == 0", {
  lambda <- 0
  EXm <- moments_GIG(m, lambda, chi, psi)
  EXm_smallchi <- moments_GIG_smallchi(m, lambda, chi, psi)

  expect_is(EXm, "numeric")
  expect_is(EXm_smallchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_smallchi[k])
  }
})

test_that("moments_GIG: lambda == 0, input omega & eta", {
  lambda <- 0
  EXm <- moments_GIG(m, lambda, omega=omega, eta=eta)
  EXm_smallchi <- moments_GIG_smallchi(m, lambda, omega=omega, eta=eta)

  expect_is(EXm, "numeric")
  expect_is(EXm_smallchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_smallchi[k])
  }
})

#### lambda in (0,m) ####
test_that("moments_GIG: lambda in (0,m)", {
  lambda <- m/2
  EXm <- moments_GIG(m, lambda, chi, psi)
  EXm_smallchi <- moments_GIG_smallchi(m, lambda, chi, psi)

  expect_is(EXm, "numeric")
  expect_is(EXm_smallchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_smallchi[k])
  }
})

test_that("moments_GIG: lambda in (0,m), input omega & eta", {
  lambda <- m/2
  EXm <- moments_GIG(m, lambda, omega=omega, eta=eta)
  EXm_smallchi <- moments_GIG_smallchi(m, lambda, omega=omega, eta=eta)

  expect_is(EXm, "numeric")
  expect_is(EXm_smallchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_smallchi[k])
  }
})

#### lambda == m ####
test_that("moments_GIG: lambda == m", {
  lambda <- m
  EXm <- moments_GIG(m, lambda, chi, psi)
  EXm_smallchi <- moments_GIG_smallchi(m, lambda, chi, psi)

  expect_is(EXm, "numeric")
  expect_is(EXm_smallchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_smallchi[k])
  }
})

test_that("moments_GIG: lambda == m, input omega & eta", {
  lambda <- m
  EXm <- moments_GIG(m, lambda, omega=omega, eta=eta)
  EXm_smallchi <- moments_GIG_smallchi(m, lambda, omega=omega, eta=eta)

  expect_is(EXm, "numeric")
  expect_is(EXm_smallchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_smallchi[k])
  }
})

#### lambda > m ####
test_that("moments_GIG: lambda > m", {
  lambda <- 2*m
  EXm <- moments_GIG(m, lambda, chi, psi)
  EXm_smallchi <- moments_GIG_smallchi(m, lambda, chi, psi)

  expect_is(EXm, "numeric")
  expect_is(EXm_smallchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_smallchi[k])
  }
})

test_that("moments_GIG: lambda > m, input omega & eta", {
  lambda <- 2*m
  EXm <- moments_GIG(m, lambda, omega=omega, eta=eta)
  EXm_smallchi <- moments_GIG_smallchi(m, lambda, omega=omega, eta=eta)

  expect_is(EXm, "numeric")
  expect_is(EXm_smallchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_smallchi[k])
  }
})
