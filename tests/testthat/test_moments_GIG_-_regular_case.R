context("moments_GIG tests: regular case")

#### load parameters to test ####
if(!exists("regchi")) source("./tests/testthat/helper.R")
chi <- regchi
omega <- sqrt(chi*psi)
eta <- sqrt(chi/psi)

#### lambda < 0 ####
test_that("moments_GIG: lambda < 0", {
	lambda <- -4
	EXm <- moments_GIG(m, lambda, chi, psi)
	EXm_regchi <- moments_GIG_raw(m, lambda, chi, psi)

  expect_is(EXm, "numeric")
  expect_is(EXm_regchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_regchi[k])
  }
})

test_that("moments_GIG: lambda < 0, input omega & eta", {
  lambda <- -4
  EXm <- moments_GIG(m, lambda, omega=omega, eta=eta)
  EXm_regchi <- moments_GIG_raw(m, lambda, omega=omega, eta=eta)

  expect_is(EXm, "numeric")
  expect_is(EXm_regchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_regchi[k])
  }
})

#### lambda == 0 ####
test_that("moments_GIG: lambda == 0", {
  lambda <- 0
  EXm <- moments_GIG(m, lambda, chi, psi)
  EXm_regchi <- moments_GIG_raw(m, lambda, chi, psi)

  expect_is(EXm, "numeric")
  expect_is(EXm_regchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_regchi[k])
  }
})

test_that("moments_GIG: lambda == 0, input omega & eta", {
  lambda <- 0
  EXm <- moments_GIG(m, lambda, omega=omega, eta=eta)
  EXm_regchi <- moments_GIG_raw(m, lambda, omega=omega, eta=eta)

  expect_is(EXm, "numeric")
  expect_is(EXm_regchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_regchi[k])
  }
})

#### lambda in (0,m) ####
test_that("moments_GIG: lambda in (0,m)", {
  lambda <- m/2
  EXm <- moments_GIG(m, lambda, chi, psi)
  EXm_regchi <- moments_GIG_raw(m, lambda, chi, psi)

  expect_is(EXm, "numeric")
  expect_is(EXm_regchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_regchi[k])
  }
})

test_that("moments_GIG: lambda in (0,m), input omega & eta", {
  lambda <- m/2
  EXm <- moments_GIG(m, lambda, omega=omega, eta=eta)
  EXm_regchi <- moments_GIG_raw(m, lambda, omega=omega, eta=eta)

  expect_is(EXm, "numeric")
  expect_is(EXm_regchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_regchi[k])
  }
})

#### lambda == m ####
test_that("moments_GIG: lambda == m", {
  lambda <- m
  EXm <- moments_GIG(m, lambda, chi, psi)
  EXm_regchi <- moments_GIG_raw(m, lambda, chi, psi)

  expect_is(EXm, "numeric")
  expect_is(EXm_regchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_regchi[k])
  }
})

test_that("moments_GIG: lambda == m, input omega & eta", {
  lambda <- m
  EXm <- moments_GIG(m, lambda, omega=omega, eta=eta)
  EXm_regchi <- moments_GIG_raw(m, lambda, omega=omega, eta=eta)

  expect_is(EXm, "numeric")
  expect_is(EXm_regchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_regchi[k])
  }
})

#### lambda > m ####
test_that("moments_GIG: lambda > m", {
  lambda <- 2*m
  EXm <- moments_GIG(m, lambda, chi, psi)
  EXm_regchi <- moments_GIG_raw(m, lambda, chi, psi)

  expect_is(EXm, "numeric")
  expect_is(EXm_regchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_regchi[k])
  }
})

test_that("moments_GIG: lambda > m, input omega & eta", {
  lambda <- 2*m
  EXm <- moments_GIG(m, lambda, omega=omega, eta=eta)
  EXm_regchi <- moments_GIG_raw(m, lambda, omega=omega, eta=eta)

  expect_is(EXm, "numeric")
  expect_is(EXm_regchi, "numeric")
  expect_length(EXm, length(chi))
  expect_length(EXm_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXm[k], EXm_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXm_regchi[k])
  }
})
