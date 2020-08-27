context("EXr_GIG tests: smallchi case")

#### load parameters to test ####
if(!exists("smallchi")) source("./tests/testthat/helper.R")
chi <- smallchi
omega <- sqrt(chi*psi)
eta <- sqrt(chi/psi)

#### lambda < 0 ####
test_that("EXr_GIG: lambda < 0", {
	lambda <- -4
	EXr <- EXr_GIG(lambda, chi, psi)
	EXr_smallchi <- EXr_GIG_smallchi(lambda, chi, psi)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

test_that("EXr_GIG: lambda < 0, input omega & eta", {
  lambda <- -4
  EXr <- EXr_GIG(lambda, omega=omega, eta=eta)
  EXr_smallchi <- EXr_GIG_smallchi(lambda, omega=omega, eta=eta)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})


#### lambda == 0 ####
test_that("EXr_GIG: lambda == 0", {
  lambda <- 0
  EXr <- EXr_GIG(lambda, chi, psi)
  EXr_smallchi <- EXr_GIG_smallchi(lambda, chi, psi)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

test_that("EXr_GIG: lambda == 0", {
  lambda <- 0
  EXr <- EXr_GIG(lambda, omega=omega, eta=eta)
  EXr_smallchi <- EXr_GIG_smallchi(lambda, omega=omega, eta=eta)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})


#### lambda in (0,1) ####
test_that("EXr_GIG: lambda in (0,1)", {
  lambda <- 0.5
  EXr <- EXr_GIG(lambda, chi, psi)
  EXr_smallchi <- EXr_GIG_smallchi(lambda, chi, psi)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

test_that("EXr_GIG: lambda in (0,1)", {
  lambda <- 0.5
  EXr <- EXr_GIG(lambda, omega=omega, eta=eta)
  EXr_smallchi <- EXr_GIG_smallchi(lambda, omega=omega, eta=eta)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

#### lambda == 1 ####
test_that("EXr_GIG: lambda == 1", {
  lambda <- 1
  EXr <- EXr_GIG(lambda, chi, psi)
  EXr_smallchi <- EXr_GIG_smallchi(lambda, chi, psi)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

test_that("EXr_GIG: lambda == 1", {
  lambda <- 1
  EXr <- EXr_GIG(lambda, omega=omega, eta=eta)
  EXr_smallchi <- EXr_GIG_smallchi(lambda, omega=omega, eta=eta)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

#### lambda > 1 ####
test_that("EXr_GIG: lambda > 1", {
  lambda <- 4
  EXr <- EXr_GIG(lambda, chi, psi)
  EXr_smallchi <- EXr_GIG_smallchi(lambda, chi, psi)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

test_that("EXr_GIG: lambda > 1", {
  lambda <- 4
  EXr <- EXr_GIG(lambda, omega=omega, eta=eta)
  EXr_smallchi <- EXr_GIG_smallchi(lambda, omega=omega, eta=eta)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})
