context("EX_GIG tests: smallchi case")

#### load parameters to test ####
if(!exists("smallchi")) source("./tests/testthat/helper.R")
chi <- smallchi
omega <- sqrt(chi*psi)
eta <- sqrt(chi/psi)

#### lambda < -1 ####
test_that("EX_GIG: lambda < -1", {
	lambda <- -4
	EXr <- EX_GIG(lambda, chi, psi)
	EXr_smallchi <- EX_GIG_smallchi(lambda, chi, psi)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

test_that("EX_GIG: lambda < -1, input omega & eta", {
  lambda <- -4
  EXr <- EX_GIG(lambda, omega=omega, eta=eta)
  EXr_smallchi <- EX_GIG_smallchi(lambda, omega=omega, eta=eta)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})


#### lambda == -1 ####
test_that("EX_GIG: lambda == -1", {
  lambda <- -1
  EXr <- EX_GIG(lambda, chi, psi)
  EXr_smallchi <- EX_GIG_smallchi(lambda, chi, psi)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

test_that("EX_GIG: lambda == -1, input omega & eta", {
  lambda <- -1
  EXr <- EX_GIG(lambda, omega=omega, eta=eta)
  EXr_smallchi <- EX_GIG_smallchi(lambda, omega=omega, eta=eta)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})



#### lambda in (-1,0) ####
test_that("EX_GIG: lambda in (-1,0)", {
  lambda <- -0.5
  EXr <- EX_GIG(lambda, chi, psi)
  EXr_smallchi <- EX_GIG_smallchi(lambda, chi, psi)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

test_that("EX_GIG: lambda in (-1,0), input omega & psi", {
  lambda <- -0.5
  EXr <- EX_GIG(lambda, omega=omega, eta=eta)
  EXr_smallchi <- EX_GIG_smallchi(lambda, omega=omega, eta=eta)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})


#### lambda == 0 ####
test_that("EX_GIG: lambda == 0", {
  lambda <- 0
  EXr <- EX_GIG(lambda, chi, psi)
  EXr_smallchi <- EX_GIG_smallchi(lambda, chi, psi)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

test_that("EX_GIG: lambda == 0, input omega & eta", {
  lambda <- 0
  EXr <- EX_GIG(lambda, omega=omega, eta=eta)
  EXr_smallchi <- EX_GIG_smallchi(lambda, omega=omega, eta=eta)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})


#### lambda > 0 ####
test_that("EX_GIG: lambda > 0", {
  lambda <- 4
  EXr <- EX_GIG(lambda, chi, psi)
  EXr_smallchi <- EX_GIG_smallchi(lambda, chi, psi)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

test_that("EX_GIG: lambda > 0, input omega & eta", {
  lambda <- 4
  EXr <- EX_GIG(lambda, omega=omega, eta=eta)
  EXr_smallchi <- EX_GIG_smallchi(lambda, omega=omega, eta=eta)

  expect_is(EXr, "numeric")
  expect_is(EXr_smallchi, "numeric")
  expect_length(EXr, length(chi))
  expect_length(EXr_smallchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EXr[k], EXr_smallchi[k], tolerance=.Machine$double.eps^(1/4), scale=EXr_smallchi[k])
  }
})

