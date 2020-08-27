context("EX_GIG tests: regular case")

#### load parameters to test ####
if(!exists("regchi")) source("./tests/testthat/helper.R")
chi <- regchi
omega <- sqrt(chi*psi)
eta <- sqrt(chi/psi)


#### lambda < -1 ####
test_that("EX_GIG: lambda < -1", {
	lambda <- -4
	EX <- EX_GIG(lambda, chi, psi)
	EX_regchi <- moments_GIG_raw(1, lambda, chi, psi)

  expect_is(EX, "numeric")
  expect_is(EX_regchi, "numeric")
  expect_length(EX, length(chi))
  expect_length(EX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EX[k], EX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EX_regchi[k])
  }
})

test_that("EX_GIG: lambda < -1, input omega & eta", {
  lambda <- -4
  EX <- EX_GIG(lambda, omega=omega, eta=eta)
  EX_regchi <- moments_GIG_raw(1, lambda, omega=omega, eta=eta)

  expect_is(EX, "numeric")
  expect_is(EX_regchi, "numeric")
  expect_length(EX, length(chi))
  expect_length(EX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EX[k], EX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EX_regchi[k])
  }
})


#### lambda == -1 ####
test_that("EX_GIG: lambda == -1", {
  lambda <- -1
  EX <- EX_GIG(lambda, chi, psi)
  EX_regchi <- moments_GIG_raw(1, lambda, chi, psi)

  expect_is(EX, "numeric")
  expect_is(EX_regchi, "numeric")
  expect_length(EX, length(chi))
  expect_length(EX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EX[k], EX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EX_regchi[k])
  }
})

test_that("EX_GIG: lambda == -1, input omega & eta", {
  lambda <- -1
  EX <- EX_GIG(lambda, omega=omega, eta=eta)
  EX_regchi <- moments_GIG_raw(1, lambda, omega=omega, eta=eta)

  expect_is(EX, "numeric")
  expect_is(EX_regchi, "numeric")
  expect_length(EX, length(chi))
  expect_length(EX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EX[k], EX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EX_regchi[k])
  }
})

#### lambda in (-1,0) ####
test_that("EX_GIG: lambda in (-1,0)", {
  lambda <- -0.5
  EX <- EX_GIG(lambda, chi, psi)
  EX_regchi <- moments_GIG_raw(1, lambda, chi, psi)

  expect_is(EX, "numeric")
  expect_is(EX_regchi, "numeric")
  expect_length(EX, length(chi))
  expect_length(EX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EX[k], EX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EX_regchi[k])
  }
})

test_that("EX_GIG: lambda in (-1,0), input omega & eta", {
  lambda <- -0.5
  EX <- EX_GIG(lambda, omega=omega, eta=eta)
  EX_regchi <- moments_GIG_raw(1, lambda, omega=omega, eta=eta)

  expect_is(EX, "numeric")
  expect_is(EX_regchi, "numeric")
  expect_length(EX, length(chi))
  expect_length(EX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EX[k], EX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EX_regchi[k])
  }
})

#### lambda == 0 ####
test_that("EX_GIG: lambda == 0", {
  lambda <- 0
  EX <- EX_GIG(lambda, chi, psi)
  EX_regchi <- moments_GIG_raw(1, lambda, chi, psi)

  expect_is(EX, "numeric")
  expect_is(EX_regchi, "numeric")
  expect_length(EX, length(chi))
  expect_length(EX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EX[k], EX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EX_regchi[k])
  }
})

test_that("EX_GIG: lambda == 0, input omega & eta", {
  lambda <- 0
  EX <- EX_GIG(lambda, omega=omega, eta=eta)
  EX_regchi <- moments_GIG_raw(1, lambda, omega=omega, eta=eta)

  expect_is(EX, "numeric")
  expect_is(EX_regchi, "numeric")
  expect_length(EX, length(chi))
  expect_length(EX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EX[k], EX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EX_regchi[k])
  }
})


#### lambda > 0 ####
test_that("EX_GIG: lambda > 0", {
  lambda <- 4
  EX <- EX_GIG(lambda, chi, psi)
  EX_regchi <- moments_GIG_raw(1, lambda, chi, psi)

  expect_is(EX, "numeric")
  expect_is(EX_regchi, "numeric")
  expect_length(EX, length(chi))
  expect_length(EX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EX[k], EX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EX_regchi[k])
  }
})

test_that("EX_GIG: lambda > 0, input omega & eta", {
  lambda <- 4
  EX <- EX_GIG(lambda, omega=omega, eta=eta)
  EX_regchi <- moments_GIG_raw(1, lambda, omega=omega, eta=eta)

  expect_is(EX, "numeric")
  expect_is(EX_regchi, "numeric")
  expect_length(EX, length(chi))
  expect_length(EX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(EX[k], EX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=EX_regchi[k])
  }
})

