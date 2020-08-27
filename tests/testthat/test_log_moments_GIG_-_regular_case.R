context("log_moments_GIG tests: regular case")

#### load parameters to test ####
if(!exists("regchi")) source("./tests/testthat/helper.R")
chi <- regchi
omega <- sqrt(chi*psi)
eta <- sqrt(chi/psi)

#### lambda < -m ####
test_that("log_moments_GIG: lambda < -m", {
	lambda <- -4
	ElogX <- log_moments_GIG(m, lambda, chi, psi)
	ElogX_regchi <- DmomentsGIG_Dm(m, lambda, chi, psi)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_regchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_regchi[k]))
  }
})

test_that("log_moments_GIG: lambda < -m, input omega & eta", {
  lambda <- -4
  ElogX <- log_moments_GIG(m, lambda, omega=omega, eta=eta)
  ElogX_regchi <- DmomentsGIG_Dm(m, lambda, omega=omega, eta=eta)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_regchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_regchi[k]))
  }
})


#### lambda == -m ####
test_that("log_moments_GIG: lambda == -m", {
  lambda <- -m
  ElogX <- log_moments_GIG(m, lambda, chi, psi)
  ElogX_regchi <- DmomentsGIG_Dm(m, lambda, chi, psi)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_regchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_regchi[k]))
  }
})

test_that("log_moments_GIG: lambda == -m, input omega & eta", {
  lambda <- -m
  ElogX <- log_moments_GIG(m, lambda, omega=omega, eta=eta)
  ElogX_regchi <- DmomentsGIG_Dm(m, lambda, omega=omega, eta=eta)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_regchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_regchi[k]))
  }
})

#### lambda in (-m,0) ####
test_that("log_moments_GIG: lambda in (-m,0)", {
  lambda <- -m/2
  ElogX <- log_moments_GIG(m, lambda, chi, psi)
  ElogX_regchi <- DmomentsGIG_Dm(m, lambda, chi, psi)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_regchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_regchi[k]))
  }
})

test_that("log_moments_GIG: lambda in (-m,0), input omega & eta", {
  lambda <- -m/2
  ElogX <- log_moments_GIG(m, lambda, omega=omega, eta=eta)
  ElogX_regchi <- DmomentsGIG_Dm(m, lambda, omega=omega, eta=eta)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_regchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_regchi[k]))
  }
})

#### lambda == 0 ####
test_that("log_moments_GIG: lambda == 0", {
  lambda <- 0
  ElogX <- log_moments_GIG(m, lambda, chi, psi)
  ElogX_regchi <- DmomentsGIG_Dm(m, lambda, chi, psi)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_regchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_regchi[k]))
  }
})

test_that("log_moments_GIG: lambda == 0, input omega & eta", {
  lambda <- 0
  ElogX <- log_moments_GIG(m, lambda, omega=omega, eta=eta)
  ElogX_regchi <- DmomentsGIG_Dm(m, lambda, omega=omega, eta=eta)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_regchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_regchi[k]))
  }
})

#### lambda > 0 ####
test_that("log_moments_GIG: lambda > 0", {
  lambda <- 4
  ElogX <- log_moments_GIG(m, lambda, chi, psi)
  ElogX_regchi <- DmomentsGIG_Dm(m, lambda, chi, psi)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_regchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_regchi[k]))
  }
})

test_that("log_moments_GIG: lambda > 0, input omega & eta", {
  lambda <- 4
  ElogX <- log_moments_GIG(m, lambda, omega=omega, eta=eta)
  ElogX_regchi <- DmomentsGIG_Dm(m, lambda, omega=omega, eta=eta)

  expect_is(ElogX, "numeric")
  expect_is(ElogX_regchi, "numeric")
  expect_length(ElogX, length(chi))
  expect_length(ElogX_regchi, length(chi))
  for(k in seq_along(chi)){
    expect_equal(ElogX[k], ElogX_regchi[k], tolerance=.Machine$double.eps^(1/4), scale=abs(ElogX_regchi[k]))
  }
})
