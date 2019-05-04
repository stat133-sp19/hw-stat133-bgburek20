library(testthat)
context('Test Summary Measures')

test_that("aux_mean() works as expected", {
  expect_equal(aux_mean(20, .5), 20 * .5)
  expect_type(aux_mean(20, .5), 'double')
  expect_length(aux_mean(20, .5), 1)
})


test_that("aux_variance() works as expected", {
  expect_equal(aux_variance(20, .5), 20 * .5 * (1-.5))
  expect_type(aux_variance(20, .5), 'double')
  expect_length(aux_variance(20, .5), 1)
})

test_that("aux_mode() works as expected", {
  expect_equal(aux_mode(19, .5), c((19 * .5 + .5),
                                   (19 * .5 + .5 - 1)))
  expect_type(aux_mode(19, .5), 'double')
  expect_length(aux_mode(19, .5), 2)
  expect_equal(aux_mode(20, .5), floor(20 * .5 + .5))
  expect_type(aux_mode(20, .5), 'double')
  expect_length(aux_mode(20, .5), 1)
})

test_that("aux_skewness() works as expected", {
  expect_equal(aux_skewness(20, .5), (1-2*.5) / (sqrt(20*.5*(1-.5))))
  expect_type(aux_skewness(20, .5), 'double')
  expect_length(aux_variance(20, .5), 1)
})

test_that("aux_kurtosis() works as expected", {
  expect_equal(aux_kurtosis(20, .5), (1 - ((6 * .5) * (1 - .5)))/
                 (20 * .5* (1 - .5)))
  expect_type(aux_kurtosis(20, .5), 'double')
  expect_length(aux_kurtosis(20, .5), 1)
})




