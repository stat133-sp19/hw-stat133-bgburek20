library(testthat)
context('Test Binomial Package')

test_that("bin_choose() works as expected", {
  expect_equal(bin_choose(5, 2), (factorial(5))/
                 (factorial(2) * factorial(5-2)))
  expect_error(bin_choose(2, 5), "k cannot be greater than n")
  expect_length(bin_choose(5, 2), 1)
})

test_that("bin_probability() works as expected", {
  expect_equal(bin_probability(2, 5, .5),
               bin_choose(5, 2) * .5 ** 2 * (1 - .5) ** (5 - 2))
  expect_length(bin_probability(2, 5, .5), 1)
  expect_length(bin_probability(0:2, 5, .5), 3)
  expect_type(bin_probability(2, 5, .5), 'double')
})

test_that("bin_distribution() works as expected", {
  frame <- bin_distribution(5, .5)
  expect_equal(frame$success, c(0, 1, 2, 3, 4, 5))
  expect_type(frame, 'list')
  expect_length(frame, 2)
})

test_that("plot.bindis() works as expected", {
  frame <- bin_distribution(5, .5)
  frame2 <- bin_distribution(6, .6)
  expect_type(plot(frame), 'list')
  expect_length(plot(frame), 2)
  expect_length(plot(frame2), 2)
})

test_that("bin_cumulative works as expected", {
  frame <- bin_cumulative(5, .5)
  expect_equal(frame$success, c(0, 1, 2, 3, 4, 5))
  expect_type(frame, 'list')
  expect_length(frame, 3)
})

test_that("plot.bincum() works as expected", {
  frame <- bin_cumulative(5, .5)
  frame2 <- bin_cumulative(6, .6)
  expect_type(plot(frame), 'list')
  expect_length(plot(frame), 3)
  expect_length(plot(frame2), 3)
})

test_that("bin_variable() works as expected", {
  expect_type(bin_variable(5, .5), 'list')
  expect_length(bin_variable(5, .5), 2)
  expect_equal(lst$prob, .5)
  expect_equal(lst$trials, 5)
})

test_that("print.binvar() works as expected", {
  lst <- bin_variable(5, .5)
  expect_equal(lst$prob, .5)
  expect_equal(lst$trials, 5)
  expect_type(lst, 'list')
  expect_length(lst, 2)
})

test_that("summary.binvar() works as expected", {
  lst <- bin_variable(5, .5)
  expect_type(summary(lst), 'list')
  expect_length(summary(lst), 2)
  expect_equal(lst$trials, 5)
  expect_equal(lst$prob, .5)
})

test_that("print.summary.binvar() works as expected", {
  expect_type(summary(lst), 'list')
  expect_length(summary(lst), 2)
  expect_equal(lst$trials, 5)
  expect_equal(lst$prob, .5)
})

test_that("bin_mean() works as expected", {
  expect_equal(bin_mean(20, .5), 20 * .5)
  expect_type(bin_mean(20, .5), 'double')
  expect_length(bin_mean(20, .5), 1)
})


test_that("bin_variance() works as expected", {
  expect_equal(bin_variance(20, .5), 20 * .5 * (1-.5))
  expect_type(bin_variance(20, .5), 'double')
  expect_length(bin_variance(20, .5), 1)
})

test_that("bin_mode() works as expected", {
  expect_equal(bin_mode(19, .5), c((19 * .5 + .5),
                                   (19 * .5 + .5 - 1)))
  expect_type(bin_mode(19, .5), 'double')
  expect_length(bin_mode(19, .5), 2)
  expect_equal(bin_mode(20, .5), floor(20 * .5 + .5))
  expect_type(bin_mode(20, .5), 'double')
  expect_length(bin_mode(20, .5), 1)
})

test_that("bin_skewness() works as expected", {
  expect_equal(bin_skewness(20, .5), (1-2*.5) / (sqrt(20*.5*(1-.5))))
  expect_type(bin_skewness(20, .5), 'double')
  expect_length(bin_variance(20, .5), 1)
})

test_that("bin_kurtosis() works as expected", {
  expect_equal(bin_kurtosis(20, .5), (1 - ((6 * .5) * (1 - .5)))/
                 (20 * .5* (1 - .5)))
  expect_type(bin_kurtosis(20, .5), 'double')
  expect_length(bin_kurtosis(20, .5), 1)
})

