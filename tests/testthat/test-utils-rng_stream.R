test_that("rng_stream_runif() works without rstream object", {
  expect_no_error(withr::with_seed(12345, df1 <- rng_stream_runif(size = 10)))
  expect_identical(withr::with_seed(12345, rng_stream_runif(size = 10)), df1)
  expect_equal(length(df1), 10)
})

test_that("rng_stream_runif() works with rstream object", {
  # check rstream
  S <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- rng_stream_runif(size = 10, rng_stream = S))
  expect_equal(length(df2), 10)

  # Check that unpacking and resetting the rng_stream returns correct sequence
  rstream::rstream.packed(S) <- FALSE
  rstream::rstream.reset(S)
  expect_identical(rng_stream_runif(size = 10, rng_stream = S), df2)
})

test_that("rng_stream_rexp() works ", {
  expect_no_error(rng_stream_rexp(size = 10, rate = 1, rng_stream = NULL))
  expect_no_error(S1 <- methods::new("rstream.mrg32k3a"))
  expect_no_error(rng_stream_rexp(size = 10, rate = 1, rng_stream = S1))
})


test_that("rng_stream_rztpois() works ", {
  expect_no_error(rng_stream_rztpois(size = 10, lambda = 1, rng_stream = NULL))
  expect_no_error(S1 <- methods::new("rstream.mrg32k3a"))
  expect_no_error(rng_stream_rztpois(size = 10, lambda = 1, rng_stream = S1))
})


test_that("rztpois() works", {
  expect_no_error(rztpois(size = 10, lambda = 1))
  # vectorized for lambda
  expect_no_error(rztpois(size = 10, lambda = rep(1, 10)))
  expect_no_error(rztpois(size = 10, lambda = 1:10))

  expect_equal(sum(rztpois(4, c(rep(.0001, 3), 10000)) < 2), 3)
})
