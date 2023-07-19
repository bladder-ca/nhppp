test_that("ppp_next_n works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_next_n(n = 10, rate = 1, t_min = 5)
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, size = 10)
  expect_identical(withr::with_seed(12345, cumsum(rng_stream_rexp(size = 10, rate = 1)) + 5), df1)

  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_next_n(n = 10, rate = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, size = 10)

  # check RNGClass
  expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  expect_no_error(df3 <- ppp_next_n(n = 10, rate = 1, rng_stream = S2))
  check_ppp_sample_validity(times = df3, t_min = 0, size = 10)
})


test_that("ppp_t() works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_t(range_t = c(0, 10), rate = 1)
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  withr::with_seed(12345, df2 <- cumsum(rng_stream_rexp(size = 10, rate = 1)))
  expect_identical(df2[df2 <= 10], df1)

  # check only1 = T
  withr::with_seed(12345, df3 <- ppp_t(range_t = c(0, 10), rate = 1, only1 = TRUE))
  expect_identical(df3, df1[1])

  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_t(range_t = c(0, 10), rate = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10)


  # check RNGClass
  expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  expect_no_error(df3 <- ppp_t(range_t = c(0, 10), rate = 1, rng_stream = S2))
  check_ppp_sample_validity(times = df3, t_min = 0, t_max = 10)
})




test_that("ppp_n() works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_n(size = 10, range_t = c(0, 10))
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, size = 10)
  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_n(size = 10, range_t = c(0, 10), rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10, size = 10)

  # check RNGClass
  expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  expect_no_error(df3 <- ppp_n(size = 10, range_t = c(0, 10), rng_stream = S2))
  check_ppp_sample_validity(times = df3, t_min = 0, t_max = 10, size = 10)
})


test_that("ppp_t_orderstat() works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_t_orderstat(range_t = c(0, 10), rate = 1)
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  # check rstream

  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_t_orderstat(range_t = c(0, 10), rate = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10)

  # check RNGClass
  expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  expect_no_error(df3 <- ppp_t_orderstat(range_t = c(0, 10), rate = 1, rng_stream = S2))
  check_ppp_sample_validity(times = df3, t_min = 0, t_max = 10)
})


test_that("ztppp_t() arguments work", {
  expect_no_error(df0 <- ztppp_t(range_t = c(0, 10), rate = 1, only1 = TRUE))
  check_ppp_sample_validity(times = df0, t_min = 0, t_max = 10, zero_truncated = TRUE, only1 = TRUE)

  expect_no_error(df1 <- ztppp_t(range_t = c(0, 10), rate = 1))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, zero_truncated = TRUE)

  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_t(range_t = c(0, 10), rate = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10, zero_truncated = TRUE)


  # check RNGClass
  expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  expect_no_error(df3 <- ppp_t(range_t = c(0, 10), rate = 1, rng_stream = S2))
  check_ppp_sample_validity(times = df3, t_min = 0, t_max = 10, zero_truncated = TRUE)
})


test_that("ppp_t_piecewise() arguments work", {
  expect_no_error(df0 <- ppp_t_piecewise(times_vector = c(0, 10), rates_vector = 1, only1 = TRUE))
  check_ppp_sample_validity(times = df0, t_min = 0, t_max = 10, only1 = TRUE)

  expect_no_error(df0 <- ppp_t_piecewise(times_vector = c(0:10), rates_vector = runif(10), only1 = TRUE))
  check_ppp_sample_validity(times = df0, t_min = 0, t_max = 10, only1 = TRUE)

  expect_no_error(df1 <- ppp_t_piecewise(times_vector = c(0, 10), rates_vector = 1))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  expect_no_error(df1 <- ppp_t_piecewise(times_vector = c(0:10), rates_vector = runif(10)))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  expect_no_error(df1 <- ppp_t_piecewise(times_vector = c(0:10), rates_vector = runif(10)/1000, zero_truncated = TRUE))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, zero_truncated = TRUE)

  expect_no_error(df1 <- ppp_t_piecewise(times_vector = c(0:10), rates_vector = runif(10)/1000, only1= TRUE, zero_truncated = TRUE))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, only1 = TRUE, zero_truncated = TRUE)

  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_t_piecewise(times_vector = c(0, 10), rates_vector = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10)

  # check RNGClass
  expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  expect_no_error(df3 <- ppp_t_piecewise(times_vector = c(0, 10), rates_vector = 1, rng_stream = S2))
  check_ppp_sample_validity(times = df3, t_min = 0, t_max = 10)
})


