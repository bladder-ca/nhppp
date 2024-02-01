test_that("ppp_next_n() works", {
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

  # # check RNGClass
  # expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  # expect_no_error(df3 <- ppp_next_n(n = 10, rate = 1, rng_stream = S2))
  # check_ppp_sample_validity(times = df3, t_min = 0, size = 10)
})
