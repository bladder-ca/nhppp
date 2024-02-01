test_that("ppp_sequential() works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_sequential(range_t = c(0, 10), rate = 1)
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  withr::with_seed(12345, df2 <- cumsum(rng_stream_rexp(size = 10, rate = 1)))
  expect_identical(df2[df2 <= 10], df1)

  # check atmost1 = T
  withr::with_seed(12345, df3 <- ppp_sequential(range_t = c(0, 10), rate = 1, atmost1 = TRUE))
  expect_identical(df3, df1[1])

  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_sequential(range_t = c(0, 10), rate = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10)


  # # check RNGClass
  # expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  # expect_no_error(df3 <- ppp_sequential(range_t = c(0, 10), rate = 1, rng_stream = S2))
  # check_ppp_sample_validity(times = df3, t_min = 0, t_max = 10)
})
