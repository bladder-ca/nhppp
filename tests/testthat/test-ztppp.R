test_that("ztppp() arguments work", {
  set.seed(123)
  expect_no_error(df0 <- ztppp(range_t = c(0, 10), rate = 1, atmost1 = TRUE))
  check_ppp_sample_validity(times = df0, t_min = 0, t_max = 10, atleast1 = TRUE, atmost1 = TRUE)

  expect_no_error(df1 <- ztppp(range_t = c(0, 10), rate = 1))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, atleast1 = TRUE)

  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_sequential(range_t = c(0, 10), rate = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10, atleast1 = TRUE)
})
