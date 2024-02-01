test_that("draw_sc_step_regular() arguments work", {
  l <- stats::runif(10)
  L <- cumsum(l)

  expect_no_error(df0 <- draw_sc_step_regular(range_t = c(0, 10), lambda_vector = 1, atmost1 = TRUE))
  check_ppp_sample_validity(times = df0, t_min = 0, t_max = 10, atmost1 = TRUE)

  expect_no_error(df0 <- draw_sc_step_regular(range_t = c(0, 10), lambda_vector = l, atmost1 = TRUE))
  check_ppp_sample_validity(times = df0, t_min = 0, t_max = 10, atmost1 = TRUE)

  expect_no_error(df0 <- draw_sc_step_regular(range_t = c(0, 10), Lambda_vector = 1, atmost1 = TRUE))
  check_ppp_sample_validity(times = df0, t_min = 0, t_max = 10, atmost1 = TRUE)

  expect_no_error(df0 <- draw_sc_step_regular(range_t = c(0, 10), Lambda_vector = L, atmost1 = TRUE))
  check_ppp_sample_validity(times = df0, t_min = 0, t_max = 10, atmost1 = TRUE)

  expect_no_error(df1 <- draw_sc_step_regular(range_t = c(0, 10), Lambda_vector = 1))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  expect_no_error(df1 <- draw_sc_step_regular(range_t = c(0, 10), Lambda_vector = L))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  expect_no_error(df1 <- draw_sc_step_regular(range_t = c(0, 10), Lambda_vector = L / 1000, atleast1 = TRUE))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, atleast1 = TRUE)

  expect_no_error(df1 <- draw_sc_step_regular(range_t = c(0, 10), Lambda_vector = L / 1000, atmost1 = TRUE, atleast1 = TRUE))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, atmost1 = TRUE, atleast1 = TRUE)

  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- draw_sc_step_regular(range_t = c(0, 10), Lambda_vector = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10)

  # # check RNGClass
  # expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  # expect_no_error(df3 <- draw_sc_step_regular(range_t =  c(0,  10), Lambda_vector = 1, rng_stream = S2))
  # check_ppp_sample_validity(times = df3, t_min = 0, t_max = 10)
})
