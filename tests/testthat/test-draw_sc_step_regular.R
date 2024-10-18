test_that("draw_sc_step_regular() arguments work", {
  set.seed(123)
  l <- stats::runif(10)
  L <- cumsum(l)

  expect_no_error(df0 <- draw_sc_step_regular(t_min = 0, t_max = 10, lambda_vector = 1, atmost1 = TRUE))
  check_ppp_sample_validity(times = df0, t_min = 0, t_max = 10, atmost1 = TRUE)

  expect_no_error(df0a <- draw_sc_step_regular(t_min = 0, t_max = 10, lambda_vector = l, atmost1 = TRUE))
  check_ppp_sample_validity(times = df0a, t_min = 0, t_max = 10, atmost1 = TRUE)

  expect_no_error(df0b <- draw_sc_step_regular(t_min = 0, t_max = 10, Lambda_vector = 1, atmost1 = TRUE))
  check_ppp_sample_validity(times = df0b, t_min = 0, t_max = 10, atmost1 = TRUE)

  expect_no_error(df0c <- draw_sc_step_regular(t_min = 0, t_max = 10, Lambda_vector = L, atmost1 = TRUE))
  check_ppp_sample_validity(times = df0c, t_min = 0, t_max = 10, atmost1 = TRUE)

  expect_no_error(df1 <- draw_sc_step_regular(t_min = 0, t_max = 10, Lambda_vector = 1))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  expect_no_error(df1a <- draw_sc_step_regular(t_min = 0, t_max = 10, Lambda_vector = L))
  check_ppp_sample_validity(times = df1a, t_min = 0, t_max = 10)

  expect_no_error(df1b <- draw_sc_step_regular(t_min = 0, t_max = 10, Lambda_vector = L / 1000, atleast1 = TRUE))
  check_ppp_sample_validity(times = df1b, t_min = 0, t_max = 10, atleast1 = TRUE)

  expect_no_error(df1c <- draw_sc_step_regular(t_min = 0, t_max = 10, Lambda_vector = L / 1000, atmost1 = TRUE, atleast1 = TRUE))
  check_ppp_sample_validity(times = df1c, t_min = 0, t_max = 10, atmost1 = TRUE, atleast1 = TRUE)
})
