test_that("draw_sc_loglinear() works", {
  withr::with_seed(12345, df1 <- draw_sc_loglinear(intercept = 2, slope = 0, t_min = 1, t_max = 10))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)

  withr::with_seed(12345, df1 <- draw_sc_loglinear(intercept = 0, slope = 1, t_min = 1, t_max = 10))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)

  withr::with_seed(12345, df1 <- draw_sc_loglinear(intercept = 10, slope = -2, t_min = 1, t_max = 10))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)
})
