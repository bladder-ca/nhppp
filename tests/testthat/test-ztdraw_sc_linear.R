test_that("ztdraw_sc_linear() works", {
  withr::with_seed(12345, df1 <- ztdraw_sc_linear(intercept = 2, slope = 0, t_min = 1, t_max = 10))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, atleast1 = TRUE)

  withr::with_seed(12345, df1 <- ztdraw_sc_linear(intercept = 0, slope = 1, t_min = 1, t_max = 10))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, atleast1 = TRUE)

  withr::with_seed(12345, df1 <- ztdraw_sc_linear(intercept = 10, slope = -2, t_min = 1, t_max = 10))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, atleast1 = TRUE)

  withr::with_seed(12345, df1 <- ztdraw_sc_linear(intercept = 10, slope = -2, t_min = 1, t_max = 10, atmost1 = TRUE))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, atmost1 = TRUE, atleast1 = TRUE)
})
