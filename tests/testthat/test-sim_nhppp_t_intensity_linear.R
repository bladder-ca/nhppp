test_that("draw_intensity_linear() works", {
  withr::with_seed(12345, df1 <- draw_intensity_linear(alpha = 2, beta = 0, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)

  withr::with_seed(12345, df1 <- draw_intensity_linear(alpha = 0, beta = 1, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)

  withr::with_seed(12345, df1 <- draw_intensity_linear(alpha = 10, beta = -2, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)
})
