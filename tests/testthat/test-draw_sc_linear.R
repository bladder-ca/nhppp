test_that("draw_sc_linear() works", {
  withr::with_seed(12345, df1 <- draw_sc_linear(alpha = 2, beta = 0, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)

  withr::with_seed(12345, df1 <- draw_sc_linear(alpha = 0, beta = 1, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)

  withr::with_seed(12345, df1 <- draw_sc_linear(alpha = 10, beta = -2, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)

  withr::with_seed(12345, df1 <- draw_sc_linear(alpha = 2, beta = 0, range_t = c(1, 10)))
  withr::with_seed(12345, df2 <- ppp_orderstat(rate = 2, range_t = c(1, 10)))
  expect_identical(df1, df2)

  withr::with_seed(12345, df1 <- draw_sc_linear(alpha = 0, beta = 1, range_t = c(1, 10)))
  withr::with_seed(12345, df2 <- draw_cumulative_intensity_inversion(
    Lambda = function(t) Lambda_linear_form(t, alpha = 0, beta = 1, t0 = 1),
    range_t = c(1, 10)
  ))
  expect_identical(round(df1, 3), round(df2, 3))

  withr::with_seed(12345, df1 <- draw_sc_linear(alpha = 10, beta = -2, range_t = c(1, 10)))
  withr::with_seed(12345, df2 <- draw_cumulative_intensity_inversion(
    Lambda = function(t) Lambda_linear_form(t, alpha = 10, beta = -2, t0 = 1),
    range_t = c(1, 5)
  ))
  expect_identical(round(df1, 3), round(df2, 3))
})
