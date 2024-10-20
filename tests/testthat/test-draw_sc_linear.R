test_that("draw_sc_linear() works", {
  withr::with_seed(12345, df1 <- draw_sc_linear(intercept = 2, slope = 0, t_min = 1, t_max = 10))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)

  withr::with_seed(12345, df1 <- draw_sc_linear(intercept = 0, slope = 1, t_min = 1, t_max = 10))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)

  withr::with_seed(12345, df1 <- draw_sc_linear(intercept = 10, slope = -2, t_min = 1, t_max = 10))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10)

  withr::with_seed(12345, df1 <- draw_sc_linear(intercept = 2, slope = 0, t_min = 1, t_max = 10))
  withr::with_seed(12345, df2 <- ppp2(rate = 2, t_min = 1, t_max = 10))
  expect_identical(df1, df2)

  withr::with_seed(12345, df1 <- draw_sc_linear(intercept = 0, slope = 1, t_min = 1, t_max = 10))
  withr::with_seed(12345, df2 <- draw_cumulative_intensity_inversion(
    Lambda = function(t) Lambda_linear_form(t, intercept = 0, slope = 1, t0 = 1),
    Lambda_inv = function(z) Lambda_inv_linear_form(z, intercept = 0, slope = 1, t0 = 1),
    t_min = 1, t_max = 10
  ))
  expect_identical(round(df1, 3), round(df2, 3))

  withr::with_seed(12345, df1 <- draw_sc_linear(intercept = 10, slope = -2, t_min = 1, t_max = 10))
  withr::with_seed(12345, df2 <- draw_cumulative_intensity_inversion(
    Lambda = function(t) Lambda_linear_form(t, intercept = 10, slope = -2, t0 = 1),
    Lambda_inv = function(z) Lambda_inv_linear_form(z, intercept = 10, slope = -2, t0 = 1),
    t_min = 1, t_max = 5
  ))
  expect_identical(round(df1, 3), round(df2, 3))
})
