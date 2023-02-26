test_that("nhppp_n_intensity_linear() works", {
  withr::with_seed(12345, df1 <- nhppp_n_intensity_linear(size = 23, alpha = 2, beta = 0, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, size = 23)

  withr::with_seed(12345, df1 <- nhppp_n_intensity_linear(size = 23, alpha = 0, beta = 1, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, size = 23)

  withr::with_seed(12345, df1 <- nhppp_n_intensity_linear(size = 23, alpha = 10, beta = -2, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, size = 23)
})
