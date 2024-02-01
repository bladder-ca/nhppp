test_that("ztdraw_sc_linear() works", {
  withr::with_seed(12345, df1 <- ztdraw_sc_linear(alpha = 2, beta = 0, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, atleast1 = TRUE)

  withr::with_seed(12345, df1 <- ztdraw_sc_linear(alpha = 0, beta = 1, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, atleast1 = TRUE)

  withr::with_seed(12345, df1 <- ztdraw_sc_linear(alpha = 10, beta = -2, range_t = c(1, 10)))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, atleast1 = TRUE)

  withr::with_seed(12345, df1 <- ztdraw_sc_linear(alpha = 10, beta = -2, range_t = c(1, 10), atmost1 = TRUE))
  check_ppp_sample_validity(times = df1, t_min = 1, t_max = 10, atmost1 = TRUE, atleast1 = TRUE)
})
