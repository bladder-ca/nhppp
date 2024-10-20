test_that("ztdraw_cumulative_intensity() works", {
  set.seed(123)
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }

  expect_no_error(withr::with_preserve_seed(df1 <- ztdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = 0, t_max = 10)))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, atleast1 = TRUE)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df2 <- ztdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = 5, t_max = 10)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10, atleast1 = TRUE)

  # only 1 works
  expect_no_error(withr::with_preserve_seed(df4 <- ztdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = 5, t_max = 10, atmost1 = TRUE)))
  check_ppp_sample_validity(times = df4, t_min = 5, t_max = 10, atleast1 = TRUE, atmost1 = TRUE)

  # works with very small range
  expect_no_error(withr::with_preserve_seed(df4 <- ztdraw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = 5, t_max = 5 + 10^-8, atmost1 = TRUE)))
  check_ppp_sample_validity(times = df4, t_min = 5, t_max = 5 + 10^-8, atleast1 = TRUE, atmost1 = TRUE)
})
