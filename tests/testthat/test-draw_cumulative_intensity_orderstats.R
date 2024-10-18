test_that("draw_cumulative_intensity_orderstats() works", {
  set.seed(123)
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }

  # works when Lambda_inv() is given
  expect_no_error(withr::with_preserve_seed(df1 <- draw_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, t_min = 0, t_max = 10)))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df2 <- draw_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, t_min = 5, t_max = 10)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10)

  # works with atmost1=TRUE
  expect_no_error(withr::with_preserve_seed(df4 <- draw_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, t_min = 5, t_max = 10, atmost1 = TRUE)))
  check_ppp_sample_validity(times = df4, t_min = 5, t_max = 10, atmost1 = TRUE)
})
