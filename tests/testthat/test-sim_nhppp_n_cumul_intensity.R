test_that("nhppp_n_cumulative_intensity() works", {
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }
  S <- methods::new("rstream.mrg32k3a")
  expect_no_error(withr::with_preserve_seed(df <- nhppp_n_cumulative_intensity(size = 23, Lambda = L)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, size = 23)

  # works when Lambda_inv() is given
  expect_no_error(withr::with_preserve_seed(df1 <- nhppp_n_cumulative_intensity(size = 23, Lambda = L, Lambda_inv = Li)))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, size = 23)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df2 <- nhppp_n_cumulative_intensity(size = 23, Lambda = L, Lambda_inv = Li, range_t = c(5, 10))))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10, size = 23)

  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(df3 <- nhppp_n_cumulative_intensity(size = 23, Lambda = L, Lambda_inv = Li, range_t = c(5, 10), rng_stream = S)))
  check_ppp_sample_validity(times = df3, t_min = 5, t_max = 10, size = 23)
})
