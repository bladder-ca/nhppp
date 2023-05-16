test_that("ztnhppp_t_cumulative_intensity() works", {
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }

  S <- methods::new("rstream.mrg32k3a")
  expect_no_error(withr::with_preserve_seed(df <- ztnhppp_t_cumulative_intensity(Lambda = L)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, zero_truncated = TRUE)

  # works when Lambda_inv() is given
  expect_no_error(withr::with_preserve_seed(df1 <- ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = Li)))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, zero_truncated = TRUE)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df2 <- ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = c(5, 10))))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10, zero_truncated = TRUE)

  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(df3 <- ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = c(5, 10), rng_stream = S)))
  check_ppp_sample_validity(times = df3, t_min = 5, t_max = 10, zero_truncated = TRUE)

  # only 1 works
  expect_no_error(withr::with_preserve_seed(df4 <- ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = c(5, 10), rng_stream = S, only1 = TRUE)))
  check_ppp_sample_validity(times = df4, t_min = 5, t_max = 10, zero_truncated = TRUE, only1 = TRUE)

  # works with very small range
  expect_no_error(withr::with_preserve_seed(df4 <- ztnhppp_t_cumulative_intensity(Lambda = L, Lambda_inv = Li, range_t = c(5, 5 + 10^-8), rng_stream = S, only1 = TRUE)))
  check_ppp_sample_validity(times = df4, t_min = 5, t_max = 5 + 10^-8, zero_truncated = TRUE, only1 = TRUE)
})
