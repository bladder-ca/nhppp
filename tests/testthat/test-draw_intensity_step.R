test_that("draw_intensity_step() works", {
  l <- function(t) {
    return(rep(2, length(t)))
  }
  times <- c(0, 1, pi, 2 * pi, 10)
  lambda_maj <- c(2, 2.1, 2.9, 4)
  S <- methods::new("rstream.mrg32k3a")

  expect_no_error(withr::with_preserve_seed(df <- draw_intensity_step(lambda = l, times_vector = times, lambda_maj_vector = lambda_maj)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)

  # works when range_t[1]>0
  times2 <- c(0, 1, pi, 2 * pi, 10) / 2 + 5
  expect_no_error(withr::with_preserve_seed(df <- draw_intensity_step(lambda = l, times_vector = times2, lambda_maj_vector = lambda_maj)))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # works with atmost1 = TRUE
  expect_no_error(
    withr::with_preserve_seed(
      df1 <- draw_intensity_step(
        lambda = l, times_vector = times2, lambda_maj_vector = lambda_maj, atmost1 = TRUE
      )
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 5, t_max = 10, atmost1 = TRUE)

  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(df2 <- draw_intensity_step(lambda = l, times_vector = times2, lambda_maj_vector = lambda_maj, rng_stream = S)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10)
})
