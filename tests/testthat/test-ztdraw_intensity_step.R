test_that("ztdraw_intensity_step() works", {
  set.seed(123)
  l <- function(t) {
    return(2)
  }
  times <- c(0, 1, pi, 2 * pi, 10)
  lambda_maj <- c(2, 2.1, 2.9, 4)

  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity_step(lambda = l, time_breaks = times, majorizer_vector = lambda_maj)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, atleast1 = TRUE)

  # works when range_t[1]>0
  times2 <- c(0, 1, pi, 2 * pi, 10) / 2 + 5
  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity_step(lambda = l, time_breaks = times2, majorizer_vector = lambda_maj)))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)

  # works with atmost1 = TRUE
  expect_no_error(
    withr::with_preserve_seed(
      df1 <- ztdraw_intensity_step(
        lambda = l, time_breaks = times2, majorizer_vector = lambda_maj, atmost1 = TRUE
      )
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 5, t_max = 10, atmost1 = TRUE, atleast1 = TRUE)
})
