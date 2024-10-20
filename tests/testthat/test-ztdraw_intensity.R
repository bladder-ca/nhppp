test_that("ztdraw_intensity() works", {
  set.seed(123)
  l <- function(t) {
    return(2)
  }
  lmaj_intercept <- 2.2
  lmaj_slope <- 0

  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity(lambda = l, line_majorizer_intercept = lmaj_intercept, line_majorizer_slope = lmaj_slope, t_min = 0, t_max = 10)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, atleast1 = TRUE)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity(lambda = l, line_majorizer_intercept = lmaj_intercept, line_majorizer_slope = lmaj_slope, t_min = 5, t_max = 10)))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)

  # works with atmost1
  expect_no_error(withr::with_preserve_seed(df2 <- ztdraw_intensity(lambda = l, line_majorizer_intercept = lmaj_intercept, line_majorizer_slope = lmaj_slope, t_min = 5, t_max = 10, atmost1 = TRUE)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10, atleast1 = TRUE, atmost1 = TRUE)


  lambda_maj <- c(2, 2.1, 2.9, 4)

  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity(lambda = l, t_min = 0, t_max = 10, step_majorizer_vector = lambda_maj)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, atleast1 = TRUE)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity(lambda = l, t_min = 5, t_max = 10, step_majorizer_vector = lambda_maj)))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)

  # works with atmost1 = TRUE
  expect_no_error(
    withr::with_preserve_seed(
      df1 <- ztdraw_intensity(
        lambda = l, t_min = 5, t_max = 10, step_majorizer_vector = lambda_maj, atmost1 = TRUE
      )
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 5, t_max = 10, atmost1 = TRUE, atleast1 = TRUE)
})
