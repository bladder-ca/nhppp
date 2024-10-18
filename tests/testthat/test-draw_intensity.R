test_that("draw_intensity() works", {
  set.seed(123)
  l <- function(t) {
    return(2)
  }

  expect_no_error(withr::with_preserve_seed(df <- draw_intensity(lambda = l, line_majorizer_intercept = 2.2, line_majorizer_slope = 0, t_min = 0, t_max = 10)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df <- draw_intensity(lambda = l, line_majorizer_intercept = 2.2, line_majorizer_slope = 0, t_min = 5, t_max = 10)))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # works with atmost1 = TRUE
  expect_no_error(
    withr::with_preserve_seed(
      df1 <- draw_intensity(
        lambda = l,
        line_majorizer_intercept = 2.2,
        line_majorizer_slope = 0,
        t_min = 5,
        t_max = 10,
        atmost1 = TRUE
      )
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 5, t_max = 10, atmost1 = TRUE)
})


test_that("draw_intensity() works with linear majorization function", {
  set.seed(123)
  l <- function(t) {
    return(2)
  }
  S <- methods::new("rstream.mrg32k3a")

  # works with zero slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- draw_intensity(
      lambda = l,
      line_majorizer_intercept = 2.2,
      line_majorizer_slope = 0,
      t_min = 5,
      t_max = 10
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)


  # works with negative slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- draw_intensity(
      lambda = l,
      line_majorizer_intercept = 50,
      line_majorizer_slope = -1,
      t_min = 5,
      t_max = 10
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # works with positive slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- draw_intensity(
      lambda = l,
      line_majorizer_intercept = 2,
      line_majorizer_slope = 0.1,
      t_min = 5,
      t_max = 10
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # fails when majorization function is below l
  # this is only when you sample at that point
  expect_error(withr::with_preserve_seed(
    df <- draw_intensity(
      lambda = l,
      line_majorizer_intercept = 2.2,
      line_majorizer_slope = -0.1,
      t_min = 5,
      t_max = 10
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)
})
