test_that("draw_intensity() works", {
  set.seed(123)
  l <- function(t) {
    return(2)
  }
  lmaj <- 2.2
  S <- methods::new("rstream.mrg32k3a")

  expect_no_error(withr::with_preserve_seed(df <- draw_intensity(lambda = l)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)

  expect_no_error(withr::with_preserve_seed(df <- draw_intensity(lambda = l, lambda_maj = lmaj)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df <- draw_intensity(lambda = l, lambda_maj = lmaj, range_t = c(5, 10))))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # works with atmost1 = TRUE
  expect_no_error(
    withr::with_preserve_seed(
      df1 <- draw_intensity(
        lambda = l,
        lambda_maj = lmaj,
        range_t = c(5, 10),
        atmost1 = TRUE
      )
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 5, t_max = 10, atmost1 = TRUE)


  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(df2 <- draw_intensity(lambda = l, lambda_maj = lmaj, range_t = c(5, 10), rng_stream = S)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10)
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
      lambda_maj = c(2.2, 0),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)


  # works with negative slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- draw_intensity(
      lambda = l,
      lambda_maj = c(50, -1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # works with positive slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- draw_intensity(
      lambda = l,
      lambda_maj = c(2, .1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # fails when majorization function is below l
  # this is only when you sample at that point
  expect_error(withr::with_preserve_seed(
    df <- draw_intensity(
      lambda = l,
      lambda_maj = c(2, -.1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)
})
