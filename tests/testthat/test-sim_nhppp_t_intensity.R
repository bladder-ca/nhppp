test_that("nhppp_t_intensity() works", {
  l <- function(t) {
    return(2)
  }
  lmaj <- 2.2
  S <- methods::new("rstream.mrg32k3a")

  expect_no_error(withr::with_preserve_seed(df <- nhppp_t_intensity(lambda = l)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)

  expect_no_error(withr::with_preserve_seed(df <- nhppp_t_intensity(lambda = l, lambda_maj = lmaj)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df <- nhppp_t_intensity(lambda = l, lambda_maj = lmaj, range_t = c(5, 10))))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # works with only1 = TRUE
  expect_no_error(
    withr::with_preserve_seed(
      df1 <- nhppp_t_intensity(
        lambda = l,
        lambda_maj = lmaj,
        range_t = c(5, 10),
        only1 = TRUE
      )
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 5, t_max = 10, only1 = TRUE)


  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(df2 <- nhppp_t_intensity(lambda = l, lambda_maj = lmaj, range_t = c(5, 10), rng_stream = S)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10)
})


test_that("nhppp_t_intensity() works with linear majorization function", {
  l <- function(t) {
    return(2)
  }
  S <- methods::new("rstream.mrg32k3a")

  # works with zero slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- nhppp_t_intensity(
      lambda = l,
      lambda_maj = c(2.2, 0),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)


  # works with negative slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- nhppp_t_intensity(
      lambda = l,
      lambda_maj = c(50, -1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # works with positive slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- nhppp_t_intensity(
      lambda = l,
      lambda_maj = c(2, .1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # fails when majorization function is below l
  # this is only when you sample at that point
  expect_error(withr::with_preserve_seed(
    df <- nhppp_t_intensity(
      lambda = l,
      lambda_maj = c(2, -.1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)
})


test_that("sim_nhppp_intensity_piecewise() works", {
  l <- function(t) {
    return(rep(2, length(t)))
  }
  times <- c(0, 1, pi, 2 * pi, 10)
  lambda_maj <- c(2, 2.1, 2.9, 4)
  S <- methods::new("rstream.mrg32k3a")

  expect_no_error(withr::with_preserve_seed(df <- nhppp_t_intensity_piecewise(lambda = l, times_vector = times, lambda_maj_vector = lambda_maj)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)

  # works when range_t[1]>0
  times2 <- c(0, 1, pi, 2 * pi, 10) / 2 + 5
  expect_no_error(withr::with_preserve_seed(df <- nhppp_t_intensity_piecewise(lambda = l, times_vector = times2, lambda_maj_vector = lambda_maj)))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10)

  # works with only1 = TRUE
  expect_no_error(
    withr::with_preserve_seed(
      df1 <- nhppp_t_intensity_piecewise(
        lambda = l, times_vector = times2, lambda_maj_vector = lambda_maj, only1 = TRUE
      )
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 5, t_max = 10, only1 = TRUE)

  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(df2 <- nhppp_t_intensity_piecewise(lambda = l, times_vector = times2, lambda_maj_vector = lambda_maj, rng_stream = S)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10)
})
