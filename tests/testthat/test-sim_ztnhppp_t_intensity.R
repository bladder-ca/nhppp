test_that("ztnhppp_t_intensity() works", {
  l <- function(t) {
    return(2)
  }
  lmaj <- 2.2
  S <- methods::new("rstream.mrg32k3a")

  expect_no_error(withr::with_preserve_seed(df <- ztnhppp_t_intensity(lambda = l)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, zero_truncated = TRUE)

  expect_no_error(withr::with_preserve_seed(df <- ztnhppp_t_intensity(lambda = l, lambda_maj = lmaj)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, zero_truncated = TRUE)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df <- ztnhppp_t_intensity(lambda = l, lambda_maj = lmaj, range_t = c(5, 10))))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, zero_truncated = TRUE)

  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(df2 <- ztnhppp_t_intensity(lambda = l, lambda_maj = lmaj, range_t = c(5, 10), rng_stream = S)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10, zero_truncated = TRUE)

  # works with only1
  expect_no_error(withr::with_preserve_seed(df2 <- ztnhppp_t_intensity(lambda = l, lambda_maj = lmaj, range_t = c(5, 10), rng_stream = S, only1 = TRUE)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10, zero_truncated = TRUE, only1 = TRUE)
})

test_that("ztnhppp_t_intensity matches nhppp_t_cumulative_intensity_inversion (and linear intensity)", {
  l <- function(t) {
    return(2)
  }
  L <- function(t) {
    return(2 * t)
  }
  L_inv <- function(z) {
    return(z / 2)
  }

  lmaj <- 1.01
  S <- methods::new("rstream.mrg32k3a")

  r1 <- lapply(integer(10000), function(x) ztnhppp_t_intensity(lambda = l, range_t = c(0, 2), rng_stream = S, only1 = TRUE))
  r2 <- lapply(integer(10000), function(x) nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = L_inv, range_t = c(0, 2), rng_stream = S, only1 = TRUE))
  compare_ppp_vectors(ppp1 = unlist(r1), ppp2 = unlist(r2), threshold = 0.15, showQQ = TRUE)
})



test_that("ztnhppp_t_intensity() works with linear majorization function", {
  l <- function(t) {
    return(2)
  }
  S <- methods::new("rstream.mrg32k3a")

  # works with zero slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- ztnhppp_t_intensity(
      lambda = l,
      lambda_maj = c(2.2, 0),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, zero_truncated = TRUE)


  # works with negative slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- ztnhppp_t_intensity(
      lambda = l,
      lambda_maj = c(50, -1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, zero_truncated = TRUE)

  # works with positive slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- ztnhppp_t_intensity(
      lambda = l,
      lambda_maj = c(2, .1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, zero_truncated = TRUE)

  # fails when majorization function is below l
  # this is only when you sample at that point
  expect_error(withr::with_preserve_seed(
    df <- ztnhppp_t_intensity(
      lambda = l,
      lambda_maj = c(2, -.1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, zero_truncated = TRUE)
})





test_that("ztnhppp_intensity_piecewise() works", {
  l <- function(t) {
    return(2)
  }
  times <- c(0, 1, pi, 2 * pi, 10)
  lambda_maj <- c(2, 2.1, 2.9, 4)
  S <- methods::new("rstream.mrg32k3a")

  expect_no_error(withr::with_preserve_seed(df <- ztnhppp_t_intensity_piecewise(lambda = l, times_vector = times, lambda_maj_vector = lambda_maj)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, zero_truncated = TRUE)

  # works when range_t[1]>0
  times2 <- c(0, 1, pi, 2 * pi, 10) / 2 + 5
  expect_no_error(withr::with_preserve_seed(df <- ztnhppp_t_intensity_piecewise(lambda = l, times_vector = times2, lambda_maj_vector = lambda_maj)))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, zero_truncated = TRUE)

  # works with only1 = TRUE
  expect_no_error(
    withr::with_preserve_seed(
      df1 <- ztnhppp_t_intensity_piecewise(
        lambda = l, times_vector = times2, lambda_maj_vector = lambda_maj, only1 = TRUE
      )
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 5, t_max = 10, only1 = TRUE, zero_truncated = TRUE)

  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(df2 <- ztnhppp_t_intensity_piecewise(lambda = l, times_vector = times2, lambda_maj_vector = lambda_maj, rng_stream = S)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10, zero_truncated = TRUE)
})
