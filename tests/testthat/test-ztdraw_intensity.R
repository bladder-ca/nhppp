test_that("ztdraw_intensity() works", {
  set.seed(123)
  l <- function(t) {
    return(2)
  }
  lmaj <- 2.2
  S <- methods::new("rstream.mrg32k3a")

  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity(lambda = l)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, atleast1 = TRUE)

  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity(lambda = l, lambda_maj = lmaj)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, atleast1 = TRUE)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity(lambda = l, lambda_maj = lmaj, range_t = c(5, 10))))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)

  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(df2 <- ztdraw_intensity(lambda = l, lambda_maj = lmaj, range_t = c(5, 10), rng_stream = S)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10, atleast1 = TRUE)

  # works with atmost1
  expect_no_error(withr::with_preserve_seed(df2 <- ztdraw_intensity(lambda = l, lambda_maj = lmaj, range_t = c(5, 10), rng_stream = S, atmost1 = TRUE)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10, atleast1 = TRUE, atmost1 = TRUE)
})

test_that("ztdraw_intensity() matches draw_cumulative_intensity_inversion() (and linear intensity)", {
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

  r1 <- lapply(integer(10000), function(x) ztdraw_intensity(lambda = l, range_t = c(0, 2), rng_stream = S, atmost1 = TRUE))
  r2 <- lapply(integer(10000), function(x) draw_cumulative_intensity_inversion(Lambda = L, Lambda_inv = L_inv, range_t = c(0, 2), rng_stream = S, atmost1 = TRUE))
  compare_ppp_vectors(ppp1 = unlist(r1), ppp2 = unlist(r2), threshold = 0.20, showQQ = TRUE)
})



test_that("ztdraw_intensity() works with linear majorization function", {
  l <- function(t) {
    return(2)
  }
  S <- methods::new("rstream.mrg32k3a")

  # works with zero slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- ztdraw_intensity(
      lambda = l,
      lambda_maj = c(2.2, 0),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)


  # works with negative slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- ztdraw_intensity(
      lambda = l,
      lambda_maj = c(50, -1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)

  # works with positive slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- ztdraw_intensity(
      lambda = l,
      lambda_maj = c(2, .1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)

  # fails when majorization function is below l
  # this is only when you sample at that point
  expect_error(withr::with_preserve_seed(
    df <- ztdraw_intensity(
      lambda = l,
      lambda_maj = c(2, -.1),
      range_t = c(5, 10)
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)
})
