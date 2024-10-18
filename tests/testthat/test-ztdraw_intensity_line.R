test_that("ztdraw_intensity_line() works", {
  set.seed(123)
  l <- function(t) {
    return(2)
  }
  lmaj_intercept <- 2.2
  lmaj_slope <- 0

  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity_line(lambda = l, majorizer_intercept = lmaj_intercept, majorizer_slope = lmaj_slope, t_min = 0, t_max = 10)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, atleast1 = TRUE)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(df <- ztdraw_intensity_line(lambda = l, majorizer_intercept = lmaj_intercept, majorizer_slope = lmaj_slope, t_min = 5, t_max = 10)))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)

  # works with atmost1
  expect_no_error(withr::with_preserve_seed(df2 <- ztdraw_intensity_line(lambda = l, majorizer_intercept = lmaj_intercept, majorizer_slope = lmaj_slope, t_min = 5, t_max = 10, atmost1 = TRUE)))
  check_ppp_sample_validity(times = df2, t_min = 5, t_max = 10, atleast1 = TRUE, atmost1 = TRUE)
})

test_that("ztdraw_intensity_line() matches draw_cumulative_intensity_inversion() (and linear intensity)", {
  l <- function(t) {
    return(2)
  }
  L <- function(t) {
    return(2 * t)
  }
  L_inv <- function(z) {
    return(z / 2)
  }

  r1 <- lapply(integer(10000), function(x) ztdraw_intensity_line(lambda = l, majorizer_intercept = 2.01, majorizer_slope = 0, t_min = 0, t_max = 2, atmost1 = TRUE))
  r2 <- lapply(integer(10000), function(x) draw_cumulative_intensity_inversion(Lambda = L, Lambda_inv = L_inv, t_min = 0, t_max = 2, atmost1 = TRUE))
  compare_ppp_vectors(ppp1 = unlist(r1), ppp2 = unlist(r2), threshold = 0.20, showQQ = TRUE)
})



test_that("ztdraw_intensity_line() works with linear majorization function", {
  l <- function(t) {
    return(2)
  }


  # works with negative slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- ztdraw_intensity_line(
      lambda = l,
      majorizer_intercept = 50, majorizer_slope = -1,
      t_min = 5, t_max = 10
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)

  # works with positive slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- ztdraw_intensity_line(
      lambda = l,
      majorizer_intercept = 2, majorizer_slope = .1,
      t_min = 5, t_max = 10
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)

  # fails when majorization function is below l
  # this is only when you sample at that point
  expect_error(withr::with_preserve_seed(
    df <- ztdraw_intensity_line(
      lambda = l,
      majorizer_intercept = 2, majorizer_slope = -.1,
      t_min = 5, t_max = 10
    )
  ))
  check_ppp_sample_validity(times = df, t_min = 5, t_max = 10, atleast1 = TRUE)
})
