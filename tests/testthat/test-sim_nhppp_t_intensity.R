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

#
# test_that("sim_nhppp_ct_thinning() works", {
#   expect_no_error(
#     df <- sim_nhppp_ct_thinning(
#       t_min = 1,
#       t_max = 8,
#       l_str = "l",
#       l_params = 0,
#       l_maj_params = c(16, 0),
#       only1 = FALSE
#     )
#   )
#   expect_true(max(df) <= 10 & length(df) >= 1)
#
#   expect_no_error(
#     df <- sim_nhppp_ct_thinning(
#       t_min = 1,
#       t_max = 8,
#       l_str = "l",
#       l_params = 0,
#       l_maj_params = c(16, 0),
#       only1 = TRUE
#     )
#   )
#   expect_true(max(df) <= 10 & length(df) == 1)
#
#   # positive slope
#   expect_no_error(
#     df <- sim_nhppp_ct_thinning(
#       t_min = 1,
#       t_max = 8,
#       l_str = "l",
#       l_params = 0,
#       l_maj_params = c(16, 0.1),
#       only1 = FALSE
#     )
#   )
#
#   # negative slope
#   expect_no_error(
#     df <- sim_nhppp_ct_thinning(
#       t_min = 1,
#       t_max = 8,
#       l_str = "l",
#       l_params = 0,
#       l_maj_params = c(24, -0.1),
#       only1 = FALSE
#     )
#   )
#
#   # Error in sim_nhppp_ct_thinning(t_min = 1, t_max = 8, l_str = "l", l_params = 0,  :
#   # The majorizing function is below the intensity function.
#   expect_error(
#     df <- sim_nhppp_ct_thinning(
#       t_min = 1,
#       t_max = 8,
#       l_str = "l",
#       l_params = 0,
#       l_maj_params = c(1, -0.1),
#       only1 = FALSE
#     )
#   )
#
#   # Lambda supremum rounding error
#   expect_no_error(replicate(
#     10^4,
#     nhppp_t_intensity(function(t) 2 * t)
#   ))
# })
