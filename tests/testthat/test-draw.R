test_that("draw() works with lambda option", {
  set.seed(123)
  l <- function(t) {
    return(2)
  }
  lmaj <- 2.2

  expect_no_error(withr::with_preserve_seed(df <- draw(lambda = l, lambda_maj = lmaj)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)

  expect_no_error(withr::with_preserve_seed(df <- draw(lambda = l, lambda_maj = lmaj, atleast1 = TRUE)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, atleast1 = TRUE)
})



test_that("draw() works with Lambda option", {
  set.seed(123)
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }

  expect_no_error(withr::with_preserve_seed(df <- draw(Lambda = L, Lambda_inv = Li)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10)

  expect_no_error(withr::with_preserve_seed(df <- draw(Lambda = L, Lambda_inv = Li, atleast1 = TRUE)))
  check_ppp_sample_validity(times = df, t_min = 0, t_max = 10, atleast1 = TRUE)
})
