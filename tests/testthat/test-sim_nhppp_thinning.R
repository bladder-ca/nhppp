test_that("sim_nhppp_thinning() works", {
  l <- function(t) {
    return(2)
  }
  lmaj <- 2.2
  S <- methods::new("rstream.mrg32k3a")

  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_thinning(lambda = l)))
  expect_no_error(withr::with_preserve_seed(df <- sim_nhppp_t_thinning(lambda = l, lambda_maj = lmaj)))
  expect_true(max(df) <= 10)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_thinning(lambda = l, lambda_maj = lmaj, range_t = c(5, 10))))

  # works with only1 = TRUE
  expect_no_error(
    withr::with_preserve_seed(
      df1 <- sim_nhppp_t_thinning(
        lambda = l,
        lambda_maj = lmaj,
        range_t = c(5, 10),
        only1 = TRUE
      )
    )
  )
  expect_true(length(df1) <= 1)


  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_thinning(lambda = l, lambda_maj = lmaj, range_t = c(5, 10), rng_stream = S)))
  rm(list = c("l", "lmaj", "S"))
})


test_that("sim_nhppp_thinning() works with linear majorization function", {
  l <- function(t) {
    return(2)
  }
  S <- methods::new("rstream.mrg32k3a")

  # works with zero slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- sim_nhppp_t_thinning(
      lambda = l,
      lambda_maj = c(2.2, 0),
      range_t = c(5, 10)))
  )
  expect_true(max(df) <= 10)


  # works with negative slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- sim_nhppp_t_thinning(
      lambda = l,
      lambda_maj = c(50, -1),
      range_t = c(5, 10)))
  )
  expect_true(max(df) <= 10)

  # works with positive slope majorization function
  expect_no_error(withr::with_preserve_seed(
    df <- sim_nhppp_t_thinning(
      lambda = l,
      lambda_maj = c(2, .1),
      range_t = c(5, 10)))
  )
  expect_true(max(df) <= 10)

  # fails when majorization function is below l
  # this is only when you sample at that point
  expect_error(withr::with_preserve_seed(
    sim_nhppp_t_thinning(
      lambda = l,
      lambda_maj = c(2, -.1),
      range_t = c(5, 10)))
  )


})

