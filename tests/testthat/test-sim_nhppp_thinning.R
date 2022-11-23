test_that("sim_nhppp_thinning() works", {
  l <- function(t) {
    return(2)
  }
  lmax <- 2
  S <- methods::new("rstream.mrg32k3a")

  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_thinning(lambda = l)))
  expect_no_error(withr::with_preserve_seed(df <- sim_nhppp_t_thinning(lambda = l, lambda_max = lmax)))
  expect_true(max(df) <= 10)

  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_thinning(lambda = l, lambda_max = lmax, range_t = c(5, 10))))

  # works with only1 = TRUE
  expect_no_error(
    withr::with_preserve_seed(
      df1 <- sim_nhppp_t_thinning(
        lambda = l,
        lambda_max = lmax,
        range_t = c(5, 10),
        only1 = TRUE
      )
    )
  )
  expect_true(length(df1) <= 1)


  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_thinning(lambda = l, lambda_max = lmax, range_t = c(5, 10), rng_stream = S)))
  rm(list = c("l", "lmax", "S"))
})
