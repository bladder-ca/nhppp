test_that("sim_nhppp_t_order_stats() works", {
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }
  S <- methods::new("rstream.mrg32k3a")

  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_order_stats(Lambda = L)))
  expect_no_error(withr::with_preserve_seed(df <- sim_nhppp_t_order_stats(Lambda = L)))
  expect_true(max(df) <= 10)



  # works when Lambda_inv() is given
  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_order_stats(Lambda = L, Lambda_inv = Li)))
  # works when range_t[1]>0
  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_order_stats(Lambda = L, Lambda_inv = Li, range_t = c(5, 10))))

  # works with rstream generator
  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_order_stats(Lambda = L, Lambda_inv = Li, range_t = c(5, 10), rng_stream = S)))

  # works with only1=TRUE
  expect_no_error(withr::with_preserve_seed(sim_nhppp_t_order_stats(Lambda = L, Lambda_inv = Li, range_t = c(5, 10), rng_stream = S, only1 = TRUE)))

  rm(list = c("L", "Li", "S"))
})
