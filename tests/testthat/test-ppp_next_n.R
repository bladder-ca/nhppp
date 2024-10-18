test_that("ppp_next_n() works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_next_n(n = 10, rate = 1, t_min = 5)
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 5, size = 10)
})
