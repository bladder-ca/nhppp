test_that("ppp_exactly_n() works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_exactly_n(n = 10, t_min = 0, t_max = 10)
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, size = 10)
})
