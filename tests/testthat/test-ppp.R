test_that("ppp() works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp(rate = 1, t_min = 0, t_max = 10)
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  # check atmost1 = TRUE
  withr::with_seed(12345, df2 <- ppp(rate = 1, t_min = 0, t_max = 10, atmost1 = TRUE))
  expect_identical(df2, df1[1])
})
