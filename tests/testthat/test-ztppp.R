test_that("ztppp() arguments work", {
  set.seed(123)
  expect_no_error(df0 <- ztppp(rate = 1, t_min = 0, t_max = 10, atmost1 = TRUE))
  check_ppp_sample_validity(times = df0, t_min = 0, t_max = 10, atleast1 = TRUE, atmost1 = TRUE)

  expect_no_error(df1 <- ztppp(rate = 1, t_min = 0, t_max = 10))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, atleast1 = TRUE)
})
