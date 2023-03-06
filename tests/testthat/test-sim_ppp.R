test_that("ppp_next_n works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_next_n(n = 10, rate = 1, t_min = 5)
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, size = 10)
  expect_identical(withr::with_seed(12345, cumsum(rng_stream_rexp(size = 10, rate = 1)) + 5), df1)

  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_next_n(n = 10, rate = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, size = 10)

  # check RNGClass
  expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  expect_no_error(df3 <- ppp_next_n(n = 10, rate = 1, rng_stream = S2))
  check_ppp_sample_validity(times = df3, t_min = 0, size = 10)
})


test_that("ppp_t() works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_t(range_t = c(0, 10), rate = 1)
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  withr::with_seed(12345, df2 <- cumsum(rng_stream_rexp(size = 10, rate = 1)))
  expect_identical(df2[df2 <= 10], df1)

  # check only1 = T
  withr::with_seed(12345, df3 <- ppp_t(range_t = c(0, 10), rate = 1, only1 = TRUE))
  expect_identical(df3, df1[1])

  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_t(range_t = c(0, 10), rate = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10)


  # check RNGClass
  expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  expect_no_error(df3 <- ppp_t(range_t = c(0, 10), rate = 1, rng_stream = S2))
  check_ppp_sample_validity(times = df3, t_min = 0, t_max = 10)
})




test_that("ppp_n() works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_n(size = 10, range_t = c(0, 10))
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10, size = 10)
  # check rstream
  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_n(size = 10, range_t = c(0, 10), rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10, size = 10)

  # check RNGClass
  expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  expect_no_error(df3 <- ppp_n(size = 10, range_t = c(0, 10), rng_stream = S2))
  check_ppp_sample_validity(times = df3, t_min = 0, t_max = 10, size = 10)
})


test_that("ppp_t_orderstat() works", {
  expect_no_error(
    withr::with_seed(
      12345,
      df1 <- ppp_t_orderstat(range_t = c(0, 10), rate = 1)
    )
  )
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 10)

  # check rstream

  S1 <- methods::new("rstream.mrg32k3a")
  expect_no_error(df2 <- ppp_t_orderstat(range_t = c(0, 10), rate = 1, rng_stream = S1))
  check_ppp_sample_validity(times = df2, t_min = 0, t_max = 10)

  # check RNGClass
  expect_no_error(S2 <- readRDS(file.path("example_RNGCLass.rds"))$unpack())
  expect_no_error(df3 <- ppp_t_orderstat(range_t = c(0, 10), rate = 1, rng_stream = S2))
  check_ppp_sample_validity(times = df3, t_min = 0, t_max = 10)
})

# test_that("sim_ppp_cn() works", {
#   expect_no_error(
#     df <- sim_ppp_cn(
#       1, # rate
#       30, # n
#       1 # t_min
#     )
#   )
#   expect_true(length(df) == 30)
# })
#
# test_that("sim_ppp_ct() works", {
#   expect_no_error(
#     df <- sim_ppp_ct(
#       100, # rate
#       3, # t_min
#       6, # t_max
#       10^-6, # tol
#       FALSE # only1
#     )
#   )
#   expect_true(max(df) <= 6)
#
#   expect_no_error(
#     df <- sim_ppp_ct(
#       100, # rate
#       3, # t_min
#       6, # t_max
#       10^-6, # tol
#       TRUE # only1
#     )
#   )
#   expect_true(length(df) == 1)
# })
