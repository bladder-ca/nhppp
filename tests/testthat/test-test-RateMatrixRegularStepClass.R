test_that("new_RateMatrixRegularStep() works", {
  l1  <- matrix(rep(1, 5), ncol = 5)
  L1  <- matrix(1:5, ncol = 5)
  expect_no_error(new_RateMatrixRegularStep(l1, FALSE))
  expect_no_error(new_RateMatrixRegularStep(L1, TRUE))
})

test_that("validate_RateMatrixRegularStep() works", {
  l1 <- l1_na <- l1_nan <-  l1_neg <- matrix(rep(1, 5), ncol = 5)
  expect_no_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1, FALSE)))
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(as.vector(l1), FALSE)))
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1)))
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1, NA)))
  l1_neg[1,3] <- -1
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1_neg, FALSE)))
  l1_na[1,3] <- NA
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1_na, FALSE)))
  l1_nan[1,3] <- NaN
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1_nan, FALSE)))
  l1_rev <- l1[, 5:1, drop = FALSE]
  expect_no_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1_rev, FALSE)))

  l1 <- l1_na <- l1_nan <-  l1_neg <- matrix(rep(1, 20), ncol = 5)
  expect_no_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1, FALSE)))
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1)))
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1, NA)))
  l1_neg[1,3] <- -1
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1_neg, FALSE)))
  l1_na[1,3] <- NA
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1_na, FALSE)))
  l1_nan[1,3] <- NaN
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1_nan, FALSE)))
  l1_rev <- l1[, 5:1, drop = FALSE]
  expect_no_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(l1_rev, FALSE)))

  L1 <- L1_na <- L1_nan <-  L1_neg <- matrix(1:5, ncol = 5)
  expect_no_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1, TRUE)))
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(as.vector(L1), TRUE)))
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1)))
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1, NA)))
  L1_neg[1,3] <- -1
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1_neg, TRUE)))
  L1_na[1,3] <- NA
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1_na, TRUE)))
  L1_nan[1,3] <- NaN
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1_nan, TRUE)))
  L1_rev <- L1[, 5:1, drop = FALSE]
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1_rev, TRUE)))

  L1 <- L1_na <- L1_nan <-  L1_neg <- matrix(rep(1:5, each = 5), ncol = 5)
  expect_no_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1, TRUE)))
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1)))
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1, NA)))
  L1_neg[1,3] <- -1
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1_neg, TRUE)))
  L1_na[1,3] <- NA
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1_na, TRUE)))
  L1_nan[1,3] <- NaN
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1_nan, TRUE)))
  L1_rev <- L1[, 5:1, drop = FALSE]
  expect_error(validate_RateMatrixRegularStep(new_RateMatrixRegularStep(L1_rev, TRUE)))
})


