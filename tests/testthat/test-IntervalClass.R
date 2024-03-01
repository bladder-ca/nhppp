test_that("new_Interval() works", {
  I1 <- c(1, 2)
  expect_no_error(interval <- new_Interval(I1))
  expect_false(attr(interval, "vectorized"))

  Imat1 <- matrix(I1, nrow = 1)
  expect_no_error(interval <- new_Interval(Imat1))
  expect_true(attr(interval, "vectorized"))

  Imat2 <- Imat1[rep(1, 10),]
  expect_no_error(interval <- new_Interval(Imat2))
  expect_true(attr(interval, "vectorized"))
})

test_that("validate_Interval() works", {
  I1 <- c(1, 2)
  expect_no_error(validate_Interval(new_Interval(I1)))
  expect_error(validate_Interval(new_Interval(rep(I1, 2))))
  expect_error(validate_Interval(new_Interval(c(1, NA))))
  expect_error(validate_Interval(new_Interval(c(1, NaN))))
  expect_error(validate_Interval(new_Interval(c(2, 1))))

  Imat1 <- Imat1_na <- Imat1_nan <-matrix(I1, nrow = 1)
  expect_no_error(validate_Interval(new_Interval(Imat1)))
  expect_error(validate_Interval(new_Interval(cbind(Imat1, Imat1))))
  Imat1_na[1,2] <- NA
  expect_error(validate_Interval(new_Interval(Imat1_na)))
  Imat1_nan[1,2] <- NaN
  expect_error(validate_Interval(new_Interval(Imat1_nan)))
  expect_error(validate_Interval(new_Interval(Imat1[,c(2, 1)])))

  Imat2 <- Imat2_na <- Imat2_nan <-Imat1[rep(1, 10),]
  expect_no_error(validate_Interval(new_Interval(Imat2)))
  expect_error(validate_Interval(new_Interval(cbind(Imat2, Imat2))))
  Imat2_na[10,2] <- NA
  expect_error(validate_Interval(new_Interval(Imat2_na)))
  Imat2_nan[10,2] <- NaN
  expect_error(validate_Interval(new_Interval(Imat2_nan)))
  expect_error(validate_Interval(new_Interval(Imat2[,c(2, 1)])))
})

