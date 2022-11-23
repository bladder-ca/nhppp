test_that("inverse_with_uniroot() works", {
  expect_equal(inverse_with_uniroot(f = function(x) {
    2 * x
  }, y = 0.5), 0.25)
})


test_that("vectorization inverse_with_uniroot_sorted() works", {
  expect_equal(inverse_with_uniroot_sorted(f = function(x) {
    2 * x
  }, y = c(0, 0.5)), c(0, 0.25))
})

### Add testst hat show how these functions break
