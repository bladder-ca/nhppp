test_that("draw_cumulative_intensity() works", {
  # This function is pass through to the draw_cumulative_intensity_inversion() function
  L <- function(t) {
    return(2 * t)
  }
  Li <- function(z) {
    return(z / 2)
  }
  withr::with_seed(12345, df1 <- draw_cumulative_intensity(Lambda = L, Lambda_inv = Li, t_min = 0, t_max = 1))
  check_ppp_sample_validity(times = df1, t_min = 0, t_max = 1)
})
