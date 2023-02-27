
l <- function(x) 2*x
L <- function(x) x^2
Li <- function(z) sqrt(z)


res <- bench::mark(
	"ppp_n (1)" = ppp_n(size = 1, range_t = c(0,10)),
	"ppp_n (100)" = ppp_n(size = 100, range_t = c(0,10)),

	"ppp_t (1st)" = ppp_t(rate = 1, range_t = c(0,10), only1=TRUE),
	"ppp_t (All)" = ppp_t(rate = 1, range_t = c(0,10)),

    "ppp_t_orderstat (1st)" = ppp_t_orderstat(rate =1, range_t=c(0,10), only1=TRUE),
	"ppp_t_orderstat (All)" = ppp_t_orderstat(rate=1, range_t=c(0,10)),

	"nhppp_n_intensity_linear (1)" = nhppp_n_intensity_linear(size = 1,alpha = 0, beta = 2, range_t = c(0,10)),
	"nhppp_n_intensity_linear (100)" = nhppp_n_intensity_linear(size = 100, alpha = 0, beta = 2, range_t = c(0,10)),

	"nhppp_t_intensity_linear (1st)" = nhppp_t_intensity_linear(alpha = 0, beta = 2, range_t = c(0,10), only1 = TRUE),
	"nhppp_t_intensity_linear (All)" = nhppp_t_intensity_linear(alpha = 0, beta = 2, range_t = c(0,10)),

	"nhppp_n_intensity (1, constmaj)" = nhppp_n_intensity(size = 1, lambda = l, lambda_maj = c(20, 0), range_t = c(0,10)),
	"nhppp_n_intensity (100, constmaj)" = nhppp_n_intensity(size = 100, lambda = l, lambda_maj = c(20, 0), range_t = c(0,10)),
	"nhppp_n_intensity (1, linemaj)" = nhppp_n_intensity(size = 1, lambda = l, lambda_maj = c(0, 2), range_t = c(0,10)),
	"nhppp_n_intensity (100, linemaj)" = nhppp_n_intensity(size = 100, lambda = l, lambda_maj = c(0, 2), range_t = c(0,10)),

	"nhppp_t_intensity (1st, constmaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(20, 0), range_t = c(0,10), only1 = TRUE),
	"nhppp_t_intensity (100, constmaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(20, 0), range_t = c(0,10)),
	"nhppp_t_intensity (1st, linemaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = c(0,10), only1 = TRUE),
	"nhppp_t_intensity (100, linemaj)" = nhppp_t_intensity(lambda = l, lambda_maj = c(0, 2), range_t = c(0,10)),

	"nhppp_n_cumulative_intensity (1, worst)" = nhppp_n_cumulative_intensity(size = 1, Lambda = L, Lambda_inv = NULL, range_t = c(0, 10)),
	"nhppp_n_cumulative_intensity (100, worst)" = nhppp_n_cumulative_intensity(size = 100, Lambda = L, Lambda_inv = NULL, range_t = c(0, 10)),
	"nhppp_n_cumulative_intensity (1, best)" = nhppp_n_cumulative_intensity(size = 1, Lambda = L, Lambda_inv = Li, range_t = c(0, 10)),
	"nhppp_n_cumulative_intensity (100, best)" = nhppp_n_cumulative_intensity(size = 100, Lambda = L, Lambda_inv = Li, range_t = c(0, 10)),

	"nhppp_t_cumulative_intensity_inversion (1st, worst)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = NULL, range_t = c(0, 10), only1 = TRUE),
	"nhppp_t_cumulative_intensity_inversion (All, worst)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = NULL, range_t = c(0, 10)),
	"nhppp_t_cumulative_intensity_inversion (1st, best)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = c(0, 10), only1 = TRUE),
	"nhppp_t_cumulative_intensity_inversion (All, best)" = nhppp_t_cumulative_intensity_inversion(Lambda = L, Lambda_inv = Li, range_t = c(0, 10)),

    "nhppp_t_cumulative_intensity_orderstats (1st, worst)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = NULL, range_t = c(0, 10), only1 = TRUE),
	"nhppp_t_cumulative_intensity_orderstats (All, worst)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = NULL, range_t = c(0, 10)),
	"nhppp_t_cumulative_intensity_orderstats (1st, best)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, range_t = c(0, 10), only1 = TRUE),
	"nhppp_t_cumulative_intensity_orderstats (All, best)" = nhppp_t_cumulative_intensity_orderstats(Lambda = L, Lambda_inv = Li, range_t = c(0, 10)),

	check=FALSE,
	min_iterations = 500
	)

autoplot(res, type="violin")
# microbenchmark::microbenchmark(
#  sim_nhppp_ct_inv(0, 10, "L", "Linv", TRUE),
#  sim_nhppp_ct_inv(0, 10, "L", "Linv", FALSE)[1],
#  sim_nhppp_ct_thinning(0, 10, 20, "l", TRUE),
#  sim_nhppp_ct_thinning(0, 10, 20, "l", FALSE)[1],
#  times = 10000
# )
