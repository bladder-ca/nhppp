

microbenchmark::microbenchmark(
  sim_nhppp_ct_inv(0, 10, "L", "Linv", TRUE),
  sim_nhppp_ct_inv(0, 10, "L", "Linv", FALSE)[1],
  sim_nhppp_ct_thinning(0, 10, 20, "l", TRUE),
  sim_nhppp_ct_thinning(0, 10, 20, "l", FALSE)[1],
  times = 10000
)
