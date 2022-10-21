# do not run
if(FALSE) {
  # a simple function: l=2t, L=t^2, Linv(x) = sqrt(x)
  microbenchmark::microbenchmark(
    sim_nhppp_ct_inv(0, 10, "L", "Linv", TRUE),
    sim_nhppp_ct_inv(0, 10, "L", "Linv", FALSE)[1],
    sim_nhppp_ct_thinning(0, 10, 20, "l", TRUE),
    sim_nhppp_ct_thinning(0, 10, 20, "l", FALSE)[1],
    times = 1000)

  # the vignette function
  l_vignette_R <- function(t, r=2) {
    exp(r*t) * (1 + sin(t));
  }

  comps <- microbenchmark::microbenchmark(
    thinning_R_only1 = des.sim.functions::sim_nhppp_t_thinning(
      lambda = l_vignette_R,
      lambda_max = 10,
      range_t =c(0, 2*pi),
      only1 = TRUE),
    thinning_R_all = des.sim.functions::sim_nhppp_t_thinning(
      lambda = l_vignette_R,
      lambda_max = 10,
      range_t =c(0, 2*pi),
      only1 = FALSE),
    thinning_Cpp_only1 = sim_nhppp_ct_thinning(0, 2*pi, 10, "l_vignette", TRUE),
    thinning_Cpp_all = sim_nhppp_ct_thinning(0, 2*pi, 10, "l_vignette", FALSE),
    times = 1000)

  the_plot <- ggplot2::autoplot(comps)

  saveRDS(list(the_ggplot = the_plot, results= comps), "./tmp/RvsCpp_results.rds")
}
