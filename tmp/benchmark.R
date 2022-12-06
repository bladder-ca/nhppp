res10 <- 
bench::mark(
  sim_ppp_ct(10, 0, 10, 1e-06, F), 
  sim_nhppp_t_thinning(lambda = function(t)2*t, lambda_maj=20, range_t=c(0,10)), 
  sim_nhppp_t_thinning(lambda = function(t)2*t, lambda_maj=c(0.05,2), range_t=c(0,10)), 
  sim_nhppp_t_thinning_1(lambda = function(t)2*t, lambda_maj=20, range_t=c(0,10)), 
  sim_nhppp_ct_thinning(0,10,"l", 0, 20, 1e-06, F),
  sim_nhppp_ct_thinning(0,10,"l", 0, c(0.05, 2), 1e-06, F), 
  sim_nhppp_ct_thinning_1(0,10,"l", 0, 20, 1e-06, F), 
  reda::simEvent(rho= function(t)2*t, rhoMax = 20, origin =0, endTime = 10),
  check = F
)



res100 <- 
bench::mark(
  sim_ppp_ct(100, 0, 100, 1e-06, F), 
  sim_nhppp_t_thinning(lambda = function(t)2*t, lambda_maj=200, range_t=c(0,100)), 
  sim_nhppp_t_thinning(lambda = function(t)2*t, lambda_maj=c(0.05,2), range_t=c(0,100)), 
  sim_nhppp_t_thinning_1(lambda = function(t)2*t, lambda_maj=200, range_t=c(0,100)), 
  sim_nhppp_ct_thinning(0,100,"l", 0, 200, 1e-06, F),
  sim_nhppp_ct_thinning(0,100,"l", 0, c(0.05, 2), 1e-06, F), 
  sim_nhppp_ct_thinning_1(0,100,"l", 0, 200, 1e-06, F), 
  reda::simEvent(rho= function(t)2*t, rhoMax = 200, origin =0, endTime = 100),
  check = F
)


