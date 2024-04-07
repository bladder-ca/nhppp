devtools::clean_dll()
devtools::load_all()


set.seed(123)
 lfun <- function(x, lambda_args, ...) .2 * x^lambda_args$exponent
  l_args <- list(exponent = 1L)
  l_ <- function(x) lfun(x, lambda_args = l_args)
  N <- 1000
  lmaj0 <- get_step_majorizer(fun = l_, breaks= matrix(rep(1:11, each = N), nrow = N), is_monotone = FALSE, K =0)
  lmaj1 <- get_step_majorizer(fun = l_, breaks= matrix(rep(1:11, each = N), nrow = N), is_monotone = FALSE, K =1)
  lmaj10 <- get_step_majorizer(fun = l_, breaks= matrix(rep(1:11, each = N), nrow = N), is_monotone = FALSE, K =10)


lala <- vdraw_intensity_step_regular_cpp(lambda = lfun, lambda_args = l_args, lambda_maj_matrix = lmaj0, range_t = c(1, 5), tol = 10^-6,     atmost1 = FALSE)

Z <- vdraw_intensity_step_regular_R(
  lambda = function(x, lambda_args = NULL) 0.1 * x,
  lambda_maj_matrix = matrix(rep(1, 5), nrow = 1), 
  range_t = c(1,10)
 )


#library(tidyverse)
dat <- readRDS("refactor/SEER_WM_50K_data.rds")
lambda_death <- readRDS("refactor/dt_intensities_death_from_other_causes.rds")



L <- matrix((1:110)/110, nrow =1)

n_people <- 10000
L_mat <- L[rep(1, n_people),]
tictoc::tic()
death_other_causes0 <- nhppp::vdraw_sc_step_regular_cpp(
  Lambda_matrix = L_mat,
  range_t = c(0, 110),
  atmost1 = TRUE,
  subinterval = c(0, 110))
tictoc::toc()



tictoc::tic()
death_other_causes1 <- nhppp::vztdraw_sc_step_regular_cpp(
  Lambda_matrix = L_mat,
  range_t = c(0, 110),
  atmost1 = TRUE,
  subinterval = c(0, 110))
tictoc::toc()




death_other_causes <- nhppp::vztdraw_sc_step_regular_cpp(Lambda_matrix = L_mat,
                                                     range_t = c(40, 110),
                                                     atmost1 = TRUE)



l_args <- list(trunc_age = 80, jump_age = 60)
lesion_intensity_function <- function(age, lambda_args,...){
   exp(-10 + 0.08 * age * as.numeric(age < lambda_args$trunc_age) +
         0.08 * lambda_args$trunc_age * as.numeric(age >= lambda_args$trunc_age) +
         0.5 * as.numeric(age >= lambda_args$jump_age))
}


n_lambda_intervals <- 20
l_maj_mat <- matrix(NA, ncol = n_lambda_intervals, nrow = n_people)
for(r in 1:n_people) {
  t_steps <- seq(40, death_other_causes[r,1], length.out = n_lambda_intervals+1)
  l_maj_mat[r,] <- pmax(lesion_intensity_function(t_steps[1:n_lambda_intervals], lambda_args = l_args),
                      lesion_intensity_function(t_steps[2:(n_lambda_intervals+1)], lambda_args = l_args))
}

# lesion_times <- vztdraw_intensity_step_regular(
#   lambda = lesion_intensity_function,
#   lambda_args = l_args,
#   lambda_maj_matrix = l_maj_mat,
#   range_t = cbind(rep(40, n_people), death_other_causes)
# )

# table(is.na(lesion_times))

# for(i in 1:5) {
#   tictoc::tic()
#   death_other_causes <- nhppp::vztdraw_sc_step_regular_cpp(Lambda_matrix = L_mat,
#                                                            range_t = c(lesion_times[,1], 110),
#                                                            atmost1 = TRUE)

#   lesion_times <- vztdraw_intensity_step_regular(
#     lambda = lesion_intensity_function,
#     lambda_args = l_args,
#     lambda_maj_matrix = l_maj_mat,
#     range_t = cbind(rep(40, n_people), death_other_causes)
#   )
#   tictoc::toc()
# }


