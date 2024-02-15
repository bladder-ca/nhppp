devtools::clean_dll()
devtools::load_all()


#library(tidyverse)
dat <- readRDS("refactor/SEER_WM_50K_data.rds")
lambda_death <- readRDS("refactor/dt_intensities_death_from_other_causes.rds")

L <- lambda_death |>
  dplyr::filter(sex == "male" & race == "white" & cohort == 1940 & intensity == "Lambda") |>
  dplyr::filter(age >=40 & age <= 77) |>
  dplyr::select(age, estimate) |>
  tidyr::pivot_wider(names_from = age, values_from = estimate) |>
  as.matrix()

n_people <- 10000
L_mat <- L[rep(1, n_people),]


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

lesion_times <- vztdraw_intensity_step_regular(
  lambda = lesion_intensity_function,
  lambda_args = l_args,
  lambda_maj_matrix = l_maj_mat,
  range_t = cbind(rep(40, n_people), death_other_causes)
)

table(is.na(lesion_times))

for(i in 1:5) {
  tictoc::tic()
  death_other_causes <- nhppp::vztdraw_sc_step_regular_cpp(Lambda_matrix = L_mat,
                                                           range_t = c(lesion_times[,1], 110),
                                                           atmost1 = TRUE)

  lesion_times <- vztdraw_intensity_step_regular(
    lambda = lesion_intensity_function,
    lambda_args = l_args,
    lambda_maj_matrix = l_maj_mat,
    range_t = cbind(rep(40, n_people), death_other_causes)
  )
  tictoc::toc()
}


