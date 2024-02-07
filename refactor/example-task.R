library(tidyverse)
dat <- readRDS("refactor/SEER_WM_50K_data.rds")
lambda_death <- readRDS("refactor/dt_intensities_death_from_other_causes.rds")

L <- lambda_death |>
  dplyr::filter(sex == "male" & race == "white" & cohort == 1940 & intensity == "Lambda") |>
  dplyr::filter(age >=40 & age <= 77) |>
  dplyr::select(age, estimate) |>
  tidyr::pivot_wider( names_from = age, values_from = estimate) |>
  as.matrix()

n_people <- 10000
L_mat <- L[rep(1, n_people),]


death_other_causes <- nhppp::vztdraw_sc_step_regular_cpp(Lambda_matrix = L_mat,
                                                     range_t = c(40, 77),
                                                     atmost1 = TRUE)



### Maybe this code is buggy

lesion_intensity_function <- function(age, extra_args=list(center_age=40)) {
  0.01*(age-extra_args$center_age)^2
}
n_lambda_intervals <- 20
l_maj_mat <- matrix(NA, ncol = n_lambda_intervals, nrow = n_people)
for(r in 1:n_people) {
  t_steps <- seq(40, death_other_causes[r,1], length.out = n_lambda_intervals+1)
  l_maj_mat[r,] <- pmax(lesion_intensity_function(t_steps[1:n_lambda_intervals]),
                      lesion_intensity_function(t_steps[2:(n_lambda_intervals+1)]))
}

lesion_times <- vdraw_intensity_step_regular(
  lambda = lesion_intensity_function,
  lambda_maj_matrix = l_maj_mat,
  range_t = cbind(rep(40, n_people), death_other_causes)
)
lesion_times
