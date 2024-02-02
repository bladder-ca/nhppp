library(tidyverse)
dat <- readRDS("data/SEER_WM_50K_data.rds")
lambda_death <- readRDS("data/dt_intensities_death_from_other_causes.rds")

L <- lambda_death |>
  dplyr::filter(sex == "male" & race == "white" & cohort == 1940 & intensity == "Lambda") |>
  dplyr::filter(age >=40 & age <= 77) |>
  dplyr::select(age, estimate) |>
  tidyr::pivot_wider( names_from = age, values_from = estimate) |>
  as.matrix()
L_mat <- L[rep(1, 10000),]

Zmat <- nhppp::vdraw_sc_step_regular(Lambda_matrix = L_mat, range_t = c(40, 77), atmost1 = TRUE)



S <- RNGClass$new()
# we need to parallelize this:
draw_age_dead_other_causes_in_years(
  start_age_in_years = dat$SpawnAge[1],
  sex = dat$Sex[1],
  inception_calendar_year = as.numeric(format(dat$InceptionMoment[1], "%Y")),
  rng_stream = S)

