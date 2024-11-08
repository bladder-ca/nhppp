devtools::clean_dll()
devtools::load_all()

set.seed(20241017)

library(data.table)
library(nhppp)

K <- 10^5
pop <- data.table(id  = 1:K,
                  birth_cohort = 2015,
                  spawn_age = 40,
                  max_simulation_age = 110,
                  sex = sample(c("male", "female"), K, replace = TRUE)
        )


annual_mortality_rates_2015[
  sex %in% c("male", "female"),
  c(1:5, 111:113)
]

rhos <- annual_mortality_rates_2015[
    pop,
    on = c("birth_cohort", "sex")
  ]
setindex(rhos, "id")
rho_matrix <- as.matrix(rhos[, c(paste0("age_", 0:109), "age_110+"),
                             with = FALSE])

rm(list = "rhos")   #cleanup

pop[,
    age_dead_from_other_causes:=
      nhppp::vdraw_sc_step_regular(
        lambda_matrix = rho_matrix,
        rate_matrix_t_min = 0,
        rate_matrix_t_max = 110,
        t_min = pop$spawn_age,          # 40
        t_max = pop$max_simulation_age, # 110
        atmost1 = TRUE,
        atleast1 = TRUE
      )
]


