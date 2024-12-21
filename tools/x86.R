devtools::dev_mode()
devtools::clean_dll()
devtools::load_all()

library(data.table)
library(nhppp)
library(Kystis)

remove_lecuyer()

S <- Simulator$new(
    target_population_size = 100,
    recipe = SEER_recipe_white_men,
    simulate_at_initialization = FALSE
)

design <- cbind(
    point_num = 1,
    value_coarse = NA_real_,
    value_T = NA_real_,
S$extractSimulationParameters()
)

C <- Calibrator$new(simulator = S, design = design)
C$simulateDesignPoints(dp_indices = 1, save_folder = tempdir())

remove_lecuyer()
  


