devtools::dev_mode()
devtools::clean_dll()
devtools::load_all()

library(data.table)
library(nhppp)
library(Kystis)


remove_lecuyer()
P <- PopulationClass$new(
      target_population_size = 1,
      recipe = SEER_recipe_white_men[1,]
    )
remove_lecuyer()


