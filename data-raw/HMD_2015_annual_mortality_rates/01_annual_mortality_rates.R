library(data.table)
annual_mortality_rate_dat <- fread("data_raw/HMD_2015_annual_mortality_rates/HMD_USA_Mx_2015.csv")
annual_mortality_rate_dat[, V1 := NULL]
colnames(annual_mortality_rate_dat) <- tolower(colnames(annual_mortality_rate_dat))
long_dat <- melt(annual_mortality_rate_dat,
  id.vars = c("year", "age"),
  measure.vars = c("female", "male", "total"),
  variable.name = "sex",
  value.name = "lambda"
)
annual_mortality_rates_2015 <- dcast(long_dat, year + sex ~ age, value.var = "lambda")
setnames(annual_mortality_rates_2015, "year", "birth_cohort")
setnames(annual_mortality_rates_2015, as.character(0:110), c(paste0("age_", 0:109), "age_110+"))
# saveRDS(annual_mortality_rates_2015, "HMD_2015_annual_mortality_rates.rds")
rm(list = c("annual_mortality_rate_dat", "long_dat"))


usethis::use_data(
  annual_mortality_rates_2015,
  internal = FALSE,
  overwrite = TRUE
)
