#' Human mortality database age and sex specific rates for all cause deaths
#'
#' This is the 2015 annual death rates from the 2023 version of the
#' Human Mortality Database for the USA.
#'
#' @keywords internal
#' @format  ## `annual_mortality_rates_2015`
#' a data.table with 3 rows and 113 columns.
#' \describe{
#'    \item{birth_cohort}{Birth cohort as a calendar year}
#'    \item{sex}{sex: `female`, `male`, `total`.}
#'    \item{age_0, ..., age 110+}{age-specific death rates}
#' }
#' @source <https://www.mortality.org>
"annual_mortality_rates_2015"
