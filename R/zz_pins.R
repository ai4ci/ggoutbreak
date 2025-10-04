# setup package internal pin board function ----
.pin_board = .singleton({
  folder = system.file("pkgdown/assets/data/", package = "ggoutbreak")
  if (dir.exists(folder)) {
    pins::board_folder(folder)
  } else {
    pins::board_url("https://ai4ci.github.io/ggoutbreak/data/")
  }
})

# package external data access functions ----

#' The England COVID-19 age stratified poisson model dataset
#'
#' This is the output of the following estimator, and is here to speed up some
#' examples:
#'
#' ```R
#' england_covid %>% ggoutbreak::poisson_locfit_model(window=14)
#' ```
#'
#' @returns
#' A dataframe containing the following columns:
#'
#' * class (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) - the class column
#' * time (as.time_period) - the time column
#' * incidence.fit (numeric) - the incidence.fit column
#' * incidence.se.fit (numeric) - the incidence.se.fit column
#' * incidence.0.025 (numeric) - the incidence.0.025 column
#' * incidence.0.05 (numeric) - the incidence.0.05 column
#' * incidence.0.25 (numeric) - the incidence.0.25 column
#' * incidence.0.5 (numeric) - the incidence.0.5 column
#' * incidence.0.75 (numeric) - the incidence.0.75 column
#' * incidence.0.95 (numeric) - the incidence.0.95 column
#' * incidence.0.975 (numeric) - the incidence.0.975 column
#' * growth.fit (numeric) - the growth.fit column
#' * growth.se.fit (numeric) - the growth.se.fit column
#' * growth.0.025 (numeric) - the growth.0.025 column
#' * growth.0.05 (numeric) - the growth.0.05 column
#' * growth.0.25 (numeric) - the growth.0.25 column
#' * growth.0.5 (numeric) - the growth.0.5 column
#' * growth.0.75 (numeric) - the growth.0.75 column
#' * growth.0.95 (numeric) - the growth.0.95 column
#' * growth.0.975 (numeric) - the growth.0.975 column
#'
#' Minimally grouped by: class (and other groupings allowed).
#'
#' 26790 rows and 20 columns
#'
#' @keywords datasets
#' @concept datasets
#' @export
england_covid_poisson_age_stratified = function() {
  pins::pin_read(.pin_board(), "england_covid_poisson_age_stratified")
}


#' #' The England COVID-19 age stratified proportion model dataset
#'
#' This is the output of the following estimator, and is here to speed up some
#' examples. The proportion represented here is the positive tests in this
#' age group, versus the positive tests in all age groups. It has to be
#' considered with respect to the overall population.
#'
#' ```R
#' england_covid %>% ggoutbreak::proportion_locfit_model(window=14)
#' ```
#'
#' @returns
#' A dataframe containing the following columns:
#'
#' * class (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) - the class column
#' * time (as.time_period) - the time column
#' * proportion.fit (numeric) - the proportion.fit column
#' * proportion.se.fit (numeric) - the proportion.se.fit column
#' * proportion.0.025 (numeric) - the proportion.0.025 column
#' * proportion.0.05 (numeric) - the proportion.0.05 column
#' * proportion.0.25 (numeric) - the proportion.0.25 column
#' * proportion.0.5 (numeric) - the proportion.0.5 column
#' * proportion.0.75 (numeric) - the proportion.0.75 column
#' * proportion.0.95 (numeric) - the proportion.0.95 column
#' * proportion.0.975 (numeric) - the proportion.0.975 column
#' * relative.growth.fit (numeric) - the relative.growth.fit column
#' * relative.growth.se.fit (numeric) - the relative.growth.se.fit column
#' * relative.growth.0.025 (numeric) - the relative.growth.0.025 column
#' * relative.growth.0.05 (numeric) - the relative.growth.0.05 column
#' * relative.growth.0.25 (numeric) - the relative.growth.0.25 column
#' * relative.growth.0.5 (numeric) - the relative.growth.0.5 column
#' * relative.growth.0.75 (numeric) - the relative.growth.0.75 column
#' * relative.growth.0.95 (numeric) - the relative.growth.0.95 column
#' * relative.growth.0.975 (numeric) - the relative.growth.0.975 column
#'
#' Minimally grouped by: class (and other groupings allowed).
#'
#' 26790 rows and 20 columns
#'
#' @keywords datasets
#' @concept datasets
#' @export
england_covid_proportion_age_stratified = function() {
  pins::pin_read(.pin_board(), "england_covid_proportion_age_stratified")
}


#' An example of the linelist output of the poisson model simulation with defined
#' $R_t$
#'
#' This is generated using the `test_ip` infectivity profile
#'
#' @return
#' A dataframe containing the following columns:
#'
#' * time (as.time_period) - the time column
#' * rt (numeric) - the time varying rt column (parameters)
#' * imports (numeric) - the imports column
#' * rate (numeric) - the poisson rate column (underlying infection rate)
#' * count (integer) - the count column
#' * statistic (character) - the statistic column
#'
#' 81 rows and 6 columns
#'
#' @keywords datasets
#' @concept datasets
#' @export
test_poisson_rt = function() {
  pins::pin_read(.pin_board(), "test_poisson_rt")
}
