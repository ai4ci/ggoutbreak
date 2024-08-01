## germany_covid definition ----

#' Weekly COVID-19 case counts by age group in Germany
#'
#' A dataset of the weekly count of covid cases by age group in Germany
#' downloaded from the Robert Koch Institute Survstat service, and formatted for
#' use in growth rates. A denominator is calculated which is the overall
#' positive count for all age groups. This data set can be used to calculate
#' group-wise incidence and absolute growth rates and group wise proportions and
#' relative growth rates.
#'
#' @usage data(germany_covid)
#'
#' @format
#' A dataframe containing the following columns:
#' - class (enum(`0–14`,`15–19`,`20–24`,`25–29`,`30–39`,`40–49`,`50–59`,`60–69`,`70–79`,`80+`,`Unknown`, .ordered=TRUE)) - the age group
#' - date (as.Date) - the date column
#' - count (integer) - the test positives for each age group
#' - time (as.time_period) - the time column
#' - denom (integer) - the test positives for all age groups
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 2070 rows and 6 columns
#'
#' @docType data
#' @concept datasets
#' @keywords datasets
#' @name germany_covid
NULL

## germany_covid definition ends


## england_covid definition ----

#' Daily COVID-19 case counts by age group in England
#'
#' A dataset of the daily count of covid cases by age group in England
#' downloaded from the UKHSA coronavirus API, and formatted for
#' use in `growthrates`. A denominator is calculated which is the overall
#' positive count for all age groups. This data set can be used to calculate
#' group-wise incidence and absolute growth rates and group wise proportions and
#' relative growth rates.
#'
#' @usage data(england_covid)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (as.Date) - the date column
#' - class (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) - the class column
#' - count (numeric) - the test positives for each age group
#' - denom (numeric) - the test positives for all age groups
#' - time (as.time_period) - the time column
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 26790 rows and 5 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_covid
NULL

## england_covid definition ends


## germany_demographics definition ----

#' Germany demographics
#'
#' Derived from the Robert Koch Survstat service by comparing counts and
#' incidence rates.
#'
#' @usage data(germany_demographics)
#'
#' @format
#' A dataframe containing the following columns:
#' - class (enum(`0–14`,`15–19`,`20–24`,`25–29`,`30–39`,`40–49`,`50–59`,`60–69`,`70–79`,`80+`, .ordered=TRUE)) - the class column
#' - population (integer) - the population column
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 10 rows and 2 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name germany_demographics
NULL

## germany_demographics definition ends
## england_consensus_rt definition ----

#' The SPI-M-O England consensus reproduction number
#'
#' SPI-M-O used a range of different statistical and mechanistic models to
#' produce estimates of the  reproduction number of the epidemic from various data
#' sources.
#'
#' @usage data(england_consensus_rt)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (date) - the date of the estimate
#' - low (numeric) - the lower published estimate of the reproduction number
#' - high (numeric) - the higher published estimate of the reproduction number
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 113 rows and 3 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_consensus_rt
NULL

## england_consensus_rt definition ends

## england_events definition ----

#' Key dated in the COVID-19 response in England
#'
#' This includes mainly the dates of lockdowns, releases from social distancing
#' measures and the dates that new variants were first detected.
#'
#' @usage data(england_events)
#'
#' @format
#' A dataframe containing the following columns:
#' - label (character) - the event label
#' - start (date) - the event start date
#' - end (date) - the (optional) event end date
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 13 rows and 3 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_events
NULL

## england_events definition ends
## england_covid_pcr_positivity definition ----

#' England COVID-19 PCR test positivity
#'
#' The `coronavirus.gov.uk` dashboard published tests conducted and positive
#' results as separate data sets for a range of geographies. In this case the
#' data is combined with testing rate as denominator, and positives as count for
#' England.
#'
#' @usage data(england_covid_pcr_positivity)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (date) - a daily time series
#' - time (as.time_period) - the time column
#' - count (numeric) - test positives in England on that day
#' - denom (numeric) - total tests conducted on that day
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 1413 rows and 4 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_covid_pcr_positivity
NULL

## england_covid_pcr_positivity definition ends
## england_consensus_growth_rate definition ----

#' The SPI-M-O England consensus growth rate
#'
#' SPI-M-O used a range of different statistical and mechanistic models to
#' produce estimates of the growth rate of the epidemic from various data
#' sources (including with an early version of `growthrates`).
#'
#' @usage data(england_consensus_growth_rate)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (date) - the date of the estimate
#' - low (numeric) - the lower published estimate of the growth rate
#' - high (numeric) - the higher published estimate of the growth rate
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 111 rows and 3 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_consensus_growth_rate
NULL

## england_consensus_growth_rate definition ends
## england_demographics definition ----

#' England demographics
#'
#' Population counts by 5 year age group for England only from the 2021 census.
#'
#' @source https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021/census2021/census2021firstresultsenglandwales1.xlsx
#' @usage data(england_demographics)
#'
#' @format
#' A dataframe containing the following columns:
#' - class (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) - the class column
#' - population (numeric) - the population count column
#' - baseline_proportion (numeric) - the baseline proportion is the proportion
#'    this age group makes up of the total.
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 19 rows and 3 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_demographics
NULL

## england_demographics definition ends

## england_variants definition ----

#' Counts of COVID-19 variants
#'
#' Data from the COG-UK and Sanger centre sequencing
#' programme. The data were made available through the Welcome foundation at
#' Lower tier local authority level, and is weekly timeseries of counts per
#' variant. Variants were assigned using the tree structure of the Pango
#' lineage. Different sub-lineages are aggregated to the major WHO variants of
#' concern.
#'
#' @usage data(england_variants)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (date) - the end date of the week
#' - time (as.time_period) - the time column
#' - class (enum(`Other`,`Alpha (B.1.1.7)`,`Delta (B.1.617.2)`,`Delta (AY.4)`,`Omicron (Other)`,`Omicron (BA.2)`,`Omicron (BA.4)`,`Omicron (BA.5)`,`XBB (Other)`,`Kraken (XBB.1.5)`,`Arcturus (XBB.1.16)`,`Eris (EG.5.1)`)) - the class column
#' - who_class (enum(`Other`,`Alpha`,`Delta`,`Omicron`,`Kraken`,`Arcturus`,`Eris`)) - the who_class column
#' - count (numeric) - the weekly count column
#' - denom (numeric) - the number of sequences performed in that week
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 479 rows and 6 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_variants
NULL

## england_variants definition ends
## england_nhs_app definition ----

#' NHS COVID-19 app data
#'
#' check-in (social activity) and alerts (self isolation instruction) data from
#' the NHS COVID-19 app, aggregated to country level on a week by week basis.
#'
#' @usage data(england_nhs_app)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (date) - the start date of the week
#' - alerts (integer) - the count of self-isolation alerts
#' - visits (integer) - the number of venue check-ins representing visits to
#' social venues.
#' - time (as.time_period) - the time column
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 137 rows and 4 columns
#'
#' @docType data
#' @keywords datasets
#' @name england_nhs_app
NULL

## england_nhs_app definition ends
## england_covid_proportion definition ----

#' England COVID by age group for ascertainment
#'
#' An age group stratified dataset from
#'
#' * the coronavirus.gov.uk site for
#' positive cases aggregated to 10 year age groups and by weekly time.
#' * NHS test
#' and trace date which reported regional by age group testing effort aggregated
#' to country level.
#' * ONS 2021 census population aggregated to 10 year age
#' groups.
#'
#' @usage data(england_covid_proportion)
#'
#' @format
#' A dataframe containing the following columns:
#' - class (character) - the age group
#' - date (date) - the start date of a week
#' - count (numeric) - the count of COVID positives
#' - denom (numeric) - the number of COVID tests performed
#' - population (numeric) - the size of the population at this age group
#' - time (as.time_period) - the time column (weekly)
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 1050 rows and 6 columns
#'
#' @docType data
#' @keywords datasets
#' @name england_covid_proportion
NULL

## england_covid_proportion definition ends

## england_ons_infection_survey definition ----

#' The england_ons_infection_survey dataset
#'
#' The COVID-19 ONS infection survey took a random sample of the population
#' and provides an estimate of the prevalence of COVID-19 that is supposedly
#' free from ascertainment bias.
#'
#' The data is available here:
#' https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2023/20230310covid19infectionsurveydatasetsengland.xlsx
#'
#' @usage data(england_ons_infection_survey)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * date (date) - the date column
#' * geography (character) - the geography column
#' * proportion.0.5 (numeric) - the median proportion of people in the region testing
#'    positive for COVID-19
#' * proportion.0.025 (numeric) - the lower CI of the proportion of people in the region testing
#'    positive for COVID-19
#' * proportion.0.975 (numeric) - the upper CI of the proportion of people in the region testing
#'    positive for COVID-19
#' * denom (integer) - the sample size on which this estimate was made (daily rate inferred from
#'    weekly sample sizes.)
#' * time (as.time_period) - the time column
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 9820 rows and 7 columns
#'
#' @docType data
#' @keywords datasets
#' @name england_ons_infection_survey
NULL

## england_ons_infection_survey definition ends
