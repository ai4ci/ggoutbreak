## germany_covid definition ----

#' Weekly COVID-19 case counts by age group in Germany
#'
#' A dataset of the weekly count of COVID-19 cases by age group in Germany
#' downloaded from the Robert Koch Institute `Survstat` service, and formatted for
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
#' - time (time_period) - the time column
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
#' A dataset of the daily count of COVID-19 cases by age group in England
#' downloaded from the UKHSA coronavirus API, and formatted for
#' use in `ggoutbreak`. A denominator is calculated which is the overall
#' positive count for all age groups. This data set can be used to calculate
#' group-wise incidence and absolute growth rates and group wise proportions and
#' relative growth rates by age group.
#'
#' You may want `england_covid_test_positives` instead which includes the
#' population denominator. The denominator here is the total number of positive
#' tests across all age groups and not the number of tests taken or population
#' size.
#'
#' @usage data(england_covid)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (as.Date) - the date column
#' - class (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) - the class column
#' - count (numeric) - the test positives for each age group
#' - denom (numeric) - the test positives across all age groups
#' - time (time_period) - the time column
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
#' data is combined with testing rate as denominator, and test positives as
#' count for the whole of England.
#'
#' @usage data(england_covid_pcr_positivity)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (date) - a daily time series
#' - time (time_period) - the time column
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
#' sources (including with an early version of `ggoutbreak`).
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
#' Lower tier local authority level, and is weekly time series of counts per
#' variant. Variants were assigned using the tree structure of the Pango
#' lineage. Different sub-lineages are aggregated to the major WHO variants of
#' concern.
#'
#' @usage data(england_variants)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (date) - the end date of the week
#' - time (time_period) - the time column
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
#' - time (time_period) - the time column
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 137 rows and 4 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_nhs_app
NULL

## england_nhs_app definition ends
## england_covid_proportion definition ----

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
#' * prevalence.0.5 (numeric) - the median proportion of people in the region testing
#'    positive for COVID-19
#' * prevalence.0.025 (numeric) - the lower CI of the proportion of people in the region testing
#'    positive for COVID-19
#' * prevalence.0.975 (numeric) - the upper CI of the proportion of people in the region testing
#'    positive for COVID-19
#' * denom (integer) - the sample size on which this estimate was made (daily rate inferred from
#'    weekly sample sizes.)
#' * time (time_period) - the time column
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 9820 rows and 7 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_ons_infection_survey
NULL

## england_ons_infection_survey definition ends

## infectivity profiles

#' A COVID-19 infectivity profile based on an empirical resampling approach
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * boot (anything + default(1)) - a bootstrap identifier
#' * probability (proportion) - the probability of infection between previous time period until `time`
#' * time (double) - the end of the time period (in days)
#'
#' Must be grouped by: boot (exactly).
#'
#' @docType data
#' @concept datasets
#' @name covid_ip
NULL


#' A COVID-19 infectivity profile based on an Ganyani et al 2020
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * boot (anything + default(1)) - a bootstrap identifier
#' * probability (proportion) - the probability of infection between previous time period until `time`
#' * tau (double) - the time index this probability relates to (in days)
#' * a0 - the beginning of the time period
#' * a1 - the end of the time period
#'
#' Grouped by boot (exactly).
#'
#' @references \url{https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.17.2000257}
#' @docType data
#' @concept datasets
#' @name ganyani_ip
NULL

#' A COVID-19 infectivity profile based on an Ganyani et al 2020
#'
#' This version is discretised in a manner that makes it incompatible with
#' `EpiEstim`.
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * boot (anything + default(1)) - a bootstrap identifier
#' * probability (proportion) - the probability of infection between previous time period until `time`
#' * tau (double) - the time index this probability relates to (in days)
#' * a0 - the beginning of the time period
#' * a1 - the end of the time period
#'
#' Grouped by boot (exactly).
#'
#' @references \url{https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.17.2000257}
#' @docType data
#' @concept datasets
#' @name ganyani_ip_2
NULL


## covid_test_sensitivity definition ----

#' Test sensitivity of PCR tests
#'
#' The probability of detecting COVID using PCR given time since infection, based on
#' Binny et al 2023.
#'
#' @references \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9384503/}
#'
#' @usage data(covid_test_sensitivity)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * tau (numeric) - the time column
#' * probability (numeric) - the probability column
#' * boot (integer) - the boot column
#'
#' Must be grouped by: boot (and other groupings allowed).
#'
#' 5100 rows and 3 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name covid_test_sensitivity
NULL

## covid_test_sensitivity definition ends

## covid_infectivity_profile definition ends

## ganyani_infectivity_profile definition ends
## covid_ip definition ----

#' The COVID Infectivity Profile dataset
#'
#' An infectivity profile derived from a meta-analysis of serial intervals.
#'
#' @usage data(covid_ip)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * boot (anything + default(1)) - a bootstrap identifier
#' * probability (proportion) - the probability of infection between previous time period until `time`
#' * tau (numeric) - the time index this probability relates to (in days)
#' * a0 (numeric) - the beginning of the time period
#' * a1 (numeric) - the end of the time period
#'
#' Grouped by: boot.
#'
#' 1400 rows and 5 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name covid_ip
NULL

## covid_ip definition ends
## ganyani_ip definition ----

#' The Ganyani serial interval dataset, compatible with `EpiEstim`
#'
#' @usage data(ganyani_ip)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * boot (anything + default(1)) - a bootstrap identifier
#' * probability (proportion) - the probability of infection between previous time period until `time`
#' * tau (numeric) - the time index this probability relates to (in days)
#' * a0 (numeric) - the beginning of the time period
#' * a1 (numeric) - the end of the time period
#'
#' Grouped by: boot.
#'
#' 2400 rows and 5 columns
#'
#' @keywords datasets
#' @concept datasets
#' @name ganyani_ip
NULL

## ganyani_ip definition ends
## ganyani_ip_2 definition ----

#' The Ganyani serial interval dataset, incompatible with `EpiEstim`
#'
#' @usage data(ganyani_ip_2)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * boot (anything + default(1)) - a bootstrap identifier
#' * probability (proportion) - the probability of infection between previous time period until `time`
#' * tau (numeric) - the time index this probability relates to (in days)
#' * a0 (numeric) - the beginning of the time period
#' * a1 (numeric) - the end of the time period
#'
#' Grouped by: boot.
#'
#' 2800 rows and 5 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name ganyani_ip_2
NULL

## ganyani_ip_2 definition ends
## covid_viral_shedding definition ----

#' The COVID-19 viral shedding duration
#'
#' @references \url{https://www.nature.com/articles/s41467-020-20568-4}
#' From Von Kampen et al.
#'
#' @usage data(covid_viral_shedding)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * boot (anything + default(1)) - a bootstrap identifier
#' * probability (proportion) - the probability of infection between previous time period until `time`
#' * tau (numeric) - the time index this probability relates to (in days)
#' * a0 (numeric) - the beginning of the time period
#' * a1 (numeric) - the end of the time period
#'
#' Grouped by: boot.
#'
#' 2600 rows and 3 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name covid_viral_shedding
NULL

## covid_viral_shedding definition ends
## du_serial_interval_ip definition ----

#' The Du empirical serial interval dataset
#'
#' From Z. Du, X. Xu, Y. Wu, L. Wang, B. J. Cowling, and L. A. Meyers, ‘Serial Interval of COVID-19 among Publicly Reported Confirmed Cases’, Emerg Infect Dis, vol. 26, no. 6, pp. 1341–1343, Jun. 2020, doi: 10.3201/eid2606.200357.
#'
#' @usage data(du_serial_interval_ip)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * boot (anything + default(1)) - a bootstrap identifier
#' * probability (proportion) - the probability of infection between previous time period until `time`
#' * tau (numeric) - the time index this probability relates to (in days)
#' * a0 (numeric) - the beginning of the time period
#' * a1 (numeric) - the end of the time period
#'
#' Grouped by: boot.
#'
#' 2603 rows and 5 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name du_serial_interval_ip
NULL

## du_serial_interval_ip definition ends
## test_ip definition ----

#' A test infectivity profile generated from a set of discretised gamma
#' distributions with parameters mean 5 (95% CI 4-6) and sd 2 (95% CI 1.5-2.5).
#'
#' @usage data(test_ip)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * boot (anything + default(1)) - a bootstrap identifier
#' * probability (proportion) - the probability of infection between previous time period until `time`
#' * tau (numeric) - the time index this probability relates to (in days)
#' * a0 (numeric) - the beginning of the time period
#' * a1 (numeric) - the end of the time period
#'
#' Grouped by: boot.
#'
#' 2000 rows and 5 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name test_ip
NULL

## test_ip definition ends
## test_ts definition ----

#' A test time series dataset, containing no statistical noise.
#'
#' @usage data(test_ts)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * time (as.time_period) - the time column
#' * growth (numeric) - the growth column
#' * incidence (numeric) - the incidence column
#' * rt (numeric) - the rt column
#' * denom (numeric) - the denom column
#' * proportion (numeric) - the proportion column
#' * relative.growth (numeric) - the relative.growth column
#' * count (numeric) - the count column
#'
#' Any grouping allowed.
#'
#' 501 rows and 8 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name test_ts
NULL

## test_ts definition ends
## test_poisson_rt definition ----

#' An example of the linelist output of the poisson model simulation with defined
#' $R_t$
#'
#' This is generated using the `test_ip` infectivity profile
#'
#' @usage data(test_poisson_rt)
#'
#' @format
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
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name test_poisson_rt
NULL

## test_poisson_rt definition ends
## test_bpm definition ----

#' An example of the linelist output of the branching process model simulation
#'
#' This is generated using the `test_ip` infectivity profile and
#' also includes a delay to symptom onset which is a random gamma distributed
#' quantity with mean of 6 and standard deviation of 2
#'
#' @usage data(test_bpm)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * time (as.time_period) - the time column
#' * id (integer) - an id per individual
#' * generation_interval (numeric) - the generation_interval column
#' * infector (integer) - the infector id
#' * generation (numeric) - the generation column
#' * symptom_onset (logical) - the flag for onset of symptoms
#' * symptom_onset_delay (numeric) - the time to onset of symptoms from infection
#' * symptom_onset_time (as.time_period) - the time of symptom onset
#'
#' 333126 rows and 8 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name test_bpm
NULL

## test_bpm definition ends
## test_serial definition ----

#' A serial interval estimated from simulated data
#'
#' This serial interval is resampled from the first 1000 patients in the
#' `test_bpm` dataset for whom both infector and infectee has symptoms. These
#' patients are generated with a symptom delay of mean 6 days and SD 2 from
#' infection (discrete under-dispersed gamma) and an infectivity profile with
#' mean 5 days and SD 2 as defined in `test_ip` dataset. This serial interval is
#' relevant to the estimation of $R_t$ from symptomatic case counts in the
#' `test_bpm` dataset but includes negative times, and cannot be used with
#' `EpiEstim`.
#'
#' @usage data(test_serial)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * tau (numeric) - the time delay between symptoms in infector and infectee
#' * a0 (numeric) - the a0 column
#' * a1 (numeric) - the a1 column
#' * probability (numeric) - the probability column
#' * boot (integer) - the boot column
#'
#' Minimally grouped by: boot (and other groupings allowed).
#'
#' 2166 rows and 5 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name test_serial
NULL

## test_serial definition ends
## test_poisson_growth_rate definition ----

#' A simulation dataset determined by a step function of growth rates. This is
#' useful for demonstrating growth rate estimators.
#'
#' @usage data(test_poisson_growth_rate)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * time (as.time_period) - the time column
#' * growth (numeric) - the time varying growth rate column (input parameter)
#' * imports (numeric) - the imports column
#' * rate (numeric) - the poisson rate column
#' * count (integer) - the sampled count column
#' * statistic (character) - the statistic column (infections)
#'
#' Minimally grouped by: statistic (and other groupings allowed).
#'
#' 105 rows and 6 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name test_poisson_growth_rate
NULL

## test_poisson_growth_rate definition ends
## test_poisson_rt_smooth definition ----

#' Output of a poisson model simulation with a
#' smooth function for $R_t$ defined as `R(t) = e^(sin(t/80*pi)^4-0.25))`. This
#' is a relatively unchallenging test data set that should not pose a problem
#' for smooth estimators.
#'
#' This is generated using the central value of the `test_ip` infectivity profile
#'
#' @usage data(test_poisson_rt_smooth)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * time (as.time_period) - the time column
#' * rt (numeric) - the time varying rt column (parameters)
#' * imports (numeric) - the imports column
#' * rate (numeric) - the poisson rate column (underlying infection rate)
#' * count (integer) - the count column
#' * statistic (character) - the statistic column
#'
#' Minimally grouped by: statistic (and other groupings allowed).
#'
#' 161 rows and 6 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name test_poisson_rt_smooth
NULL

## test_poisson_rt_smooth definition ends
## test_poisson_rt_2class definition ----
## Generated code. remove this line to prevent manual changes being overwritten

#' The test_poisson_rt_2class dataset
#'
#' @usage data(test_poisson_rt_2class)
#'
#' @format
#' A dataframe containing the following columns:
#'
#' * time (as.time_period) - the time column
#' * rt (numeric) - the rt column
#' * imports (numeric) - the imports column
#' * rate (numeric) - the rate column
#' * count (integer) - the count column
#' * statistic (character) - the statistic column
#' * class (enum(`one`,`two`)) - the class column
#' * denom (integer) - the denom column
#'
#' Minimally grouped by: class (and other groupings allowed).
#'
#' 322 rows and 8 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name test_poisson_rt_2class
NULL

## test_poisson_rt_2class definition ends

## england_covid_poisson_age_stratified definition ----

#' The England COVID-19 age stratified poisson model dataset
#'
#' This is the output of the following estimator, and is here to speed up some
#' examples:
#'
#' ```R
#' england_covid %>% ggoutbreak::poisson_locfit_model(window=14)
#' ```
#'
#' @usage data(england_covid_poisson_age_stratified)
#'
#' @format
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
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_covid_poisson_age_stratified
NULL

## england_covid_poisson_age_stratified definition ends
## england_covid_poisson definition ----

#' The England COVID-19 poisson model dataset
#'
#' This is the output of the following estimator, and is here to speed up some
#' examples:
#'
#' ```R
#' england_covid %>% time_aggregate() %>% ggoutbreak::poisson_locfit_model(window=14)
#' ```
#'
#' @usage data(england_covid_poisson)
#'
#' @format
#' A dataframe containing the following columns:
#'
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
#' Any grouping allowed.
#'
#' 1410 rows and 19 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_covid_poisson
NULL

## england_covid_poisson definition ends
## england_covid_proportion_age_stratified definition ----

#' The England COVID-19 age stratified proportion model dataset
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
#' @usage data(england_covid_proportion_age_stratified)
#'
#' @format
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
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_covid_proportion_age_stratified
NULL

## england_covid_proportion_age_stratified definition ends

## england_covid_test_positives definition ----

#' Weekly England COVID test positives by age group including testing effort
#'
#' An age group stratified dataset from
#'
#' * the coronavirus.gov.uk site for positive cases aggregated to 10 year age
#' groups and by weekly time.
#' * NHS test and trace date which reported regional by age group testing
#' effort aggregated to country level.
#' * ONS 2021 census population aggregated to 10 year age groups.
#'
#' @usage data(england_covid_test_positives)
#'
#' @format
#' A dataframe containing the following columns:
#' - class (character) - the age group
#' - date (date) - the start date of a week
#' - count (numeric) - the count of COVID positives
#' - denom (numeric) - the number of COVID tests performed for that age group
#' that week
#' - population (numeric) - the size of the population in this age group
#' - time (time_period) - the time column (weekly)
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 1050 rows and 6 columns
#'
#' @docType data
#' @keywords datasets
#' @concept datasets
#' @name england_covid_test_positives
NULL

## england_covid_test_positives definition ends
