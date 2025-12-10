# The england_ons_infection_survey dataset

The COVID-19 ONS infection survey took a random sample of the population
and provides an estimate of the prevalence of COVID-19 that is
supposedly free from ascertainment bias.

## Usage

``` r
data(england_ons_infection_survey)
```

## Format

A dataframe containing the following columns:

- date (date) - the date column

- geography (character) - the geography column

- prevalence.0.5 (numeric) - the median proportion of people in the
  region testing positive for COVID-19

- prevalence.0.025 (numeric) - the lower CI of the proportion of people
  in the region testing positive for COVID-19

- prevalence.0.975 (numeric) - the upper CI of the proportion of people
  in the region testing positive for COVID-19

- denom (integer) - the sample size on which this estimate was made
  (daily rate inferred from weekly sample sizes.)

- time (time_period) - the time column

No mandatory groupings.

No default value.

9820 rows and 7 columns

## Details

The data is available here:
https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2023/20230310covid19infectionsurveydatasetsengland.xlsx
