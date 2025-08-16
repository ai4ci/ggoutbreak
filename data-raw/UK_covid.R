england_cases = readr::read_csv(
  "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv"
)


## Population
tmp = tempfile()
download.file(
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021/census2021/census2021firstresultsenglandwales1.xlsx",
  tmp
)
UK_pop = readxl::read_xlsx(tmp, range = "A8:V383", sheet = "P02")
UK_pop = UK_pop %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("Aged"), names_to = "Age")

england_demographics = UK_pop %>%
  # England only
  dplyr::filter(`Area code [note 2]` == "E92000001") %>%
  dplyr::mutate(
    Age = Age %>% stringr::str_remove(stringr::fixed("\r\n[note 12]"))
  ) %>%
  dplyr::transmute(
    class = dplyr::case_when(
      Age == "Aged 4 years and under" ~ "00_04",
      Age == "Aged 90 years and over" ~ "90+",
      Age == "Aged 5 to 9 years" ~ "05_09",
      TRUE ~
        stringr::str_replace_all(
          Age,
          "^Aged ([0-9]+) to ([0-9]+) years$",
          "\\1_\\2"
        )
    ) %>%
      .as_factor(),
    population = value,
    baseline_proportion = population / sum(population),
  ) %>%
  dplyr::group_by(class)

# Trust LTLA probabilistic mappings
# https://epiforecasts.io/covid19.nhs.data/reference/load_mapping.html

if (interactive()) {
  interfacer::use_dataframe(england_demographics)
}

england_covid = england_cases %>%
  dplyr::filter(age %in% levels(england_demographics$class)) %>%
  dplyr::transmute(
    date = date,
    class = factor(age, levels(england_demographics$class)),
    count = cases
  ) %>% # dplyr::inner_join(england_demographics, by="class") %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    denom = sum(count)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    time = date_to_time(date, start_date = "2020-01-30")
  ) %>%
  dplyr::group_by(class)


if (interactive()) {
  interfacer::use_dataframe(england_covid)
}

england_covid_poisson_age_stratified = england_covid %>%
  ggoutbreak::poisson_locfit_model(window = 14)

if (interactive()) {
  interfacer::use_dataframe(england_covid_poisson_age_stratified)
}

england_covid_poisson = england_covid %>%
  time_aggregate() %>%
  ggoutbreak::poisson_locfit_model(window = 14)

if (interactive()) {
  interfacer::use_dataframe(england_covid_poisson)
}

england_covid_proportion_age_stratified = england_covid %>%
  ggoutbreak::proportion_locfit_model(window = 14)

if (interactive()) {
  interfacer::use_dataframe(england_covid_proportion_age_stratified)
}


## England testing effort

england_cases_tests = readr::read_csv(
  "https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesPCROnlyBySpecimenDate&metric=newPCRTestsBySpecimenDate&format=csv"
)

england_covid_pcr_positivity = england_cases_tests %>%
  dplyr::transmute(
    date = date,
    time = as.time_period(date, unit = "1 day"),
    count = newCasesPCROnlyBySpecimenDate,
    denom = newPCRTestsBySpecimenDate
  ) %>%
  dplyr::filter(!is.na(count) & !is.na(denom))

if (interactive()) {
  interfacer::use_dataframe(england_covid_pcr_positivity)
}


tmp = tempfile()
download.file(
  "https://assets.publishing.service.gov.uk/media/63a46e3ed3bf7f375c7d833d/221221_R_and_growth_rate_time_series_for_publication_v1.0.ods",
  tmp
)
consensus_rt = readODS::read_ods(
  tmp,
  range = "B10:F122",
  sheet = "Table1_-_R",
  col_names = FALSE
)
england_consensus_rt = consensus_rt %>%
  dplyr::transmute(date = as.Date(...1, "%d-%b-%y"), low = ...4, high = ...5)

if (interactive()) {
  interfacer::use_dataframe(england_consensus_rt)
}

consensus_growth_rate = readODS::read_ods(
  tmp,
  range = "B10:F122",
  sheet = "Table2_-_Growth_rate",
  col_names = FALSE
)
england_consensus_growth_rate = consensus_growth_rate %>%
  dplyr::transmute(
    date = as.Date(...1, "%d-%b-%y"),
    low = ...4 / 100,
    high = ...5 / 100
  )

if (interactive()) {
  interfacer::use_dataframe(england_consensus_growth_rate)
}


## By age testing effort:
tmp = tempfile()
download.file(
  "https://assets.publishing.service.gov.uk/media/62b459ab8fa8f5356d206d7b/demographic-data-tables-23-june-2022.ods",
  tmp
)
tmp_by_age = readODS::read_ods(
  tmp,
  range = "A3:DF1653",
  sheet = "Table_8",
  col_names = TRUE
)
denom_by_age = tmp_by_age %>%
  pivot_longer(
    cols = -(1:5),
    names_to = "date_range",
    values_to = "count",
    values_transform = as.character
  ) %>%
  dplyr::mutate(
    date = as.Date(stringr::str_replace(
      date_range,
      "^.*([0-9]{2})[^0-9]([0-9]{2})[^0-9]([0-9]{2})$",
      "20\\3-\\2-\\1"
    )),
    denom = suppressWarnings(as.numeric(count)),
    class = case_when(
      `Age group` == "0-9" ~ "00_09",
      `Age group` == "Not Stated" ~ NA,
      `Age group` == "Not Stated" ~ "90+",
      TRUE ~ stringr::str_replace(`Age group`, "-", "_")
    )
  ) %>%
  # glimpse()
  dplyr::group_by(class, date) %>%
  dplyr::summarise(denom = sum(denom, na.rm = TRUE))
denom_by_age %>% dplyr::glimpse()


.date_band = function(t) {
  r = unique(denom_by_age$date)
  as.Date(sapply(t, function(x) {
    if (x < min(r) || x >= max(r + 7)) {
      return(NA)
    }
    return(r[r <= x & lead(r, default = max(r + 7)) > x])
  }))
}

## rebanded population
tmp_pop = ggoutbreak::england_demographics %>%
  dplyr::mutate(
    class = case_when(
      class %in% c("00_04", "05_09") ~ "00_09",
      class %in% c("10_14", "15_19") ~ "10_19",
      class %in% c("20_24", "25_29") ~ "20_29",
      class %in% c("30_34", "35_39") ~ "30_39",
      class %in% c("40_44", "45_49") ~ "40_49",
      class %in% c("50_54", "55_59") ~ "50_59",
      class %in% c("60_64", "65_69") ~ "60_69",
      class %in% c("70_74", "75_79") ~ "70_79",
      class %in% c("80_84", "85_89") ~ "80_89",
      class %in% c("90+") ~ "90+",
    )
  ) %>%
  group_by(class) %>%
  summarise(population = sum(population, na.rm = TRUE))

## rebanded by age positive cases
england_covid_test_positives = ggoutbreak::england_covid %>%
  dplyr::mutate(
    class = case_when(
      class %in% c("00_04", "05_09") ~ "00_09",
      class %in% c("10_14", "15_19") ~ "10_19",
      class %in% c("20_24", "25_29") ~ "20_29",
      class %in% c("30_34", "35_39") ~ "30_39",
      class %in% c("40_44", "45_49") ~ "40_49",
      class %in% c("50_54", "55_59") ~ "50_59",
      class %in% c("60_64", "65_69") ~ "60_69",
      class %in% c("70_74", "75_79") ~ "70_79",
      class %in% c("80_84", "85_89") ~ "80_89",
      class %in% c("90+") ~ "90+",
    ),
    date = .date_band(date)
  ) %>%
  dplyr::filter(!is.na(date)) %>%
  dplyr::group_by(class, date) %>%
  dplyr::summarise(count = sum(count, na.rm = TRUE)) %>%
  dplyr::inner_join(denom_by_age, by = c("class", "date")) %>%
  dplyr::inner_join(tmp_pop, by = "class") %>%
  dplyr::mutate(time = ggoutbreak::as.time_period(date, "1 week")) %>%
  dplyr::glimpse()


if (interactive()) {
  interfacer::use_dataframe(england_covid_test_positives)
}

# test and trace counts suppressed
# tmp_by_age = readODS::read_ods(tmp,range = "A3:DF1653",sheet = "Table_9", col_names = TRUE)
# count_by_age = tmp_by_age %>% pivot_longer(cols=dplyr::starts_with("X"), names_to = "date_range", values_to = "count",values_transform = as.character) %>%
#     dplyr::mutate(
#       date = as.Date(stringr::str_replace(date_range, "^.*([0-9]{2})\\.([0-9]{2})\\.([0-9]{2})$","20\\3-\\2-\\1")),
#       count = suppressWarnings(as.numeric(count)),
#       class = case_when(
#         Age.group == "0-9" ~ "00_09",
#         Age.group == "Not Stated" ~ NA,
#         Age.group == "Not Stated" ~ "90+",
#         TRUE ~ stringr::str_replace(Age.group,"-","_")
#       )
#     ) %>%
#     dplyr::group_by(class,date) %>%
#     dplyr::summarise(count = sum(count,na.rm = TRUE))
# count_by_age %>% dplyr::glimpse()
