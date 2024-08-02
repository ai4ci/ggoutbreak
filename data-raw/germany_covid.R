## RSurvStat COVID case data
covid_count_1 = rsurvstat::get_timeseries(
  disease = rsurvstat::diseases$`COVID-19`,
  measure = "Count",
  age_group = rsurvstat::age_groups$children_coarse
)

covid_count_2 = rsurvstat::get_timeseries(
  disease = rsurvstat::diseases$`COVID-19`,
  measure = "Incidence",
  age_group = rsurvstat::age_groups$children_coarse
)

covid_count_3 = covid_count_1 %>% dplyr::inner_join(covid_count_2)

# work out population.
germany_demographics = covid_count_3 %>%
  dplyr::filter(count > 100 & incidence > 0) %>%
  dplyr::mutate(age_cat = forcats::fct_drop(age_cat)) %>%
  # mutate(pop = count/incidence*100000, year = format(date, "%Y")) %>% group_by(age_cat,year) %>%
  dplyr::mutate(pop = count/incidence*100000) %>%
  dplyr::group_by(class = age_cat) %>%
  dplyr::summarise(
    population = as.integer(round(mean(pop, na.rm = TRUE))),.groups = "keep")



if(interactive()) interfacer::use_dataframe(germany_demographics)

germany_covid = covid_count_1 %>%
  dplyr::filter(age_cat %in% levels(germany_demographics$class)) %>%
  dplyr::transmute(
    date=date,
    count=as.integer(count),
    class=factor(as.character(age_cat), levels(germany_demographics$class))
  ) %>%
  dplyr::group_by(class) %>%
  tidyr::complete(date = date_seq(date), fill = list(count=NA_integer_)) %>%
  dplyr::mutate(time = as.time_period(date, unit="1 week", start_date="2019-12-30")) %>%
  dplyr::group_by(time,class,date) %>%
  dplyr::mutate(denom=sum(count)) %>%
  dplyr::group_by(class)


if(interactive()) interfacer::use_dataframe(germany_covid)




## Growth rates example results

covid_modelled_incidence = ggoutbreak::england_covid %>%
  dplyr::group_modify(ggoutbreak::poisson_locfit_model) %>%
  dplyr::glimpse()

if(interactive()) interfacer::use_dataframe(covid_modelled_incidence)

covid_normalised_incidence = ggoutbreak::covid_count %>%
  ggoutbreak::normalised_incidence(incidence_fn = ggoutbreak::poisson_locfit_model) %>%
  dplyr::glimpse()

if(interactive()) interfacer::use_dataframe(covid_normalised_incidence)
