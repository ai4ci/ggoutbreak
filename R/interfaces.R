## Inputs ----

i_dated = interfacer::iface(
  date = as.Date ~ "A set of events with a timestamp as a `Date`"
)

i_timestamped = interfacer::iface(
  time = ggoutbreak::time_period ~ "A set of events with a timestamp as a `time_period`"
)

# Can be grouped.
i_timeseries = interfacer::iface(
  time = ggoutbreak::time_period + group_unique ~ "A (usually complete) set of singular observations per unit time as a `time_period`"
)

i_incidence_data = interfacer::iface(
  count = positive_integer ~ "Positive case counts associated with the specified timeframe",
  i_timeseries
)

i_population_data = interfacer::iface(
  population = positive_integer ~ "Size of population"
)

i_baseline_proportion_data = interfacer::iface(
  baseline_proportion = proportion ~ "Baseline proportion for comparison"
)

i_baseline_incidence_data = interfacer::iface(
  baseline_incidence = positive_double ~ "Baseline unnormalised incidence rate as count data"
)

i_incidence_per_capita_data = interfacer::iface(
  i_population_data,
  i_incidence_data,
  population_unit = double ~ "The population unit on which the per capita incidence rate is calculated",
  time_unit = lubridate::as.period ~ "The time period over which the per capita incidence rate is calculated"
)

i_proportion_data = interfacer::iface(
  denom = positive_integer ~ "Total test counts associated with the specified timeframe",
  i_incidence_data
)

i_multinomial_proportion_data = interfacer::iface(
  class = factor ~ "A factor specifying the type of observation. This will be things like
variant, or serotype, for a multinomial model. Any missing data points are ignored.",
  i_proportion_data,
  .groups = ~ class
)


i_incidence_input = interfacer::iface(
  i_incidence_data,
  .groups = FALSE
)

i_proportion_input = interfacer::iface(
  i_proportion_data,
  .groups = FALSE
)

i_multinomial_input = interfacer::iface(
  class = factor ~ "A factor specifying the type of observation. This will be things like
variant, or serotype, for a multinomial model. Any missing data points are ignored.",
  i_incidence_input,
  .groups = ~ class
)

## Outputs ----

i_proportion_model = interfacer::iface(
  i_timeseries,
  proportion.fit = double ~ "an estimate of the proportion on a logit scale",
  proportion.se.fit = positive_double ~ "the standard error of proportion estimate on a logit scale",
  proportion.0.025 = proportion ~ "lower confidence limit of proportion (true scale)",
  proportion.0.5 = proportion ~ "median estimate of proportion (true scale)",
  proportion.0.975 = proportion ~ "upper confidence limit of proportion (true scale)"
)

i_proportion_rate = interfacer::iface(
  i_proportion_model,
  relative.growth.fit = double ~ "an estimate of the relative growth rate",
  relative.growth.se.fit = positive_double ~ "the standard error the relative growth rate",
  relative.growth.0.025 = double ~ "lower confidence limit of the relative growth rate",
  relative.growth.0.5 = double ~ "median estimate of the relative growth rate",
  relative.growth.0.975 = double ~ "upper confidence limit of the relative growth rate"
)

i_prevalence_model = interfacer::iface(
  i_timeseries,
  prevalence.0.025 = proportion ~ "lower confidence limit of prevalence (true scale)",
  prevalence.0.5 = proportion ~ "median estimate of prevalence (true scale)",
  prevalence.0.975 = proportion ~ "upper confidence limit of prevalence (true scale)"
)

i_multinomial_proportion_model = interfacer::iface(
  i_timeseries,
  class = factor ~ "A factor specifying the type of observation. This will be things like
variant, or serotype, for a multinomial model. Any missing data points are ignored.",
  proportion.0.5 = proportion ~ "median estimate of proportion (true scale)",
  .groups = ~ class
)


i_incidence_model = interfacer::iface(
  i_timeseries,
  incidence.fit = double ~ "an estimate of the incidence rate on a log scale",
  incidence.se.fit = positive_double ~ "the standard error of the incidence rate estimate on a log scale",
  incidence.0.025 = positive_double ~ "lower confidence limit of the incidence rate (true scale)",
  incidence.0.5 = positive_double ~ "median estimate of the incidence rate (true scale)",
  incidence.0.975 = positive_double ~ "upper confidence limit of the incidence rate (true scale)"
)

i_incidence_per_capita_model = interfacer::iface(
  i_timeseries,
  incidence.per_capita.fit = double ~ "an estimate of the incidence per capita rate on a log scale",
  incidence.per_capita.se.fit = positive_double ~ "the standard error of the incidence per capita rate estimate on a log scale",
  incidence.per_capita.0.025 = positive_double ~ "lower confidence limit of the incidence per capita rate (true scale)",
  incidence.per_capita.0.5 = positive_double ~ "median estimate of the incidence per capita rate (true scale)",
  incidence.per_capita.0.975 = positive_double ~ "upper confidence limit of the incidence per capita rate (true scale)",
  population_unit = double ~ "The population unit on which the per capita incidence rate is calculated",
  time_unit = lubridate::as.period ~ "The time period over which the per capita incidence rate is calculated"
)

i_risk_ratio_model = interfacer::iface(
  i_timeseries,
  # i_proportion_model,
  # risk_ratio.fit = double ~ "an estimate of the excess risk ratio for a population group on a logit scale",
  # risk_ratio.se.fit = positive_double ~ "the standard error of the excess risk ratio for a population group on a logit scale",
  risk_ratio.0.025 = positive_double ~ "lower confidence limit of the excess risk ratio for a population group",
  risk_ratio.0.5 = positive_double ~ "median estimate of the excess risk ratio for a population group",
  risk_ratio.0.975 = positive_double ~ "upper confidence limit of the excess risk ratio for a population group"
  # baseline_proportion = proportion ~ "The population baseline risk from which the excess risk ratio is based"
)

i_rate_ratio_model = interfacer::iface(
  i_timeseries,
  # i_proportion_model,
  # risk_ratio.fit = double ~ "an estimate of the excess risk ratio for a population group on a logit scale",
  # risk_ratio.se.fit = positive_double ~ "the standard error of the excess risk ratio for a population group on a logit scale",
  rate_ratio.0.025 = positive_double ~ "lower confidence limit of the rate ratio for a population group",
  rate_ratio.0.5 = positive_double ~ "median estimate of the rate ratio for a population group",
  rate_ratio.0.975 = positive_double ~ "upper confidence limit of the rate ratio for a population group"
  # baseline_proportion = proportion ~ "The population baseline risk from which the excess risk ratio is based"
)

i_risk_model = interfacer::iface(
  i_timeseries,
  # i_proportion_model,
  # risk_ratio.fit = double ~ "an estimate of the excess risk ratio for a population group on a logit scale",
  # risk_ratio.se.fit = positive_double ~ "the standard error of the excess risk ratio for a population group on a logit scale",
  risk.0.025 = positive_double ~ "lower confidence limit of the excess risk ratio for a population group",
  risk.0.5 = positive_double ~ "median estimate of the excess risk ratio for a population group",
  risk.0.975 = positive_double ~ "upper confidence limit of the excess risk ratio for a population group"
  # baseline_proportion = proportion ~ "The population baseline risk from which the excess risk ratio is based"
)

i_growth_rate = interfacer::iface(
  i_timeseries,
  growth.fit = double ~ "an estimate of the growth rate",
  growth.se.fit = positive_double ~ "the standard error the growth rate",
  growth.0.025 = double ~ "lower confidence limit of the growth rate",
  growth.0.5 = double ~ "median estimate of the growth rate",
  growth.0.975 = double ~ "upper confidence limit of the growth rate"
)

#TODO: needs renaming
i_risk_ratio_rate = interfacer::iface(
  i_risk_ratio_model,
  i_proportion_rate
)

i_incidence_rate = interfacer::iface(
  i_incidence_model,
  i_growth_rate
)

i_incidence_per_capita_rate = interfacer::iface(
  i_incidence_per_capita_model,
  i_growth_rate
)

i_reproduction_number = interfacer::iface(
  i_timeseries,
  rt.fit = double ~ "an estimate of the reproduction number",
  rt.se.fit = positive_double ~ "the standard error of the reproduction number",
  rt.0.025 = double ~ "lower confidence limit of the reproduction number",
  rt.0.5 = double ~ "median estimate of the reproduction number",
  rt.0.975 = double ~ "upper confidence limit of the reproduction number"
)

## Events ----

i_events = interfacer::iface(
  label = character ~ "the event label",
  start = date ~ "the start date, or the date of the event",
  end = date ~ "the end date or NA if a single event",
  .default = TRUE
)

## Infectivity profiles ----


i_ip_base = interfacer::iface(
  boot = anything + default(1) ~ "a bootstrap identifier",
  probability = proportion ~ "the probability of new event during this period."
)

i_discrete_ip = interfacer::iface(
  i_ip_base,
  tau = integer + complete ~ "the days since the index event.",
  .groups = ~ boot,
  .default = ggoutbreak::covid_ip
)

i_empirical_ip = interfacer::iface(
  i_ip_base,
  a0 = double ~ "the beginning of the time period (in days)",
  a1 = double ~ "the end of the time period (in days)",
  .groups = ~ boot ,
  .default = ggoutbreak::covid_ip
)

## Simulation formats ----

i_sim_count_data = interfacer::iface(
  statistic = character ~ "An identifier for the statistic, whether that be infections, admissions, deaths",
  i_incidence_data,
  .groups = ~ statistic + .
)

i_sim_linelist = interfacer::iface(
  id = unique_id ~ "Patient level unique id",
  time = ggoutbreak::time_period ~ "Time of infection. A `time_period`"
)
