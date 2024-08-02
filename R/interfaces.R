i_dated = interfacer::iface(
  date = as.Date ~ "A set of events with a timestamp as a `Date`"
)

i_timestamped = interfacer::iface(
  time = as.time_period ~ "A set of events with a timestamp as a `time_period`"
)

# Can be grouped.
i_timeseries = interfacer::iface(
  time = as.time_period + group_unique ~ "A (usually complete) set of singular observations per unit time as a `time_period`"
)

i_incidence_data = interfacer::iface(
  count = positive_integer ~ "Positive case counts associated with the specified timeframe",
  i_timeseries
)

i_population_data = interfacer::iface(
  population = positive_integer ~ "Size of population"
)

i_baseline_proportion_data = interfacer::iface(
  baseline_proportion = proportion ~ "Size of population"
)

i_incidence_per_capita_data = interfacer::iface(
  i_population_data,
  i_incidence_data
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

i_proportion_model = interfacer::iface(
  i_timeseries,
  proportion.fit = double ~ "an estimate of the proportion on a logit scale",
  proportion.se.fit = double ~ "the standard error of proportion estimate on a logit scale",
  proportion.0.025 = proportion ~ "lower confidence limit of proportion (true scale)",
  proportion.0.5 = proportion ~ "median estimate of proportion (true scale)",
  proportion.0.975 = proportion ~ "upper confidence limit of proportion (true scale)"
)


i_proportion_rate = interfacer::iface(
  i_proportion_model,
  relative.growth.fit = double ~ "an estimate of the relative growth rate",
  relative.growth.se.fit = double ~ "the standard error the relative growth rate",
  relative.growth.0.025 = double ~ "lower confidence limit of the relative growth rate",
  relative.growth.0.5 = double ~ "median estimate of the relative growth rate",
  relative.growth.0.975 = double ~ "upper confidence limit of the relative growth rate"
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
  incidence.se.fit = double ~ "the standard error of the incidence rate estimate on a log scale",
  incidence.0.025 = positive_double ~ "lower confidence limit of the incidence rate (true scale)",
  incidence.0.5 = positive_double ~ "median estimate of the incidence rate (true scale)",
  incidence.0.975 = positive_double ~ "upper confidence limit of the incidence rate (true scale)"
)

i_incidence_per_capita_model = interfacer::iface(
  i_timeseries,
  incidence.per_capita.fit = double ~ "an estimate of the incidence per capita rate on a log scale",
  incidence.per_capita.se.fit = double ~ "the standard error of the incidence per capita rate estimate on a log scale",
  incidence.per_capita.0.025 = positive_double ~ "lower confidence limit of the incidence per capita rate (true scale)",
  incidence.per_capita.0.5 = positive_double ~ "median estimate of the incidence per capita rate (true scale)",
  incidence.per_capita.0.975 = positive_double ~ "upper confidence limit of the incidence per capita rate (true scale)",
  population_unit = double ~ "The population unit on which the per capita incidence rate is calculated"
)

i_risk_ratio_model = interfacer::iface(
  i_proportion_model,
  # risk_ratio.fit = double ~ "an estimate of the excess risk ratio for a population group on a logit scale",
  # risk_ratio.se.fit = double ~ "the standard error of the excess risk ratio for a population group on a logit scale",
  risk_ratio.0.025 = positive_double ~ "lower confidence limit of the excess risk ratio for a population group",
  risk_ratio.0.5 = positive_double ~ "median estimate of the excess risk ratio for a population group",
  risk_ratio.0.975 = positive_double ~ "upper confidence limit of the excess risk ratio for a population group",
  baseline_proportion = proportion ~ "The population baseline risk from which the excess risk ratio is based"
)

i_growth_rate = interfacer::iface(
  i_timeseries,
  growth.fit = double ~ "an estimate of the growth rate",
  growth.se.fit = double ~ "the standard error the growth rate",
  growth.0.025 = double ~ "lower confidence limit of the growth rate",
  growth.0.5 = double ~ "median estimate of the growth rate",
  growth.0.975 = double ~ "upper confidence limit of the growth rate"
)

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
  rt.se.fit = double ~ "the standard error of the reproduction number",
  rt.0.025 = double ~ "lower confidence limit of the reproduction number",
  rt.0.5 = double ~ "median estimate of the reproduction number",
  rt.0.975 = double ~ "upper confidence limit of the reproduction number"
)


i_events = interfacer::iface(
  label = character ~ "the event label",
  start = date ~ "the start date, or the date of the event",
  end = date ~ "the end date or NA if a single event",
  .default = TRUE
)


## covid_infectivity_profile definition ----

#' The covid_infectivity_profile dataframe structure specification
#'
#' @format
#' `r i_infectivity_profile`
#'
#' @docType data
#' @keywords interfaces
#' @concept datasets
#' @name covid_infectivity_profile
NULL

i_infectivity_profile = interfacer::iface(
  boot = anything ~ "a bootstrap identifier",
  time = positive_double ~ "the end of the time period (in days)",
  probability = proportion ~ "the probability of infection between previous time period until `time`",
  .groups = ~ boot,
	.default = ggoutbreak::covid_infectivity_profile
)

## covid_infectivity_profile definition ends
