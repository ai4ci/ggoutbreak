library(tidyverse)

bpm = sim_branching_process(seed = 100) %>%
  ggoutbreak::sim_apply_delay() %>%
  glimpse()


# timeseries counts by observation day:
# All of this is observation delay (not physiological delay)
# sample results available when with result (sample date)
# admissions and deaths delayed reporting.
# test results available immediately (reporting date)
# symptoms available immediately

delayed_counts = bpm %>%
  ggoutbreak::sim_summarise_linelist(
    censoring = list(
      admitted = \(t) rgamma2(t, mean = 5),
      death = \(t) rgamma2(t, mean = 10),
      sample = \(t, result_delay) result_delay
    ),
    max_time = 0:80
  )

plot_counts(
  delayed_counts %>% filter(obs_time %% 10 == 0),
  mapping = aes(colour = labels(obs_time))
) +
  geom_line() +
  facet_wrap(~statistic, scales = "free_y")

test_delayed_observation = delayed_counts %>%
  filter(statistic == "admitted") %>%
  select(statistic, obs_time, time, count) %>%
  glimpse()

attr(test_delayed_observation, "ip_cache") = attr(bpm, "ip_cache")
attr(test_delayed_observation, "events") = attr(bpm, "events")

if (interactive()) {
  interfacer::use_dataframe(test_delayed_observation)
}
usethis::use_data(test_delayed_observation, overwrite = TRUE)
