
library(tidyverse)

test_ip = make_gamma_ip(5,4,6,2,1.5,2.5,epiestim_compat = TRUE)
interfacer::use_dataframe(test_ip)

test_ts = sim_test_data()
interfacer::use_dataframe(test_ts)

test_bpm = sim_branching_process(seed = 100) %>%
  sim_delay(~ 0.5, ~ rdiscgamma(.x, 6, 2), output="symptom_onset")
interfacer::use_dataframe(test_bpm)

test_serial = test_bpm %>%
  filter(!is.na(infector) & symptom_onset) %>%
  inner_join(test_bpm %>% select(infector=id,time.infector = time, symptom_onset.infector = symptom_onset, symptom_onset_time.infector = symptom_onset_time), by="infector") %>%
  filter(symptom_onset.infector) %>%
  mutate(serial_interval = symptom_onset_time-symptom_onset_time.infector) %>%
  arrange(time) %>%
  filter(row_number() <=  1000) %>%
  pull(serial_interval) %>%
  make_resampled_ip(truncate = -10,seed = 100)
interfacer::use_dataframe(test_serial)

test_poisson_rt = sim_poisson_Rt_model(seed = 100, fn_ip = \(t) test_ip)
interfacer::use_dataframe(test_poisson_rt)

test_poisson_growth_rate = sim_poisson_model(seed = 100)
interfacer::use_dataframe(test_poisson_growth_rate)

test_poisson_rt_smooth = sim_poisson_Rt_model(fn_Rt = \(t) exp(sin(t/80*pi)^4-0.25), max_time = 160)
interfacer::use_dataframe(test_poisson_rt_smooth)


test_poisson_rt_2class = dplyr::bind_rows(
  sim_poisson_Rt_model(fn_Rt = \(t) exp(sin(t/80*pi)^4-0.25), max_time = 160) %>% dplyr::mutate(class = "one"),
  sim_poisson_Rt_model(fn_Rt = \(t) exp(sin((t-10)/80*pi)^4-0.25), max_time = 160) %>% dplyr::mutate(class = "two")
) %>% dplyr::mutate(class = as.factor(class)) %>% dplyr::group_by(time) %>% dplyr::mutate(denom=sum(count)) %>%
  dplyr::group_by(class)

interfacer::use_dataframe(test_poisson_rt_2class)
