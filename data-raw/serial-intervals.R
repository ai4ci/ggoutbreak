## Serial interval data from meta-analysis paper.

si_sample_data = read.table("https://raw.githubusercontent.com/terminological/serial-interval/main/resampled-truncated-empirical-si-sample.txt")

covid_infectivity_profile = si_sample_data %>%
  dplyr::mutate(time = dplyr::row_number()-1) %>%
  tidyr::pivot_longer(cols = -(time), names_to = "boot", names_prefix = "V", values_to = "probability") %>%
  dplyr::filter(time>0) %>%
  dplyr::group_by(boot) %>%
  dplyr::mutate(
    probability = probability/sum(probability),
    boot = as.numeric(boot)
  )

if(interactive()) interfacer::use_iface(covid_infectivity_profile, use_as_default = TRUE)

