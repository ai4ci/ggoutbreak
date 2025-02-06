## Serial interval data from meta-analysis paper.

si_sample_data = read.table("https://raw.githubusercontent.com/terminological/serial-interval/main/resampled-truncated-empirical-si-sample.txt")

covid_ip = si_sample_data %>%
  dplyr::mutate(tau = dplyr::row_number()-1) %>%
  tidyr::pivot_longer(cols = -(tau), names_to = "boot", names_prefix = "V", values_to = "probability") %>%
  dplyr::filter(tau>=0) %>%
  dplyr::group_by(boot) %>%
  dplyr::mutate(
    probability = probability/sum(probability),
    boot = as.numeric(boot),
    a0 = c(0,seq(0.5,length.out = dplyr::n()-1)),
    a1 = seq(0.5,length.out = dplyr::n()),
  ) %>%
  dplyr::arrange(boot,tau)

if(interactive()) {
  #usethis::use_data(covid_ip, overwrite = TRUE)
  interfacer::use_dataframe(covid_ip)
}


withr::with_seed(101, {

ganyani_ip =
  ggoutbreak::make_gamma_ip(5.2, 3.78, 6.78, 1.72, 0.91, 3.93, epiestim_compat=TRUE)

if(interactive()) {
  #usethis::use_data(ganyani_ip, overwrite = TRUE)
  interfacer::use_dataframe(ganyani_ip)
}

ganyani_ip_2 =
  ggoutbreak::make_gamma_ip(5.2, 3.78, 6.78, 1.72, 0.91, 3.93, epiestim_compat=FALSE)

if(interactive()) {
  #usethis::use_data(ganyani_ip_2, overwrite = TRUE)
  interfacer::use_dataframe(ganyani_ip_2)
}

})


#TODO: Du et al. data
# https://github.com/MeyersLabUTexas/COVID-19/raw/refs/heads/master/Table%20S5.xlsx
# [1] Z. Du, X. Xu, Y. Wu, L. Wang, B. J. Cowling, and L. A. Meyers, ‘Serial Interval of COVID-19 among Publicly Reported Confirmed Cases’, Emerg Infect Dis, vol. 26, no. 6, pp. 1341–1343, Jun. 2020, doi: 10.3201/eid2606.200357.

tmp = tempfile(fileext = ".xlsx")
download.file("https://github.com/MeyersLabUTexas/COVID-19/raw/refs/heads/master/Table%20S5.xlsx", tmp)
du_data = readxl::read_excel(tmp,skip = 1)

du_data = du_data %>% dplyr::mutate(
  tau = `Seconday - symptom onset date`-`Index - symptom onset date`
)

du_serial_interval_ip = make_resampled_ip(tau = du_data$tau,add_noise = TRUE,truncate = -5.5,seed = 100)

if(interactive()) {
  #usethis::use_data(ganyani_ip_2, overwrite = TRUE)
  interfacer::use_dataframe(du_serial_interval_ip)
}

#TODO: look at using fitdistrplus for infectivity profiles
# fitdistrplus::fitdist(du_data$tau[du_data$tau>0],distr = "gamma")
