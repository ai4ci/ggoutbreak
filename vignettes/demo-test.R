library(ggoutbreak)

ggplot2::theme_set(ggplot2::theme_bw(base_size = 16)+
   ggplot2::theme(
     axis.text.x.bottom = ggplot2::element_text(angle=30,vjust = 1,hjust=1),
     axis.text.x.top = ggplot2::element_text(angle=30,vjust = 0,hjust=0),
     legend.key.size = ggplot2::unit(0.1,"inches")
   )
)

inf_prof = make_gamma_ip(
  median_of_mean = 5,
  lower_ci_of_mean = 4, upper_ci_of_mean = 6,
  median_of_sd = 1.5,
  lower_ci_of_sd = 1.2, upper_ci_of_sd = 1.7
)

estimates = england_covid %>%
  poisson_locfit_model(window = 14, deg = 1) %>%
  rt_from_incidence(ip = inf_prof) %>%
  normalise_incidence(pop = england_demographics)

plot_incidence(
  estimates,
  england_covid %>% infer_population(england_demographics),
  date_labels="%b %y")+
  ggplot2::scale_color_viridis_d(aesthetics = c("colour","fill"))+
  scale_y_log1p()

plot_growth_phase(estimates,
        duration = 56,cis = FALSE,
        timepoints = as.Date(c("2021-12-25","2022-12-25")))+
  coord_cartesian(xlim=c(-0.15,0.15))+
  ggplot2::scale_color_viridis_d(aesthetics = c("colour"))


plot_rt(estimates, date_labels="%b %y")+
  coord_cartesian(ylim=c(0.5,2.0))+
  ggplot2::scale_color_viridis_d(aesthetics = c("colour","fill"))
