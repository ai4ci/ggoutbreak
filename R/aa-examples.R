# example data cache
exdata = new.env(parent = emptyenv())

#' Example generators
#'
#' These are a set of internally cached functions to support
#' examples. They are exported as internal functions so that the examples can
#' run correctly and cache their output to prevent excessive repetition of the
#' code examples.
#'
#' @returns example output of the stated functions, usually a dataframe.
#' @keywords internal
#' @name example_fns
#' @examples
#' suppressWarnings({
#'   example_poisson_age_stratified() %>% dplyr::glimpse()
#'   example_ip() %>% dplyr::glimpse()
#'   example_bpm() %>% dplyr::glimpse()
#'   example_serial() %>% dplyr::glimpse()
#'   example_poisson_rt() %>% dplyr::glimpse()
#'   example_poisson_growth_rate() %>% dplyr::glimpse()
#'   example_poisson_rt_smooth() %>% dplyr::glimpse()
#'   example_poisson_rt_2class() %>% dplyr::glimpse()
#'   example_ganyani_ip() %>% dplyr::glimpse()
#'   example_du_serial() %>% dplyr::glimpse()
#'   example_delayed_observation() %>% dplyr::glimpse()
#' })
NULL

# Examples from COVID data ----

#' @describeIn example_fns Example input format including age stratified COVID-19 data.
#' @export
example_england_covid_by_age = function() {
  if (is.null(exdata$england_covid_by_age)) {
    with_defaults("2019-12-29", "1 day", {
      exdata$england_covid_by_age =
        ukc19::england_cases_by_5yr_age %>%
        timeseries()
    })
  }
  return(exdata$england_covid_by_age)
}

#' @describeIn example_fns Example output of [poisson_locfit_model()] run on
#'   age stratified COVID-19 data.
#' @export
example_poisson_age_stratified = function() {
  if (is.null(exdata$poisson_age_stratified)) {
    with_defaults("2019-12-29", "1 day", {
      exdata$poisson_age_stratified =
        example_england_covid_by_age() %>%
        poisson_locfit_model(window = 14)
    })
  }
  return(exdata$poisson_age_stratified)
}

#' @describeIn example_fns Example output of [poisson_locfit_model()] run on
#'   un-stratified COVID-19 data.
#' @export
example_poisson_locfit = function() {
  if (is.null(exdata$example_poisson_locfit)) {
    with_defaults("2019-12-29", "1 day", {
      exdata$example_poisson_locfit =
        example_england_covid_by_age() %>%
        time_aggregate() %>%
        poisson_locfit_model(window = 14)
    })
  }
  return(exdata$example_poisson_locfit)
}

#' @describeIn example_fns Example output of [proportion_locfit_model()]
#' An England COVID-19 age stratified proportion model dataset. The proportion
#' represented here is the positive tests in this age group, versus the positive
#' tests in all age groups, so represents a relative growth of ne age group
#' versus others.
#' @export
example_proportion_age_stratified = function() {
  if (is.null(exdata$example_proportion_age_stratified)) {
    with_defaults("2019-12-29", "1 day", {
      exdata$example_proportion_age_stratified =
        example_england_covid_by_age() %>%
        proportion_locfit_model(window = 14)
    })
  }
  return(exdata$example_proportion_age_stratified)
}

# Template ----
# "
# #' @describeIn example_fns Example output of [ YYYY()]
# #' An example of the YYYY
# #' @export
# example_XXXX = function() {
#   if (is.null(exdata$example_XXXX)) {
#     exdata$example_XXXX =
#       YYYY
#   }
#   return(exdata$example_XXXX)
# }
# "

# Serial intervals for COVID-19 ----

#' @describeIn example_fns Generation time estimate from [make_gamma_ip()].
#' This estimate is truncated to make it compatible with `EpiEstim`. Take from
#' Ganyani T, Kremer C, Chen D, Torneri A, Faes C, Wallinga J, Hens N.
#' Estimating the generation interval for coronavirus disease (COVID-19) based
#' on symptom onset data, March 2020. Euro Surveill. 2020 Apr;25(17):2000257.
#' doi: 10.2807/1560-7917.ES.2020.25.17.2000257. PMID: 32372755; PMCID:
#' PMC7201952.
#' @export
example_ganyani_ip = function() {
  if (is.null(exdata$example_ganyani_ip)) {
    exdata$example_ganyani_ip =
      make_gamma_ip(
        5.2,
        3.78,
        6.78,
        1.72,
        0.91,
        3.93,
        epiestim_compat = TRUE
      )
  }
  return(exdata$example_ganyani_ip)
}

#' @describeIn example_fns An undjusted serial interval between symptoms [make_resampled_ip()]
#' Z. Du, X. Xu, Y. Wu, L. Wang, B. J. Cowling, and L. A. Meyers, ‘Serial
#' Interval of COVID-19 among Publicly Reported Confirmed Cases’, Emerg Infect
#' Dis, vol. 26, no. 6, pp. 1341–1343, Jun. 2020, doi: 10.3201/eid2606.200357.
#' @export
example_du_serial = function() {
  if (is.null(exdata$example_du_serial)) {
    du_data =
      ukc19::du_serial_interval %>%
      dplyr::inner_join(
        ukc19::du_serial_interval %>% dplyr::select(id, symptom_onset),
        by = c("infector_id" = "id"),
        suffix = c(".sec", ".pri")
      ) %>%
      dplyr::mutate(tau = symptom_onset.sec - symptom_onset.pri)

    exdata$example_du_serial = make_resampled_ip(
      tau = du_data$tau,
      add_noise = TRUE,
      truncate = -5.5,
      seed = 100
    )
  }
  return(exdata$example_du_serial)
}


# Simulated test data ----

#' @describeIn example_fns Example output of [make_gamma_ip()]
#' A test infectivity profile generated from a set of discretised gamma
#' distributions with parameters mean 5 (95% CI 4-6) and sd 2 (95% CI 1.5-2.5).
#' @export
example_ip = function() {
  if (is.null(exdata$example_ip)) {
    exdata$example_ip =
      make_gamma_ip(5, 4, 6, 2, 1.5, 2.5, epiestim_compat = TRUE, seed = 123)
  }
  return(exdata$example_ip)
}

#' @describeIn example_fns Example output of [sim_branching_process()]
#' An example of the linelist output of the branching process model simulation.
#' This is generated using the `example_ip()` infectivity profile and
#' also includes a delay to symptom onset which is a random gamma distributed
#' quantity with mean of 6 and standard deviation of 2
#' @export
example_bpm = function() {
  if (is.null(exdata$example_bpm)) {
    with_defaults("2025-01-01", "1 day", {
      exdata$example_bpm =
        sim_branching_process(seed = 100) %>%
        sim_delay(~0.5, ~ rdiscgamma(.x, 6, 2), output = "symptom_onset")
    })
  }
  return(exdata$example_bpm)
}


#' @describeIn example_fns Example output of [make_resampled_ip()]
#' A serial interval estimated from symptom onset in simulated data including
#' negative intervals. This serial interval is resampled from the first 1000
#' patients in the [example_bpm()] dataset for whom both infector and infectee
#' has symptoms. These patients are generated with a symptom delay of mean 6
#' days and SD 2 from infection (discrete under-dispersed gamma) and an
#' infectivity profile with mean 5 days and SD 2 as defined in [example_ip()]
#' dataset. This serial interval is relevant to the estimation of $R_t$ from
#' symptomatic case counts in the [example_bpm()] dataset but includes negative
#' times, and cannot be used with `EpiEstim`.
#' @export
example_serial = function() {
  if (is.null(exdata$example_serial)) {
    with_defaults("2025-01-01", "1 day", {
      exdata$example_serial =
        example_bpm() %>%
        dplyr::filter(!is.na(infector) & symptom_onset) %>%
        dplyr::inner_join(
          example_bpm() %>%
            dplyr::select(
              infector = id,
              time.infector = time,
              symptom_onset.infector = symptom_onset,
              symptom_onset_time.infector = symptom_onset_time
            ),
          by = "infector"
        ) %>%
        dplyr::filter(symptom_onset.infector) %>%
        dplyr::mutate(
          serial_interval = symptom_onset_time - symptom_onset_time.infector
        ) %>%
        dplyr::arrange(time) %>%
        dplyr::filter(dplyr::row_number() <= 1000) %>%
        dplyr::pull(serial_interval) %>%
        make_resampled_ip(truncate = -10, seed = 100)
    })
  }
  return(exdata$example_serial)
}

#' @describeIn example_fns Example output of [sim_poisson_Rt_model()]
#' An example of the linelist output of the poisson model simulation with defined
#' $R_t$. This is generated using the `example_ip()` infectivity profile
#' @export
example_poisson_rt = function() {
  if (is.null(exdata$example_poisson_rt)) {
    with_defaults("2025-01-01", "1 day", {
      exdata$example_poisson_rt =
        sim_poisson_Rt_model(seed = 100, fn_ip = ~ example_ip())
    })
  }
  return(exdata$example_poisson_rt)
}


#' @describeIn example_fns Example output of [sim_poisson_model()]
#' A simulation dataset determined by a step function of growth rates. This is
#' useful for demonstrating growth rate estimators.
#' @export
example_poisson_growth_rate = function() {
  if (is.null(exdata$example_poisson_growth_rate)) {
    with_defaults("2025-01-01", "1 day", {
      exdata$example_poisson_growth_rate =
        sim_poisson_model(seed = 100)
    })
  }
  return(exdata$example_poisson_growth_rate)
}


#' @describeIn example_fns Example output of [sim_poisson_Rt_model()]
#' Output of a poisson model simulation with a
#' smooth function for $R_t$ defined as `R(t) = e^(sin(t/80*pi)^4-0.25))`. This
#' is a relatively unchallenging test data set that should not pose a problem
#' for smooth estimators.
#' @export
example_poisson_rt_smooth = function() {
  if (is.null(exdata$example_poisson_rt_smooth)) {
    with_defaults("2025-01-01", "1 day", {
      exdata$example_poisson_rt_smooth =
        sim_poisson_Rt_model(
          fn_Rt = function(t) exp(sin(t / 80 * pi)^4 - 0.25),
          max_time = 160
        )
    })
  }
  return(exdata$example_poisson_rt_smooth)
}

#' @describeIn example_fns Two class example using [sim_poisson_Rt_model()]
#' Two smooth $R_t$ based incidence timeseries one growing with an time varying Rt
#' `exp(sin(t / 80 * pi)^4 - 0.25)` and the other offset by 10 days:
#' `exp(sin((t - 10) / 80 * pi)^4 - 0.25)`. This is a simple relative growth test
#'
#' @export
example_poisson_rt_2class = function() {
  if (is.null(exdata$example_poisson_rt_2class)) {
    with_defaults("2025-01-01", "1 day", {
      exdata$example_poisson_rt_2class =
        dplyr::bind_rows(
          sim_poisson_Rt_model(
            fn_Rt = function(t) exp(sin(t / 80 * pi)^4 - 0.25),
            max_time = 160
          ) %>%
            dplyr::mutate(class = "one"),
          sim_poisson_Rt_model(
            fn_Rt = function(t) exp(sin((t - 10) / 80 * pi)^4 - 0.25),
            max_time = 160
          ) %>%
            dplyr::mutate(class = "two")
        ) %>%
        dplyr::mutate(class = as.factor(class)) %>%
        dplyr::group_by(time) %>%
        dplyr::mutate(denom = sum(count)) %>%
        dplyr::group_by(class)
    })
  }
  return(exdata$example_poisson_rt_2class)
}

#' @describeIn example_fns Example output of [sim_branching_process()] with
#' [sim_apply_delay()]
#' This simulates what might be observed in an outbreak if there was on average
#' a 5 day delay on the reporting of hospital admissions.
#' The configuration of the outbreak is the same as [example_bpm()], but
#' this is summary data that describes the whole history of admissions that were
#' observed, when observed at any given time point. This is a triangular set
#' of data where the counts are right censored by the observation time.
#' @export
example_delayed_observation = function() {
  if (is.null(exdata$example_delayed_observation)) {
    with_defaults("2025-01-01", "1 day", {
      bpm = sim_branching_process(seed = 100) %>%
        sim_apply_delay()
    })

    # timeseries counts by observation day:
    # All of this is observation delay (not physiological delay)
    # sample results available when with result (sample date)
    # admissions and deaths delayed reporting.
    # test results available immediately (reporting date)
    # symptoms available immediately

    delayed_counts = bpm %>%
      sim_summarise_linelist(
        censoring = list(
          admitted = function(t) rgamma2(t, mean = 5),
          death = function(t) rgamma2(t, mean = 10),
          sample = function(t, result_delay) result_delay
        ),
        max_time = 0:80
      )

    # plot_counts(
    #   delayed_counts %>% filter(obs_time %% 10 == 0),
    #   mapping = aes(colour = labels(obs_time))
    # ) +
    #   geom_line() +
    #   facet_wrap(~statistic, scales = "free_y")

    test_delayed_observation = delayed_counts %>%
      dplyr::filter(statistic == "admitted") %>%
      dplyr::select(statistic, obs_time, time, count)

    attr(test_delayed_observation, "ip_cache") = attr(bpm, "ip_cache")
    attr(test_delayed_observation, "events") = attr(bpm, "events")

    exdata$example_delayed_observation = test_delayed_observation
  }
  return(exdata$example_delayed_observation)
}
