#

#' Infer the prevalence of disease from incidence estimates and population size.
#'
#' Log-scaled incidence estimates are used to generate samples of incidence.
#' These are convolved with the infectivity profile (as a set of discrete distributions)
#' and the result is evaluated as a fraction of population. The result is
#' fitted to a logit-normal (using moments) and quantiles produced from samples.
#'
#' @iparam pop The population data must be grouped in the same way as `modelled`.
#' @iparam modelled model output from processing the `raw` dataframe with something like
#'   `poisson_locfit_model`
#' @iparam ip An infectivity profile.
#' @param bootstraps the number of samples to take at each time point. This will
#'   be rounded up to a whole multiple of the infectivity profile distribution
#'   length.
#' @param seed a random number seed for reproducibility
#' @param ... not used
#'
#' @return the modelled input with additional prevalence columns
#' @export
#'
#' @examples
#' tmp = ggoutbreak::england_covid %>%
#'   time_aggregate(count=sum(count)) %>%
#'   ggoutbreak::poisson_locfit_model(window=21) %>%
#'   ggoutbreak::infer_prevalence(
#'      pop = ggoutbreak::england_demographics %>% ungroup() %>% summarise(population = sum(population)),
#'      ip = ggoutbreak::ganyani_ip
#'   ) %>%
#'   dplyr::glimpse()
infer_prevalence = function(
    modelled = i_timeseries,
    pop = i_population_data,
    ip = i_discrete_ip,
    bootstraps = 1000,
    seed = Sys.time(),
    ...
) {

  interfacer::idispatch(modelled,
                        infer_prevalence.proportion = i_proportion_model,
                        infer_prevalence.incidence = i_incidence_model
  )

}
#


#' Infer the prevalence of disease from proportion estimates.
#'
#' Logit-scaled proportion estimates are used to generate samples of proportion.
#' These are convolved with the infectivity profile (as a set of discrete distributions)
#' and the result is interpreted as a fraction of the population. The result is
#' fitted to a logit-normal (using moments) and quantiles produced from samples.
#'
#' @iparam pop The population data must be grouped in the same way as `modelled`.
#' @iparam modelled Model output from processing the `raw` dataframe with something like
#'   `proportion_locfit_model`
#' @iparam ip An infectivity profile.
#' @param bootstraps the number of samples to take at each time point. This will
#'   be rounded up to a whole multiple of the infectivity profile distribution
#'   length.
#' @param seed a random number seed for reproducibility
#' @param ... not used
#'
#' @return the modelled input with additional proportion columns
#' @export
#'
#' @examples
#' tmp = england_covid_pcr_positivity %>%
#'   ggoutbreak::proportion_locfit_model()
#'
#' tmp %>%
#'   ggoutbreak::infer_prevalence(
#'      pop = ggoutbreak::england_demographics %>% dplyr::ungroup() %>% summarise(population = sum(population)),
#'      ip = ggoutbreak::ganyani_ip
#'   ) %>%
#'   dplyr::glimpse()
infer_prevalence.proportion = function(
    modelled = i_proportion_model,
    ip = i_discrete_ip,
    bootstraps = 1000,
    seed = Sys.time(),
    ...) {

  ip = interfacer::ivalidate(ip)

  # TODO: This needs rethinking as the meaning of a proportion model varies
  # and this only is relevant when the proportion is a test positivity rate.
  # It is not going to produce a sensible output when it is a variant proportion
  # or age group proportion. Its possible

  # We estimate the force of infection as the convolution of
  # the infectivity profile and the proportion estimates. We do this using
  # resampling as the infectivity profiles can be anything. This is very similar
  # to the way we calculate the renewal equation based Rt

  # TODO: test negative serial interval
  start = min(ip$tau)

  # omega is a matrix 13x100
  omega = ip %>% .omega_matrix(epiestim_compat = FALSE)

  window = nrow(omega)
  # at a minimum we sample 10 times per infectivity profile.
  reps = min(ceiling(bootstraps/ncol(omega)),10)
  boots = reps*ncol(omega)

  # This reverses order so that the convolution can happen correctly without
  # reordering the timeseries.
  omega = omega[window:1]
  emit_warning=FALSE

  out = interfacer::igroup_process(modelled, function(modelled, omega, window,...) {

    df = modelled
    # pad = .logit_pad(window, df$proportion.fit[1], df$proportion.se.fit[1], spread = 1.1 )

    tmp = withr::with_seed(seed, {
      # dplyr::bind_rows(
      #   tibble::tibble(
      #     time = min(df$time) - window:1,
      #     incidence.fit = pad$mu,
      #     incidence.se.fit = pad$sigma,
      #     imputed = TRUE
      #   ),
      df %>% dplyr::mutate(imputed=FALSE) %>%
        dplyr::mutate(proportion.samples = purrr::map2(proportion.fit, proportion.se.fit, ~ .expit(stats::rnorm(boots,.x,.y))))
    })

    end = nrow(tmp)+min(c(start,0))

    tmp$prevalence.samples = vector("list", nrow(tmp))

    for (i in seq_along(tmp$time)) {
      if (i>=window+start && i<=end) {
        # incidence window is 13x1000. start is typically 1, zero or potentially negative.
        window_subset = tmp$proportion.samples[(i-window-start+1):(i-start)]
        samples_window = matrix(unlist(window_subset),nrow = window,byrow = TRUE)
        samples_window_ip = samples_window*as.vector(omega)
        proportion = apply(samples_window_ip,2,sum)
        # The proportion
        if (any(na.omit(proportion) < 0 | na.omit(proportion) > 1)) emit_warning<<-TRUE
        # force prevalence into 0-1 range.
        tmp$prevalence.samples[[i]] = pmin(pmax(proportion,0),1)
      }
    }

    tmp2 = tmp %>%
      dplyr::filter(!imputed) %>%
      dplyr::mutate(
        prevalence.fit = purrr::map_dbl(prevalence.samples, ~ .finite(mean)(.logit(.x))),
        prevalence.se.fit = purrr::map_dbl(prevalence.samples, ~ .finite(stats::sd)(.logit(.x))),
        prevalence.0.025 = purrr::map_dbl(prevalence.samples, .finite(stats::quantile), p=0.025),
        prevalence.0.5 = purrr::map_dbl(prevalence.samples, .finite(stats::quantile), p=0.5),
        prevalence.0.975 = purrr::map_dbl(prevalence.samples, .finite(stats::quantile), p=0.975)
      ) %>% dplyr::select(-prevalence.samples, -proportion.samples)

    return(tmp2)

  })

  if (emit_warning) warning("Some estimates of the force of infection exceeded the population size")

  return(out)

}

# .logit_pad(100,0.5,2)
# .logit_pad = function(length, mu, sigma, spread = 1.1 ) {
#   # This is not at all trivial as logit normal cannot be estimated from moments
#   # We could attempt to fit this to the mode & median...
#   mus = rep(mu, length)
#   # This is a heuristic to widen the SD
#   sigmas = ((sigma/1.78) ^ (1/spread^(1:length)))*1.78
#   return(list(mu=rev(mus), sigma = rev(sigmas)))
# }


