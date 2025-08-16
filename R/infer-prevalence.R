#' Infer the prevalence of disease from incidence estimates and population size.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Log-scaled incidence estimates are used to generate samples of incidence.
#' These are convolved with the duration of infection (as the probability still
#' infected) on any given day (as a set of discrete distributions) and the
#' result is evaluated as a fraction of population. The result is fitted to a
#' logit-normal (using moments) and quantiles produced from samples. If test
#' sensitivity is used instead of duration of infection then the prevalence
#' estimate will be the expected proportion of test positives by screening test
#' rather than a true prevalence. Differences between this and the proportion of
#' test positives observed may be due to ascertainment bias or test sensitivity
#' misspecification.
#'
#' @iparam pop The population data must be grouped in the same way as `modelled`.
#' @iparam modelled Model output from processing the `raw` dataframe with something like
#'   `poission_locfit_model`
#' @iparam ip A discrete distribution representing the duration of infection (or
#'   probability of detection of infection). This will not sum to one.
#' @param bootstraps the number of samples to take at each time point. This will
#'   be rounded up to a whole multiple of the infection duration distribution
#'   length.
#' @param seed a random number seed for reproducibility
#' @param ... not used
#'
#' @return the modelled input with additional proportion columns
#' @export
#' @concept models
#'
#' @examples
#' tmp = ggoutbreak::test_poisson_rt_smooth %>%
#'   ggoutbreak::poisson_locfit_model(window=14) %>%
#'   ggoutbreak::infer_prevalence(
#'      pop = 10000,
#'      ip = ggoutbreak::test_ip
#'   )
#'
#' if(interactive()) {
#'   plot_prevalence(tmp)
#' }
infer_prevalence = function(
  modelled = i_incidence_model,
  pop = i_population_data,
  ip = i_discrete_ip,
  bootstraps = 1000,
  seed = Sys.time(),
  ...
) {
  modelled = interfacer::ivalidate(modelled)
  .stop_if_not_daily(modelled$time)
  ip = interfacer::ivalidate(ip)

  # add in population column (checks modelled and pop format)
  modelled = infer_population(modelled, pop)

  # We estimate the force of infection as the convolution of
  # the infectivity profile and the incidence estimates. We do this using
  # resampling as the infectivity profiles can be anything. This is very similar
  # to the way we calculate the renewal equation based Rt

  #TODO: test negative serial interval
  start = min(ip$tau)

  # omega is a matrix 13x100
  omega = ip %>% omega_matrix(epiestim_compat = FALSE)

  window = nrow(omega)
  # at a minimum we sample 10 times per infectivity profile.
  reps = min(ceiling(bootstraps / ncol(omega)), 10)
  boots = reps * ncol(omega)

  # This reverses order so that the convolution can happen correctly without
  # reordering the timeseries.
  logomega = log(omega)[window:1]
  emit_warning = FALSE

  out = interfacer::igroup_process(
    modelled,
    function(modelled, logomega, window, ...) {
      df = modelled
      pad = .ln_pad(
        length = window,
        time = df$time,
        mu = df$incidence.fit,
        vcov = df$incidence.se.fit^2,
        spread = 1.1
      )

      tmp = withr::with_seed(seed, {
        dplyr::tibble(
          time = pad$time,
          incidence.fit = pad$mu,
          incidence.se.fit = pad$sigma,
          imputed = pad$imputed
        ) %>%
          dplyr::mutate(
            incidence.logsamples = purrr::map2(
              incidence.fit,
              incidence.se.fit,
              ~ stats::rnorm(boots, .x, .y)
            )
          ) %>%
          dplyr::left_join(
            modelled %>% dplyr::select(time, population),
            by = "time"
          )
      })

      end = nrow(tmp) + min(c(start, 0))

      tmp$foi.logsamples = vector("list", nrow(tmp))
      tmp$prevalence.samples = vector("list", nrow(tmp))

      for (i in seq_along(tmp$foi.logsamples)) {
        if (i >= window + start && i <= end) {
          # incidence window is 13x1000. start is typically 1, zero or potentially negative.
          window_subset = tmp$incidence.logsamples[
            (i - window - start + 1):(i - start)
          ]
          samples_window = matrix(
            unlist(window_subset),
            nrow = window,
            byrow = TRUE
          )
          samples_window_ip = samples_window + as.vector(logomega)
          tmp$foi.logsamples[[i]] = apply(samples_window_ip, 2, .logsumexp)
          # The proportion
          proportion = exp(tmp$foi.logsamples[[i]] - log(tmp$population[[i]]))
          if (
            any(stats::na.omit(proportion) < 0 | stats::na.omit(proportion) > 1)
          ) {
            emit_warning <<- TRUE
          }
          # force prevalence into 0-1 range.
          tmp$prevalence.samples[[i]] = pmin(pmax(proportion, 0), 1)
        }
      }

      tmp2 = tmp %>%
        dplyr::filter(!imputed) %>%
        dplyr::mutate(
          prevalence.fit = purrr::map_dbl(
            prevalence.samples,
            ~ .finite(mean)(.logit(.x))
          ),
          prevalence.se.fit = purrr::map_dbl(
            prevalence.samples,
            ~ .finite(stats::sd)(.logit(.x))
          ),
          prevalence.0.025 = purrr::map_dbl(
            prevalence.samples,
            .finite(stats::quantile),
            p = 0.025
          ),
          prevalence.0.5 = purrr::map_dbl(
            prevalence.samples,
            .finite(stats::quantile),
            p = 0.5
          ),
          prevalence.0.975 = purrr::map_dbl(
            prevalence.samples,
            .finite(stats::quantile),
            p = 0.975
          )
        ) %>%
        dplyr::select(
          -incidence.logsamples,
          -foi.logsamples,
          -prevalence.samples
        )

      return(tmp2)
    }
  )

  if (emit_warning) {
    warning(
      "Some estimates of the force of infection exceeded the population size"
    )
  }

  return(out)
}
