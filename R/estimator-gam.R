#' GAM poisson time-series model
#'
#' This function lets the user supply a fitting function that models incidence,
#' and provides a set of machinery for applying it to groups, extracting
#' incidence, growth rates, and optionally reproduction numbers from the fit(s).
#' This is more advanced than other estimators as there are far more
#' configuration of GAM models, so this aims to provide sensible defaults with
#' the option to bring your own model.
#'
#' @iparam d input data

#' @inheritAllDotParams poisson_gam_model.censored
#' @inheritAllDotParams poisson_gam_model.incidence
#' @param frequency the density of the output estimates as a time period such as
#'   `7 days` or `2 weeks`.
#' @iparam ip An infectivity profile (optional) if not given (the default) the
#'   Rt value will not be estimated
#' @param quick if `ip` is provided, and quick is `TRUE` Rt estimation will be
#'   done assuming independence which is quicker but less accurate. Setting this
#'   to false will use a full variance-covariance matrix.
#' @param .progress show a CLI progress bar
#'
#' @returns `r i_incidence_rate`
#'
#' additionally if `ip` is given: `r i_reproduction_number`
#' @export
#' @concept models
#'
#' @examples
#'
#' # Simple poisson model
#' data = test_poisson_rt_smooth
#' tmp2 = poisson_gam_model(data,window=7,ip=test_ip,quick=TRUE)
#'
#' if (interactive()) {
#'   plot_incidence(
#'     tmp2,
#'     date_labels="%b %y",
#'     raw=data
#'   )
#'
#'   plot_rt(
#'     tmp2,
#'     date_labels="%b %y"
#'   )+
#'   sim_geom_function(data,colour="red")
#' }
#'
#'
#'
#' # example with delayed observation model.
#' # This data is all the day by day observations of the whole timeseries from
#' # the beginning of the outbreak.
#' data2 = test_delayed_observation
#' model = gam_delayed_reporting(window = 14)
#' tmp3 = data2 %>% poisson_gam_model(
#'   model_fn = model$model_fn,
#'   predict = model$predict,
#'   ip=test_ip)
#'
#' if (interactive()) {
#'   plot_incidence(tmp3)+
#'     ggplot2::geom_line(
#'       data=data2 %>% dplyr::filter(obs_time %% 10 == 0),
#'       mapping = ggplot2::aes(x=as.Date(time),y=count,colour=as.factor(obs_time))
#'       )
#'
#'   plot_rt(tmp3)
#' }
poisson_gam_model = function(
  d,
  ...,
  frequency = "1 day",
  ip = i_discrete_ip,
  quick = FALSE,
  .progress = interactive()
) {
  #TODO: slow example

  .message_context()
  interfacer::idispatch(
    d,
    poisson_gam_model.censored = i_censored_incidence_data,
    poisson_gam_model.incidence = i_incidence_data
  )
}

#' Poisson model for censored data
#' @inheritDotParams gam_delayed_reporting
#' @param model_fn a function that takes data relating to one time series
#'   (e.g. the input data `d` on a group by group basis) and returns a fitted
#'   GAM. The default is creates a delayed reporting model [gam_delayed_reporting()].
#' @param predict if the GAM model in `model_fn` introduces other variables we
#'   need to know what their values should be fixed at for prediction.
#'   This is a named list of defaults for variables in the model supplied by
#'   `model_fn`. These defaults will be used in prediction. This
#'   may be supplied as part of the model function generator (
#'   e.g. `gam_delayed_reporting(...)$predict`). If this is set to exactly `FALSE`
#'   no prediction is performed and a list column of fitted GAM models returned
#'   instead.
#' @keywords internal
poisson_gam_model.censored = function(
  d = i_censored_incidence_data,
  model_fn = gam_delayed_reporting(...)$model_fn,
  ...,
  frequency = "1 day",
  predict = gam_delayed_reporting(...)$predict,
  ip = i_discrete_ip,
  quick = FALSE,
  .progress = interactive()
) {
  env = rlang::current_env()
  if (.progress) {
    cli::cli_progress_bar(
      "incidence & growth rate (GAM)",
      total = dplyr::n_groups(d),
      .envir = env
    )
  }

  modelled = interfacer::igroup_process(
    df = d,
    fn = function(d, .groupdata, ...) {
      incid = .do_one_gam_fit(
        d,
        .groupdata,
        model_fn,
        predict,
        ip,
        frequency,
        quick
      )
      if (.progress) {
        cli::cli_progress_update(.envir = env)
      }
      return(incid)
    }
  )
  if (.progress) {
    cli::cli_progress_done()
  }

  return(modelled %>% .normalise_from_raw(d))
}

#' Poisson model for incidence data
#' @inheritDotParams gam_poisson_model_fn
#' @param model_fn a function that takes data relating to one time series
#'   (e.g. the input data `d` on a group by group basis) and returns a fitted
#'   GAM. The default creates a simple poisson model based on count alone ([gam_poisson_model_fn()]).
#' @param predict if the GAM model in `model_fn` introduces other variables we
#'   need to know what their values should be fixed at for prediction.
#'   This is a named list of defaults for variables in the model supplied by
#'   `model_fn`. These defaults will be used in prediction. This
#'   may be supplied as part of the model function generator (
#'   e.g. `gam_delayed_reporting(...)$predict`). If this is set to exactly `FALSE`
#'   no prediction is performed and a list column of fitted GAM models returned
#'   instead.
#' @keywords internal
poisson_gam_model.incidence = function(
  d = i_incidence_input,
  model_fn = gam_poisson_model_fn(...),
  ...,
  frequency = "1 day",
  predict = list(),
  ip = i_discrete_ip,
  quick = FALSE,
  .progress = interactive()
) {
  env = rlang::current_env()
  if (.progress) {
    cli::cli_progress_bar(
      "incidence & growth rate (GAM)",
      total = dplyr::n_groups(d),
      .envir = env
    )
  }

  modelled = interfacer::igroup_process(
    df = d,
    fn = function(d, .groupdata, ...) {
      incid = .do_one_gam_fit(
        d,
        .groupdata,
        model_fn,
        predict,
        ip,
        frequency,
        quick
      )
      if (.progress) {
        cli::cli_progress_update(.envir = env)
      }
      return(incid)
    }
  )
  if (.progress) {
    cli::cli_progress_done()
  }

  return(modelled %>% .normalise_from_raw(d))
}

.do_one_gam_fit = function(
  d,
  .groupdata,
  model_fn,
  predict,
  ip,
  frequency,
  quick
) {
  cutoff = min(d$time)
  if (interfacer::itest(ip, i_discrete_ip)) {
    max_tau = max(ip$tau)
    min_tau = min(ip$tau)
    do_rt = TRUE
    frequency = "1 day"
    pred_time = .daily_times(d$time, max_tau, -min_tau)
  } else {
    max_tau = 0
    do_rt = FALSE
    pred_time = date_seq.time_period(d$time, period = frequency)
  }
  # what are the prediction times

  if ("time" %in% names(predict)) {
    output_times = as.time_period(predict$time, d$time)
    predict$time = NULL
  } else {
    output_times = pred_time
  }

  # fit the GAM model
  fit = model_fn(d)

  if (isFALSE(predict)) {
    return(list(fit))
  }

  newdata = dplyr::tibble(time = output_times, !!!predict)
  pred = stats::predict(fit, newdata, se.fit = TRUE)
  deriv = try(predict_derivative(fit, newdata), silent = TRUE)

  incid = newdata %>%
    dplyr::mutate(
      time = as.time_period(time, "1 day"),
      incidence.fit = as.numeric(pred$fit),
      incidence.se.fit = as.numeric(pred$se.fit)
    ) %>%
    .result_from_fit(., "incidence", inv = exp) %>%
    .keep_cdf(
      type = "incidence",
      meanlog = pred$fit,
      sdlog = pred$se.fit,
      link = "log"
    ) %>%
    .tidy_fit("incidence", incidence.se.fit > 4)

  if (!inherits(deriv, "try-error")) {
    incid = incid %>%
      dplyr::mutate(
        time = as.time_period(time, "1 day"),
        growth.fit = deriv$fit,
        growth.se.fit = deriv$se.fit
      ) %>%
      .result_from_fit(., "growth") %>%
      .keep_cdf(type = "growth", mean = deriv$fit, sd = deriv$se.fit) %>%
      .tidy_fit("growth", growth.se.fit > 0.25)
  } else {
    .warn_once(
      "growth rate could not be extracted from GAM model: ",
      trimws(as.character(deriv))
    )
  }

  if (do_rt) {
    if (quick) {
      .message_once(
        "Rt estimation using GAM (approx and assuming independence)"
      )
      pred_vcov = NULL
    } else {
      .message_once(
        "Rt estimation using GAM (exact with modelled covariance)"
      )
      # get prediction vcov from GAM fit:
      Xp <- stats::predict(fit, newdata, type = "lpmatrix")
      pred_vcov <- Xp %*% stats::vcov(fit) %*% t(Xp)
    }

    tmp_ip = .select_ip(ip, .groupdata)

    rt = rt_incidence_timeseries_implementation(
      time = incid$time,
      mu = pred$fit,
      sigma = pred$se.fit,
      vcov = pred_vcov,
      ip = tmp_ip,
      tidy = TRUE,
      approx = quick
    )

    incid = incid %>% dplyr::left_join(rt, by = "time")
  }

  incid = incid %>% dplyr::filter(time >= cutoff)

  return(incid)
}

#' Default GAM count model.
#'
#' This function configures a very simple poisson count model and using:
#'
#' `count ~ s(time, bs = "cr", k = length(kts))`
#'
#' The control of knot positions is defined by the `knots_fn` and `window`
#' parameters.
#'
#' @inheritParams gam_delayed_reporting
#' @inheritDotParams gam_knots k
#'
#' @returns a function suitable as the input for
#'   `poisson_gam_model(model_fn = ...)`.
#' @export
#' @concept models
#'
#' @examples
#' model_fn = gam_poisson_model_fn(14)
#' fit = model_fn(test_poisson_rt %>% dplyr::ungroup())
#' summary(fit)
#'
gam_poisson_model_fn = function(
  window,
  ...,
  knots_fn = ~ gam_knots(.x, window, ...)
) {
  knots_fn = rlang::as_function(knots_fn)

  function(data = i_incidence_input) {
    data = interfacer::ivalidate(data)
    kts = knots_fn(data)
    return(
      mgcv::gam(
        count ~ s(time, bs = "cr", k = length(kts)),
        data = data,
        knots = list(time = kts),
        method = "REML",
        stats::quasipoisson()
      )
    )
  }
}


#' Default GAM count negative binomial model.
#'
#' This function configures a very simple negative binomial count model and using:
#'
#' `count ~ s(time, bs = "cr", k = length(kts))`
#'
#' The control of knot positions is defined by the `knots_fn` and `window`
#' parameters.
#'
#' @inheritParams gam_delayed_reporting
#' @inheritDotParams gam_knots k
#'
#' @returns a function suitable as the input for
#'   `poisson_gam_model(model_fn = ...)`.
#' @export
#' @concept models
#'
#' @examples
#' model_fn = gam_nb_model_fn(14)
#' fit = model_fn(test_poisson_rt %>% dplyr::ungroup())
#' summary(fit)
#'
gam_nb_model_fn = function(
  window,
  ...,
  knots_fn = ~ gam_knots(.x, window, ...)
) {
  knots_fn = rlang::as_function(knots_fn)

  function(data = i_incidence_input) {
    data = interfacer::ivalidate(data)
    kts = knots_fn(data)
    return(
      mgcv::gam(
        count ~ s(time, bs = "cr", k = length(kts)),
        data = data,
        knots = list(time = kts),
        method = "REML",
        mgcv::nb()
      )
    )
  }
}

#' Delayed GAM reporting model function generator
#'
#' @details
#' This function is used to configure a delayed reporting GAM model. The model
#' is of the form:
#'
#' `count ~ s(time, bs = "cr", k = length(kts)) + s(log(tau), k = 4, pc = max_delay)`
#'
#' where `tau` is the difference between time series observation time and the
#' time of the data point in the time series, and we have multiple observations
#' of the same time series. This function helps specify the knots of the GAM and
#' the maximum expected delay
#'
#' @param window controls the knot spacing in the GAM (if the default)
#' @param max_delay the maximum delay we expect to model
#' @inheritDotParams gam_knots k
#' @param knots_fn a function that takes the data as an input and returns
#'   a set of integers as time points for GAM knots, for `s(time)` term. The
#'   default here provides a roughly equally spaced grid determined by `window`,
#'   by a user supplied function could do anything. The input this function is
#'   the raw dataframe of data that will be considered for one model fit. It is
#'   guaranteed to have at least a `time` and `count` column. It is possible to
#'
#' @returns a list with 2 entries - `model_fn` and `predict` suitable as the
#'   input for `poisson_gam_model(model_fn = ..., predict=...)`.
#' @export
#' @concept models
#'
#' @examples
#'
#' data = test_delayed_observation %>% dplyr::group_by(obs_time)
#' cfg = gam_delayed_reporting(14,40)
#' fit = cfg$model_fn(data)
#' summary(fit)
#'
gam_delayed_reporting = function(
  window,
  max_delay = 40,
  ...,
  knots_fn = ~ gam_knots(.x, window, ...)
) {
  knots_fn = rlang::as_function(knots_fn)

  return(list(
    model_fn = function(data = i_censored_incidence_input) {
      data = interfacer::ivalidate(data)

      data = data %>%
        dplyr::mutate(tau = pmin(max_delay, as.numeric(obs_time - time))) %>%
        dplyr::filter(tau > 0)

      kts = knots_fn(data)

      return(
        mgcv::gam(
          count ~
            s(time, bs = "cr", k = length(kts)) +
              s(log(tau), k = 4, pc = max_delay),
          data = data,
          knots = list(time = kts),
          method = "REML",
          mgcv::nb()
        )
      )
    },
    predict = list(tau = max_delay)
  ))
}

#TODO: GAM weekday effect model.

#' Derive a set of knot points for a GAM from data
#'
#' At the moment this does nothing sophisticated. An mostly equally spaced grid
#' of knots with gaps at start and end to prevent over-fitting there. In the
#' future this could look at the number of observations or areas where there is
#' a lot of change to add in more knots.
#'
#' @iparam data the function will be called with incidence data
#' @param window the spacing between knots
#' @param k alternative to `window`, if `k` is given then the behaviour of the
#' knots will be similar to the default `mgcv::s(...,k=...)` parameter.
#' @param ... currently not used
#'
#' @returns a vector of times (as a numeric)
#' @export
#' @concept models
#'
#' @examples
#' gam_knots(ggoutbreak::test_poisson_rt, 14)
#' gam_knots(ggoutbreak::test_poisson_rt, k=10)
gam_knots = function(data = i_incidence_data, window, ..., k = NULL) {
  range = range(as.numeric(data$time))
  if (!is.null(k)) {
    return(seq(range[1], range[2], length.out = k))
  }
  diff = (range[2] - range[1])
  if (diff / window < 8) {
    window = diff %/% 8
  }
  c(
    # range[1] + window %/% 2,
    seq(range[1] + window, range[2] - window, window),
    range[2] - window %/% 2
  )
}
