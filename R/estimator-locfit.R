# LOCFIT polynomial models ----

.nn_from_window = function(window, d) {
  nn = 2 * window / nrow(d)
  if (nn < 8 / nrow(d)) {
    nn = 8 / nrow(d)
    warning("window is too small for data. using: ", nn / 2 * nrow(d))
  }
  return(nn)
}


#' A binomial proportion estimate and associated exponential growth rate
#'
#' takes a list of times, counts and a denominator and fits a quasi-binomial model
#' using a logit link function to proportion data using local regression
#' using the package `locfit`.
#'
#' This expects d to contain one combination of:
#' * `time` and `count` and `denom` columns - e.g. all tests conducted.
#'
#' This results is a one versus others comparison binomial proportion estimate plus a
#' relative growth rate estimate specifying how much quicker this is growing
#' compared to the growth of the denominator.
#'
#' The denominator maybe the sum of all subgroups `denom = sum(count)`, e.g. in
#' the situation where there are multiple variants of a disease circulating. In
#' which case the relative growth is that of the subgroup compared to the
#' overall. You can make this a one-versus-others comparison by making the
#' denominator exclude the current item (e.g. `denom = sum(count)-count`).
#'
#' The denominator can also be used to express the size of the population tested.
#' This gives us a relative growth rate that is different in essence to the previous
#' and may be a better estimate of the true growth rate in the situation where
#' testing effort is variable, or capacity saturated.
#'
#'
#' @iparam d the input
#' @inheritParams poisson_locfit_model
#'
#' @return `r i_proportion_rate`
#' @export
#' @concept models
#'
#' @examples
#'
#' data = example_poisson_rt_2class()
#'
#' tmp2 = data %>% proportion_locfit_model(window=7,deg=2)
#' tmp3 = data %>% proportion_locfit_model(window=14,deg=1)
#'
#' comp = dplyr::bind_rows(
#'   tmp2 %>% dplyr::mutate(model="7:2"),
#'   tmp3 %>% dplyr::mutate(model="14:1"),
#' ) %>% dplyr::group_by(model,class)
#'
#' if (interactive()) {
#'   plot_proportion(
#'     comp,
#'     date_labels="%b %y",
#'     mapping=ggplot2::aes(colour=model),
#'     raw=data
#'   )+ggplot2::facet_wrap(~class)
#' }
#'
proportion_locfit_model = function(
  d = i_proportion_input,
  ...,
  window = 14,
  deg = 1, # The proportions model is quite sensitive to this parameter
  frequency = "1 day",
  predict = TRUE,
  .progress = interactive()
) {
  env = rlang::current_env()
  if (.progress) {
    cli::cli_progress_bar(
      "proportion & growth rate (locfit)",
      total = dplyr::n_groups(d),
      .envir = env
    )
  }
  .message_context()

  modelled = interfacer::igroup_process(
    d,
    function(d, ..., window, deg, frequency, predict) {
      output_times = date_seq.time_period(d$time, period = frequency)
      nn = .nn_from_window(window, d)

      # Not enough non zero results
      if (sum(stats::na.omit(d$count) != 0) < deg) {
        # tmp = binom::binom.wilson(sum(d$count,na.rm = TRUE),sum(d$denom,na.rm = TRUE))
        # return(.null_result(output_times, proportion=c(tmp$lower,tmp$mean,tmp$upper), relative.growth=c(NA,0,NA)))
        # rolling binomial
        return(.null_result(
          output_times,
          proportion = c(0, 0, 0),
          relative.growth = c(NA, 0, NA)
        ))
      }

      # Not enough non one results
      #TODO: calculate CIs based on binomial.
      if (sum(stats::na.omit(d$count != d$denom)) < deg) {
        return(.null_result(
          output_times,
          proportion = c(1, 1, 1),
          relative.growth = c(NA, 0, NA)
        ))
      }

      # y = cbind(d$count, d$denom - d$count)
      # fit = suppressWarnings(locfit::locfit(y~locfit::lp(time,nn=nn,deg=deg),data = d,family="qbinomial", link="logit", maxit = 5000, maxk=5000))
      # deriv = suppressWarnings(locfit::locfit(y~locfit::lp(time,nn=nn,deg=deg),deriv=1,data = d,family="qbinomial", link="logit", maxit = 5000, maxk=5000))

      fit = withCallingHandlers(
        {
          locfit::locfit(
            count ~ locfit::lp(time, nn = nn, deg = deg),
            weights = denom,
            data = d,
            family = "qbinomial",
            link = "logit",
            maxit = 5000,
            maxk = 5000
          )
        },
        warning = .rewrite_lfproc
      )

      deriv = withCallingHandlers(
        {
          locfit::locfit(
            count ~ locfit::lp(time, nn = nn, deg = deg),
            weights = denom,
            deriv = 1,
            data = d,
            family = "qbinomial",
            link = "logit",
            maxit = 5000,
            maxk = 5000
          )
        },
        warning = .rewrite_lfproc
      )

      if (!predict) {
        return(dplyr::tibble(
          proportion = list(fit),
          relative.growth = list(deriv)
        ))
      }

      # proportion
      tmp = stats::preplot(
        fit,
        newdata = output_times,
        se.fit = TRUE,
        band = "global"
      )
      # logit transformer function:
      t = tmp$tr

      # growth rate
      tmp2 = stats::preplot(
        deriv,
        newdata = output_times,
        se.fit = TRUE,
        band = "global"
      )
      t2 = function(x) x

      prop = dplyr::tibble(
        time = output_times
      ) %>%
        .result_from_fit(type = "proportion", tmp$fit, tmp$se.fit, t) %>%
        .keep_cdf(
          type = "proportion",
          mean = tmp$fit,
          sd = tmp$se.fit,
          link = "logit"
        ) %>%
        .result_from_fit(
          type = "relative.growth",
          tmp2$fit,
          tmp2$se.fit,
          t2
        ) %>%
        .keep_cdf(type = "relative.growth", mean = tmp2$fit, sd = tmp2$se.fit)

      if (.progress) {
        cli::cli_progress_update(.envir = env)
      }

      return(prop)
    }
  )

  if (.progress) {
    cli::cli_progress_done()
  }

  return(modelled)
}


#' Poisson time-series model.
#'
#' Takes a list of times and counts and fits a quasi-poisson model
#' fitted with a log link function to count data using local regression
#' using the package `locfit`.
#'
#' This results is an incidence rate estimate plus an absolute exponential
#' growth rate estimate both based on the time unit of the input data (e.g. for
#' daily data the rate will be cases per day and the growth rate will be daily).
#'
#' @iparam d input data
#' @iparam ... not used and present to allow proportion model to be used in a
#'   `group_modify`
#' @iparam window a number of data points defining the bandwidth of the estimate,
#'   smaller values result in less smoothing, large value in more. The default
#'   value of 14 is calibrated for data provided on a daily frequency, with
#'   weekly data a lower value may be preferred.
#' @iparam deg polynomial degree (min 1) - higher degree results in less
#'   smoothing, lower values result in more smoothing. A degree of 1 is fitting
#'   a linear model piece wise.
#' @iparam frequency the density of the output estimates as a time period such as
#'   `7 days` or `2 weeks`.
#' @iparam ip An infectivity profile (optional) if not given (the default) the
#'   Rt value will not be estimated
#' @param quick if `ip` is provided, and quick is `TRUE` Rt estimation will be
#'   done assuming independence which is quicker but less accurate. Setting this
#'   to false will use a full variance-covariance matrix.
#' @iparam predict result is a prediction dataframe. If false we return the
#'   `locfit` models (advanced).
#' @param .progress show a CLI progress bar
#'
#' @return `r i_incidence_rate`
#'
#' @export
#' @concept models
#'
#' @examples
#'
#'
#' data = example_poisson_rt()
#' tmp = data %>% poisson_locfit_model(window=14,deg=2, ip=example_ip(), quick=TRUE)
#' plot_rt(tmp,
#'     raw=data,
#'     true_col = rt)
#'
#' data = example_poisson_growth_rate()
#' tmp2 = data %>% poisson_locfit_model(window=7,deg=1)
#' tmp3 = data %>% poisson_locfit_model(window=14,deg=2)
#'
#'
#'
#' comp = dplyr::bind_rows(
#'   tmp2 %>% dplyr::mutate(class="7:1"),
#'   tmp3 %>% dplyr::mutate(class="14:2"),
#' ) %>% dplyr::group_by(class)
#'
#' if (interactive()) {
#'   plot_incidence(
#'     comp,
#'     date_labels="%b %y",
#'     raw=data,
#'     true_col = rate
#'   )
#'
#'   plot_growth_rate(
#'     comp,
#'     date_labels="%b %y",
#'     raw = data,
#'     true_col = growth
#'   )
#'   # sim_geom_function(data,colour="black")
#' }
#'
poisson_locfit_model = function(
  d = i_incidence_input,
  ...,
  window = 14,
  deg = 2,
  frequency = "1 day",
  predict = TRUE,
  ip = i_discrete_ip,
  quick = FALSE,
  .progress = interactive()
) {
  env = rlang::current_env()
  if (.progress) {
    cli::cli_progress_bar(
      "incidence & growth rate (locfit)",
      total = dplyr::n_groups(d),
      .envir = env
    )
  }
  .message_context()

  modelled = interfacer::igroup_process(
    d,
    function(d, .groupdata, ..., window, deg, frequency, predict) {
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

      nn = .nn_from_window(window, d)
      output_times = pred_time

      # Not enough non zero results
      if (sum(stats::na.omit(d$count) != 0) < deg) {
        return(.null_result(
          output_times,
          incidence = c(0, 0, 0),
          growth = c(NA, 0, NA)
        ))
      }

      fit = withCallingHandlers(
        {
          locfit::locfit(
            count ~ locfit::lp(time, nn = nn, deg = deg),
            data = d,
            family = "qpoisson",
            link = "log"
          )
        },
        warning = .rewrite_lfproc
      )

      deriv = withCallingHandlers(
        {
          locfit::locfit(
            count ~ locfit::lp(time, nn = nn, deg = deg),
            deriv = 1,
            data = d,
            family = "qpoisson",
            link = "log"
          )
        },
        warning = .rewrite_lfproc
      )

      if (!predict) {
        return(dplyr::tibble(incidence = list(fit), growth = list(deriv)))
      }

      pred = stats::preplot(
        fit,
        newdata = output_times,
        se.fit = TRUE,
        band = "local",
        maxit = 5000,
        maxk = 5000
      )
      # transformer function:
      t = pred$tr

      pred2 = stats::preplot(
        deriv,
        newdata = output_times,
        se.fit = TRUE,
        band = "local",
        maxit = 5000,
        maxk = 5000
      )
      t2 = function(x) x

      incid = dplyr::tibble(
        time = output_times
      ) %>%
        .result_from_fit(type = "incidence", pred$fit, pred$se.fit, t) %>%
        .keep_cdf(
          type = "incidence",
          meanlog = pred$fit,
          sdlog = pred$se.fit,
          link = "log"
        ) %>%
        .result_from_fit(type = "growth", pred2$fit, pred2$se.fit, t2) %>%
        .keep_cdf(type = "growth", mean = pred2$fit, sd = pred2$se.fit)

      if (.progress) {
        cli::cli_progress_update(.envir = env)
      }

      if (do_rt) {
        if (quick) {
          .message_once(
            "Rt estimation using Locfit (approx and assuming independence)"
          )
          pred_vcov = NULL
        } else {
          .message_once(
            "Rt estimation using Locfit (exact with inferred covariance)"
          )
          # TODO: infer covariance
          linked = incid %>% dplyr::left_join(d, by = "time")
          pred_vcov = vcov_from_residuals(
            linked$count,
            linked$incidence.fit,
            linked$incidence.se.fit
          )$vcov_matrix
          sigma = linked$incidence.se.fit
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

        incid = incid %>%
          dplyr::left_join(rt, by = "time") %>%
          .tidy_fit("rt", rt.se.fit > 0.5)
      }

      incid = incid %>%
        dplyr::filter(time >= cutoff) %>%
        .tidy_fit("incidence", incidence.se.fit > 4) %>%
        .tidy_fit("growth", growth.se.fit > 0.25)
      return(incid)
    }
  )

  if (.progress) {
    cli::cli_progress_done()
  }

  return(modelled %>% .normalise_from_raw(d))
}

# Catch locfit warnings and display a more relevant warning

.rewrite_lfproc = function(w) {
  if (stringr::str_ends(w$message, "parameters out of bounds")) {
    .message_once(
      "incomplete fit locfit model - try decreasing `deg` or increasing `window`."
    )
    rlang::cnd_muffle(w)
  } else if (stringr::str_ends(w$message, "perfect fit")) {
    .message_once(
      "an exact fit was detected, this may mean a denominator error has been made, or synthetic data is being used."
    )
    rlang::cnd_muffle(w)
  }
}

# Fix issues with time series based on a rule
# value - the columns to change to NA - e.g. "incidence"
# rule - the trigger to change the columns
.tidy_fit = function(df, value, rule) {
  # Disabled:
  return(df)
  # rule = dplyr::enexpr(rule)
  #
  # # When does this trigger?
  # # if (
  # #   any(
  # #     df %>%
  # #       dplyr::summarise(dplyr::across(
  # #         dplyr::starts_with(value),
  # #         ~ any(!!rule)
  # #       ))
  # #   )
  # # ) {
  # #   browser()
  # # }
  #
  # df %>%
  #   dplyr::mutate(dplyr::across(
  #     dplyr::starts_with(value),
  #     function(.x) {
  #       if (any(!!rule, na.rm = TRUE)) {
  #         .message_once(sprintf(
  #           "removed some anomalous %s estimates (%s)",
  #           value,
  #           rlang::as_label(rule)
  #         ))
  #       }
  #       ifelse(!!rule, NA_real_, .x)
  #     }
  #   ))
}
