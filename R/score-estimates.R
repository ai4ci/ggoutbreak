#' Identify estimate lags in a model
#'
#' A specific parameter or set of parameters can be estimated by a `pipeline`.
#' This function applies the `pipeline` to a synthetic epidemic with sawtooth
#' incidence resulting from a stepped growth rate function. The lag between
#' synthetic input and estimate is assessed by minimising the root mean square
#' error of input and estimated based on different lag offsets.
#'
#' @param pipeline a function taking an input dataset and an infectivity profile
#'   as inputs and producing an estimate as output. This is the whole
#'   parametrised pipeline including any other inputs. This can be a `purrr`
#'   style function, in which case the `.x` variable is the input dataset and
#'   `.y` is the infectivity profile.
#' @iparam ip the infectivity profile.
#' @param lags a vector with the delays to test. Defaults to -10 to +30 days
#'
#' @return a lag analysis dataframe containing the `estimate` type and the `lag`
#'   in days that the estimate is behind the actual observation
#' @export
#' @concept test
#'
#' @examples
#' # lags from a locfit incidence model with Rt estimation.
#' # This model has no estimator lag:
#' pipeline = ~ .x %>% poisson_locfit_model() %>% rt_from_incidence(ip = .y)
#' quantify_lag(pipeline, ip = test_ip)
#'
#' # lags from an epiestim Rt estimation
#' # this model's lags depend on the infectivity profile.
#' # In this case it is 8 days
#' pipeline2 =  ~ .x %>% rt_epiestim(ip = .y)
#' quantify_lag(pipeline2, ip=test_ip )
#'
quantify_lag = function(pipeline, ip = i_empirical_ip, lags = -10:30) {
  ip = summarise_ip(ip)
  # deterministic sawtooth function based on IP
  data = sim_test_data(ip = ip)

  pipeline = rlang::as_function(pipeline)

  est = pipeline(data, ip)

  tmp = .long_quantiles(est) %>% dplyr::filter(.p == 0.5) %>% dplyr::select(-.p)
  tmp2 = data %>%
    dplyr::select(-count, -denom) %>%
    tidyr::pivot_longer(
      cols = c(growth, incidence, rt, proportion, relative.growth),
      names_to = ".type",
      values_to = "ref"
    ) %>%
    dplyr::filter(!is.na(ref))

  test = tmp %>%
    dplyr::inner_join(tmp2, by = c("time", ".type")) %>%
    dplyr::group_by(.type)

  lag_data = dplyr::bind_rows(lapply(lags, function(lag) {
    test %>%
      dplyr::summarise(
        lagged_rmse = sqrt(mean(
          (.lag(ref, n = lag, default = NA) - .value)^2,
          na.rm = TRUE
        )),
        lag = lag
      )
  })) %>%
    dplyr::group_by(estimate = .type) %>%
    dplyr::mutate(lagged_rmse = lagged_rmse / min(lagged_rmse))

  lag_summ = lag_data %>%
    dplyr::arrange(lagged_rmse) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::select(estimate, lag)

  p = ggplot2::ggplot(
    lag_data,
    ggplot2::aes(x = lag, y = lagged_rmse, colour = estimate)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::xlab("estimate lag (days)") +
    ggplot2::ylab("relative RMSE")

  return(structure(
    lag_summ,
    plot = p,
    data = lag_data
  ))
}

# bidirectional lag function.
.lag = function(x, n, ...) {
  if (n < 0) {
    return(dplyr::lead(x, n = -n, ...))
  } else {
    return(dplyr::lag(x, n = n, ...))
  }
}

# matches column names like incidence.0.25 = c(0.1)
# maps column names to 2 cols .type=incidence and
# .p = 0.25 and .value = 0.1.
# works across groups and different column names.
.long_quantiles = function(est, nest = FALSE) {
  grps = est %>% dplyr::groups()
  tmp = est %>%
    dplyr::select(
      !!!grps,
      time,
      dplyr::matches("\\.0\\.[0-9]+$")
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("\\.0\\.[0-9]+$"),
      names_pattern = "^(.*)\\.(0\\.[0-9]+$)",
      names_to = c(".type", ".p"),
      values_to = ".value"
    ) %>%
    dplyr::mutate(
      .p = as.numeric(.p)
    )
  if (nest) {
    tmp = tmp %>% tidyr::nest(quantiles = c(.p, .value))
  }
  return(tmp)
}

# Convert standard output format to a long format by estimate type with nested
# quantiles
.long_types = function(est) {
  grps = est %>% dplyr::groups()
  types = stringr::str_extract(colnames(est), "([^\\.]*)?\\..*", group = 1)
  types = unique(stats::na.omit(types))
  dplyr::bind_rows(
    lapply(types, function(t) {
      est %>%
        dplyr::select(!!!grps, time, dplyr::starts_with(t)) %>%
        dplyr::mutate(
          .type = t,
          median = .data[[paste0(t, ".0.5")]],
          lower_quartile = .data[[paste0(t, ".0.25")]],
          upper_quartile = .data[[paste0(t, ".0.75")]]
        ) %>%
        tidyr::pivot_longer(
          cols = dplyr::matches("\\.0\\.[0-9]+$"),
          names_pattern = "^.*\\.(0\\.[0-9]+$)",
          names_to = c("p"),
          values_to = "x"
        ) %>%
        dplyr::mutate(p = as.numeric(p)) %>%
        tidyr::nest(quantiles = c(p, x)) %>%
        dplyr::rename_with(.cols = dplyr::starts_with(t), .fn = function(n) {
          stringr::str_remove(n, paste0(t, "\\."))
        })
    })
  )
}

#' Calculate scoring statistics from predictions.
#'
#' This performs a range of continuous scoring metrics for each estimate
#' time-point using cumulative distribution functions for each estimate. Point
#' quality metrics are calculated for each estimate provided and summarised.
#' Summarisation is performed using bootstrap resampling to generate confidence
#' intervals for summary statistics, which are presented as median +/1 95% CI.
#'
#' @param est a dataframe of estimates of incidence, growth rate of reproduction
#'   number based off a simulation or data with known parameters. Each group in
#'   `est` is expected to contain multiple estimates and each group is scored
#'   separately. Estimates in `est` must be in the form of a column named
#'   `XXX.cdf` containing a cumulative distribution function for the estimate
#'   and `XXX.link` containing a link function specification (one if
#'   `identity`,`log` or `logit`). These are not generated by default by the
#'   `ggoutbreak` estimators but are triggered by setting the option:
#'   `options("ggoutbreak.keep_cdf"=TRUE)` before running the estimator. The
#'   CDFs generated will be analytical, if the estimator generates a
#'   parametrised output (or a mixture thereof), empirical if the estimator uses
#'   resampling, or inferred if the estimator produces quantiles only.
#' @param obs a dataframe of the ground truth, sharing the same grouping and
#'   columns as `est` with at least one column(s) named `XXX.obs` with `XXX`
#'   being e.g. `rt`,`growth` or `incidence` or any other column group
#'   predicted in `est` (i.e. if `obs` has a column `XXX.obs`, `est` must have
#'   one called `XXX.cdf`).
#' @param lags a data frame of estimate types and lags as output by
#'   [quantify_lag()] if multiple models are included then the columns must
#'   match those in `obs`. It must have 2 columns, one called `estimate` with
#'   values matching `incidence`,`rt`,`growth`,`proportion`,`relative.growth`,
#'   and a `lag` column, with (whole) number of days.
#' @param summarise_by by default every group is treated separately. This can be
#'   overridden with a `dplyr` specification of the groupings we want to
#'   see in the final summarised output (e.g. if we want to differentiate
#'   performance on a particular type of scenario or timeframe). If this is
#'   exactly `FALSE` the function will return all the raw point estimates.
#' @param bootstraps the number of bootstrap replicates to draw for assessing
#'   metric confidence. If FALSE then no bootstrapping will be done and the
#'   metrics returned will have no confidence intervals.
#' @param raw_bootstraps (defaults to FALSE) return the summary metrics for
#'   each bootstrap rather than the quantiles of the summary metrics.
#' @param seed a random seed for reproducibility
#'
#' @return a dataframe of scoring metrics, with one row per group. This includes
#'   the following columns:
#'
#' * mean_quantile_bias - the average of the universal residuals. Lower values
#'   are better.
#' * mean_trans_bias - the bias on the link function scale.
#' * link - the link function
#' * mean_bias - the bias on the natural scale (which may be interpreted as
#'   additive or multiplicative depending on the link)
#' * pit_was - an unadjusted probability integral transform histogram
#'   Wasserstein distance from the uniform (lower values are better).
#' * unbiased_pit_was - an PIT Wasserstein distance from the uniform, adjusted
#'   for estimator bias (lower values are better). This is a measure of
#'   calibration.
#' * directed_pit_was - a PIT Wasserstein distance from the uniform, directed
#'   away from the centre, adjusted for estimator bias (values closer to zero
#'   are better, positive values indicate overconfidence, and negative values
#'   excessively conservative estimates).
#' * percent_iqr_coverage - the percentage of estimators that include the true
#'   value in their IQR. For a perfectly calibrated estimate this should be 0.5.
#'   Lower values reflect overconfidence, higher values reflect excessively
#'   conservative estimates. This is a measure of calibration but is influenced
#'   by bias.
#' * unbiased_percent_iqr_coverage - the percentage of estimators that include
#'   the true value in their IQR once adjusted for bias. This should be 0.5. This
#'   is a measure of calibration, and tells you which direction (smaller numbers
#'   are over-confident, larger values excessively conservative).
#' * mean_prediction_interval_width_50 - the prediction interval width is a
#'   measure of sharpness (smaller values are sharper). Sharper estimators are
#'   superior if they are unbiased and well calibrated.
#' * mean_crps - the mean value of the continuous rank probability score for
#'   each point estimate (lower values are better)
#' * mean_unbiased_crps - the mean value of the continuous rank probability
#'   score for each point estimate assessed after adjustment for bias (lower
#'   values are better)
#' * threshold_misclassification_probability - if a metric has a natural threshold
#'   like 1 for Rt then this measures how probable it is that the estimate will
#'   propose the epidemic is shrinking when it is growing and vice versa. Lower is
#'   better
#'
#' other outputs are possible if `summarise_by` is false.
#'
#' @export
#' @concept test
#'
#' @examples
#' data = test_poisson_rt_smooth
#'
#' pipeline = ~ .x %>% poisson_locfit_model() %>% rt_from_incidence(ip = .y)
#' lags = quantify_lag(pipeline, ip = test_ip)
#'
#' withr::with_options(list("ggoutbreak.keep_cdf"=TRUE),{
#'    est = data %>% poisson_locfit_model() %>% rt_from_incidence()
#' })
#'
#' if (interactive()) plot_rt(est)+sim_geom_function(data, colour="red")
#'
#' obs = data %>% dplyr::mutate(rt.obs = rt, incidence.obs = rate)
#' score_estimate(est,obs,lags) %>% dplyr::glimpse()
#'
score_estimate = function(
  est,
  obs,
  lags = NULL,
  summarise_by = est %>% dplyr::groups(),
  bootstraps = 1000,
  raw_bootstraps = FALSE,
  seed = 100
  # cores = NULL
) {
  if (is.null(lags)) {
    lags = tibble::tibble(
      estimate = c(
        "incidence",
        "rt",
        "growth",
        "proportion",
        "relative.growth"
      ),
      lag = 0
    )
  }

  # if (is.null(cores) && requireNamespace("parallel", quietly = TRUE)) {
  #   cores = parallel::detectCores()
  # }
  #
  # if (!is.null(cores)) {
  #   if (requireNamespace("mirai", quietly = TRUE)) {
  #     if (!mirai::daemons_set()) mirai::daemons(cores)
  #   }
  # }

  join_cols = intersect(colnames(est), colnames(obs))

  .message_once(
    "estimates match true observations using columns: ",
    paste0(join_cols, collapse = ",")
  )

  join_cols_2 = intersect(colnames(obs), colnames(lags))

  obs_cols = setdiff(colnames(obs), colnames(est))
  obs_type = stringr::str_extract(obs_cols, "(.*)\\.obs", 1)
  obs_type = obs_type[!is.na(obs_type)]

  .message_once(
    "matching ground truth observations for: ",
    paste0(obs_type, collapse = ",")
  )

  long_obs = obs %>%
    dplyr::select(
      dplyr::all_of(join_cols),
      dplyr::ends_with(".obs")
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with(".obs"),
      names_to = ".type",
      values_to = "ref",
      names_pattern = "([^\\.]+)\\.obs"
    ) %>%
    dplyr::left_join(lags, by = c(join_cols_2, ".type" = "estimate")) %>%
    # shift the observed to meet the estimate.
    dplyr::mutate(
      time = time + ifelse(is.na(lag), 0, round(lag)),
      cutoff = dplyr::case_when(
        .type == "rt" ~ 1,
        .type == "growth" ~ 0,
        .type == "relative.growth" ~ 0,
        TRUE ~ NA
      )
    )

  crps_data = est %>%
    .long_types() %>%
    dplyr::filter(.type %in% obs_type)

  if (!"cdf" %in% colnames(crps_data)) {
    stop(
      "No CDFs found for estimates. Re-run the esimator after setting `options(\"ggoutbreak.keep_cdf\"=TRUE)`"
    )
  }

  crps_data = crps_data %>%
    dplyr::inner_join(
      long_obs,
      by = c(join_cols, ".type")
    ) %>%
    # dplyr::group_by(!!!summarise_by, .type) %>%
    dplyr::group_by(link) %>%
    dplyr::group_modify(function(d, g, ...) {
      # d=crps_data %>% filter(statistic=="infections" & .type=="rt") %>% ungroup() %>% select(-.type,-statistic)
      link = unique(g$link)
      trans = .trans_fn(link)
      inv = .inv_fn(link)
      low = .min_domain(link)
      high = .max_domain(link)

      mean_trans_bias = mean(trans(d$median) - trans(d$ref), na.rm = TRUE)

      d = d %>%
        dplyr::mutate(
          crps = .crps(ref, cdf, low, high, quantiles),
          pit0 = .pit(cutoff, cdf),
          threshold_error_probability = abs(pit0 - ifelse(ref <= cutoff, 1, 0)),
          threshold_distance = abs(trans(ref) - trans(cutoff)),
          # bias:
          unbiased_ref = inv(trans(ref) + mean_trans_bias),
          quantile_bias = .quantile_bias(ref, cdf),
          # sharpness:
          # prediction_variance = .variance(cdf, quantiles),
          prediction_interval_width_50 = upper_quartile - lower_quartile,
          # calibration:
          pit = .pit(ref, cdf),
          unbiased_pit = .pit(unbiased_ref, cdf),
          iqr_coverage = pit > 0.25 & pit < 0.75,
          unbiased_iqr_coverage = unbiased_pit > 0.25 & unbiased_pit < 0.75
        ) %>%
        dplyr::select(
          -cdf
        )

      return(d)
    })

  if (isFALSE(summarise_by)) {
    return(crps_data %>% dplyr::group_by(!!!dplyr::groups(est), .type))
  }

  crps_summary = crps_data %>%
    dplyr::group_by(!!!summarise_by, .type) %>%
    dplyr::group_modify(function(d, g, ...) {
      link = unique(d$link)
      trans = .trans_fn(link)
      inv = .inv_fn(link)
      low = .min_domain(link)
      high = .max_domain(link)

      n = nrow(d)
      unif = seq(1 / (2 * n), 1 - 1 / (2 * n), length.out = n)

      # d = d %>% filter(statistic=="infections") %>% ungroup() %>% select(-statistic)

      if (isFALSE(bootstraps)) {
        raw_bootstraps = TRUE
        bootstraps = 1
      }
      # bootstrap resampling:
      withr::with_seed(seed, {
        boots = dplyr::bind_rows(
          purrr::map(
            1:bootstraps + seed,
            # purrr::in_parallel(
            function(seed) {
              set.seed(seed)
              d |>
                dplyr::slice_sample(prop = 1, replace = TRUE) |>
                dplyr::summarise(
                  mean_crps = mean(crps),
                  threshold_misclassification_probability = stats::weighted.mean(
                    threshold_error_probability,
                    threshold_distance
                  ),
                  # bias
                  mean_trans_bias = mean(
                    trans(median) - trans(ref),
                    na.rm = TRUE
                  ),
                  mean_bias = mean(
                    inv(trans(median) - trans(ref)),
                    na.rm = TRUE
                  ),
                  mean_quantile_bias = mean(quantile_bias),
                  # sharpness:
                  # mean_prediction_variance = mean(prediction_variance),
                  mean_prediction_interval_width_50 = mean(
                    prediction_interval_width_50
                  ),
                  # calibration
                  pit_was = mean(abs(unif - sort(pit))),
                  unbiased_pit_was = mean(abs(unif - sort(unbiased_pit))),
                  directed_pit_was = mean(
                    (unif - sort(unbiased_pit)) * ifelse(unif < 0.5, 1, -1)
                  ),
                  percent_iqr_coverage = mean(iqr_coverage),
                  unbiased_percent_iqr_coverage = mean(unbiased_iqr_coverage),
                )
              # }
            }
            # ,
            #   d = d,
            #   unif = unif,
            #   inv = inv,
            #   trans = trans
            # )
          )
        )
      })

      if (!raw_bootstraps) {
        boots = boots %>%
          dplyr::summarise(
            dplyr::across(
              .cols = dplyr::everything(),
              .fns = list(
                `0.025` = ~ stats::quantile(.x, 0.025, na.rm = TRUE),
                `0.25` = ~ stats::quantile(.x, 0.25, na.rm = TRUE),
                `0.5` = ~ stats::quantile(.x, 0.5, na.rm = TRUE),
                `0.75` = ~ stats::quantile(.x, 0.75, na.rm = TRUE),
                `0.975` = ~ stats::quantile(.x, 0.975, na.rm = TRUE)
              ),
              .names = "{.col}.{.fn}"
            )
          )
      }

      boots = boots %>% dplyr::mutate(link = link)

      return(boots)
    })

  return(crps_summary)

  # ScoringUtils integration:
  # long_est = .long_quantiles(est)
  # scores = long_est %>%
  #   dplyr::inner_join(long_obs, by=c(join_cols,".type")) %>%
  #   dplyr::rename(true_value = ref, prediction = .value, quantile = .p)
  #
  # if(!"model" %in% colnames(scores)) scores = scores %>% dplyr::mutate(model="undefined")
  #
  # scores = scores %>%
  #   # dplyr::group_by(model, .add = TRUE) %>%
  #   dplyr::group_modify(function(d,g,...) {
  #
  #     tmp = scoringutils::as_forecast_quantile(
  #       d %>% dplyr::rename(predicted = prediction, observed=true_value, quantile_level = quantile))
  #
  #     metrics = scoringutils::get_metrics(tmp)
  #     metrics = metrics[!names(metrics) %in% c("crps","bias")]
  #
  #     return(
  #       tmp %>%
  #         scoringutils::score(metrics = metrics) %>%
  #         scoringutils::summarise_scores(by = c("model", ".type", "time"))
  #     )
  #   }) %>%
  #   dplyr::left_join(crps_data %>% dplyr::select(-lag), by=c(join_cols,".type")) %>%
  #   dplyr::rename(true_value = ref, estimate=.type) %>%
  #   dplyr::group_by(model,!!!(est %>% dplyr::groups()),estimate)
  # return(scores)
}


# vectorised CRPS calculation given list of CDFs list of quantile df and true values
# tmp2 = .cdf_generator(mean=list(c(1,2,3),c(3,4,5)),sd=list(c(1,1,1),c(2,2,2)))
# .crps(c(2,2), tmp2, min=-100,max=100)
.crps = function(true, cdf, min = -Inf, max = Inf, quantiles) {
  if (is.function(cdf)) {
    cdf = list(cdf)
  }
  interfacer::check_numeric(min, max)
  # interfacer::recycle(true, cdf, min, max, quantiles)
  purrr::map2_dbl(
    true,
    cdf,
    # purrr::in_parallel(
    # does not speed up.
    function(x, fn) {
      # ps = quantiles[[i]]$p
      # xs = quantiles[[i]]$x
      #
      # y = fn(x)
      # xs = c(xs,x)[order(c(ps,y))]
      # ps = sort(c(ps,y))

      tryCatch(
        stats::integrate(
          f = function(y) fn(y)^2,
          lower = min,
          upper = x,
          stop.on.error = FALSE,
          subdivisions = 20
        )$value +
          stats::integrate(
            f = function(y) (fn(y) - 1)^2,
            lower = x,
            upper = max,
            stop.on.error = FALSE,
            subdivisions = 20
          )$value,
        error = function(e) {
          NA
        }
      )
    }
    #   },
    #   min = min,
    #   max = max
    # )
  )
}

.quantile_bias = function(true, cdf) {
  # interfacer::recycle(true, cdf)
  purrr::map_dbl(seq_along(true), function(i) {
    x = true[[i]]
    fn = cdf[[i]]
    return(1 - 2 * fn(x))
  })
}

.pit = function(true, cdf) {
  # interfacer::recycle(true, cdf)
  purrr::map_dbl(seq_along(true), function(i) {
    x = true[[i]]
    fn = cdf[[i]]
    return(fn(x))
  })
}

# .variance = function(cdf,quantiles, min = -Inf, max=Inf) {
#   interfacer::recycle(cdf,quantiles,min,max)
#   purrr::map_dbl(seq_along(cdf), function(i){
#     fn=cdf[[i]]
#     p = quantiles[[i]]$p
#     x = quantiles[[i]]$x
#     mu = integrate(function(x) x*fn(x), min, max, subdivisions = 30)
#     v = 2 * integrate(function(x) x*(1-fn(x)+fn(-x)), 0, max, subdivisions = 30) - mu^2
#
#   })
# }
