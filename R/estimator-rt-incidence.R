#' Reproduction number from modelled incidence
#'
#' Calculate a reproduction number estimate from modelled incidence using the
#' methods described in the vignette "Estimating the reproduction number from
#' modelled incidence" and using a set of empirical generation time
#' distributions. This assumes that modelled incidence has the same time unit as
#' the `ip` distribution, and that this is daily, if this is not the case then
#' [rescale_model()] may be able to fix it.
#'
#' N.B. for certain estimators (e.g. [poisson_gam_model()],
#' [poisson_locfit_model()]) a version of this function will be called
#' automatically if the infectivity profile is supplied. The inbuilt version is
#' preferred over this function for these estimators, as the full covariance
#' matrix may be used and the initial part of the outbreak can be predicted more
#' accurately. This function is for supporting the other count models in
#' `ggoutbreak`. If you are rolling your own incidence estimates and want an Rt
#' estimate then [rt_incidence_timeseries_implementation()] maybe better suited
#' to your task.
#'
#' @iparam df modelled incidence estimate
#' @iparam ip an infectivity profile (aka generation time distribution)
#' @iparam raw the raw data that the modelled incidence is based on. This is
#'   optional. If not given the algorithm will assume independence, which will
#'   be faster to estimate (much faster for long time-series), but with more
#'   uncertain Rt estimates. In some circumstances this assumption of
#'   independence can cause underestimation of Rt. If this is a risk there will
#'   be a warning given, and this parameter may need to be supplied.
#' @param approx use a faster, but approximate, estimate of quantiles
#' @param .progress show a CLI progress bar
#'
#' @return `r i_reproduction_number`
#' @export
#' @concept models
#' @examples
#'
#' tmp = example_poisson_rt_smooth() %>%
#'   poisson_locfit_model() %>%
#'   rt_from_incidence(
#'     ip = example_ip(),
#'     raw = example_poisson_rt_smooth(),
#'     approx=FALSE
#'  )
#'
#' # This will assume independence and
#' tmp2 = example_poisson_rt_smooth()%>%
#'   poisson_locfit_model() %>%
#'   rt_from_incidence(ip = example_ip(), approx=TRUE)
#'
#' plot_data = dplyr::bind_rows(
#'   tmp %>% dplyr::mutate(class = "exact"),
#'   tmp2 %>% dplyr::mutate(class = "approx"),
#' ) %>% dplyr::group_by(class)
#'
#' if (interactive()) {
#'   plot_rt(plot_data, date_labels="%b %y")+
#'    sim_geom_function(example_poisson_rt_smooth())+
#'    ggplot2::coord_cartesian(ylim=c(0.5,3))+
#'    ggplot2::facet_wrap(~class)
#' }
#'
rt_from_incidence = function(
  df = i_incidence_model,
  ip = i_discrete_ip,
  raw = i_incidence_data,
  approx = TRUE,
  .progress = interactive()
) {
  #TODO: slow example

  ip = interfacer::ivalidate(ip)

  env = rlang::current_env()
  if (.progress) {
    cli::cli_progress_bar(
      "Rt (modelled incidence)",
      total = dplyr::n_groups(df),
      .envir = env
    )
  }

  # TODO: do residuals to alpha once for all models?
  infer_vcov = !interfacer::imissing(raw)

  if (infer_vcov) {
    raw = interfacer::ivalidate(raw, .default = NULL)
    .message_once("Rt from incidence: inferring vcov from residuals.")
    if (is.null(approx)) {
      approx = FALSE
    }
  } else {
    if (is.null(approx)) {
      approx = TRUE
    }
    if (approx) {
      .message_once(
        "Rt from incidence: assuming independence and approximating quantiles."
      )
    } else {
      .message_once("Rt from incidence: assuming independence.")
    }
  }

  modelled = interfacer::igroup_process(df, function(df, .groupdata, ...) {
    .stop_if_not_daily(df$time)

    tmp_ip = .select_ip(ip, .groupdata)
    mu = df$incidence.fit

    if (infer_vcov) {
      # Infer a vcov from residuals
      data = .select_group(raw, .groupdata)
      linked = df %>% dplyr::inner_join(data, by = "time")
      tmp = vcov_from_residuals(
        linked$count,
        linked$incidence.fit,
        linked$incidence.se.fit
      )
      vcov = tmp$vcov_matrix
      sigma = linked$incidence.se.fit
    } else {
      # just use variance as input to estimate function.
      # This will signify that the algorithm is to assume independence
      sigma = df$incidence.se.fit
      vcov = NULL
    }

    # extend the data set backwards in time by the size of the infectivity profile
    # using a log scale linear model of the first few data points, plus increased
    # uncertainty.
    pad = .ln_pad(
      length = max(ip$tau),
      # time = 1:length(mu),
      time = df$time,
      mu = mu,
      sigma = sigma,
      vcov = vcov
    )

    rt = rt_incidence_timeseries_implementation(
      time = pad$time,
      mu = pad$mu,
      vcov = pad$vcov,
      sigma = pad$sigma,
      ip = tmp_ip,
      tidy = TRUE,
      approx = approx
    )

    df2 = df %>% dplyr::left_join(rt, by = "time")

    if (anyNA(df2)) {
      browser()
    }

    if (.progress) {
      cli::cli_progress_update(.envir = env)
    }

    return(df2)
  })

  if (.progress) {
    cli::cli_progress_done()
  }

  return(modelled)
}

# unsigend log sum exp.
.logsumexp = function(x, na.rm = FALSE) {
  if (!na.rm & any(is.na(x))) {
    return(NA)
  }
  x = x[!is.na(x)]
  if (all(x == -Inf)) {
    return(-Inf)
  }
  c = max(x)
  # remove exp(-Inf) zero terms
  x = x[x != -Inf]
  return(c + log(sum(exp(x - c))))
}


# add a set of log-normals based on mu and sigma with increasing SD to the
# front of a time series.
# mu = 1:10
# sigma2 = rep(0.2,10)
# tmp = .ln_pad(5, 1:10, mu, sigma = sqrt(sigma2))
# exp(tmp$mu + tmp$vcov/2)
# exp(mu + sigma2/2)
.ln_pad = function(
  length,
  time,
  mu,
  sigma = NULL,
  vcov = NULL,
  spread = 1.05
) {
  covariances = !is.null(vcov)
  sigma2 = if (covariances) diag(vcov) else sigma^2

  old_length = length(mu)
  if (length(sigma2) != old_length) {
    stop("mu and vcov must compatible dimensions")
  }

  mean = exp(mu + sigma2 / 2)
  sd0 = sqrt((exp(sigma2[1]) - 1) * exp(2 * mu[1] + sigma2[1]))

  if (old_length > 1) {
    l = min(c(old_length, length))
    means = stats::lm(
      y ~ x,
      dplyr::tibble(y = log(rev(mean[1:l])), x = -(l - 1):0)
    ) %>%
      stats::predict(dplyr::tibble(x = 1:length)) %>%
      exp()
  } else {
    means = rep(mean, length.out = length)
  }

  sds = sd0 * spread^(1:length)
  mus = log(means) - 1 / 2 * log(sds^2 / means^2 + 1)
  sigmas = sqrt(log(sds^2 / means^2 + 1))

  mus = c(rev(mus), mu)
  sigmas = c(rev(sigmas), sqrt(sigma2))
  new_time = c(time[1] - length:1, time)
  new_length = length(mus)
  imputed = c(rep(TRUE, length), rep(FALSE, old_length))

  new_cor_ij = NULL
  if (covariances) {
    if (!all(dim(vcov) == old_length)) {
      stop("vcov must be a square matrix the same dimensions as mu")
    }
    new_cor_ij = unname(diag(sigmas^2))
    new_cor_ij[(length + 1):new_length, (length + 1):new_length] = vcov
  } else {
    new_cor_ij = NULL
  }

  return(list(
    time = new_time,
    mu = unname(mus),
    sigma = unname(sigmas),
    vcov = unname(new_cor_ij),
    imputed = imputed
  ))
}

## Reference implementation (timeseries) ----

#' Time series implementation of the Rt from modelled incidence algorithm
#'
#' This function estimates the reproduction number for a while time series
#' given log-normally distributed incidence estimates, and a set of
#' infectivity profiles. This version of the algorithm is optimised for running
#' over all of a single time series. The algorithm will not produce Rt estimates
#' until there is at least the
#'
#' N.B. the description of this algorithm is given here:
#' https://ai4ci.github.io/ggoutbreak/articles/rt-from-incidence.html
#'
#' @param time a set of time points as a numeric vector. This is a vector of length `k`.
#' @param mu a time series of meanlog parameters of a lognormal distribution of
#'   modelled incidence. This is a vector of length `k`.
#' @param vcov a log scale variance-covariance matrix of predictions. It is
#'   optional but if not given and `sigma_t` is given then this will be inferred
#'   assuming independence between estimates. This should be a matrix with
#'   dimensions `k * k`
#' @param sigma if `vcov` is not given then this must be supplied as the sdlog
#'   of the log normal incidence estimate. If this is the case the estimates
#'   will be made assuming estimate independence. A vector of length `k`. Checks
#'   will be made that determine if there is risk of bias, and this can make
#'   this slower than using the full covariance matrix.
#' @iparam ip a long format infectivity profile dataframe.
#' @param tidy do you want detailed raw output (`FALSE` - default) or summary output
#'   with quantiles predicted.
#' @param ... passed onto output formatter if `tidy=TRUE`, at the moment only
#'   `approx = FALSE`
#'
#' @returns a dataframe with `k` rows with the following columns:
#' - time_Rt: the time point of the Rt estimate (usually `k-1`)
#' - mean_Rt_star: the mean of the Rt estimate
#' - var_Rt_star: the variance of the Rt estimate
#' - meanlog_Rt_star: log normal parameter for approximate distribution of Rt
#' - sdlog_Rt_star: log normal parameter for approximate distribution of Rt
#' - mu_Rt_mix: a list of vectors of log normal parameters for more exact mixture
#'   distribution of Rt
#' - sigma_Rt_mix: a list of vectors of log normal parameters for more exact mixture
#'   distribution of Rt
#'
#' Approximate quantiles can be obtained from e.g. `qlnorm(0.5, meanlog_Rt_star, sdlog_Rt_star)`
#'
#' Alternatively if `tidy` is true the output will be post processed to conform to:
#'
#' `r i_reproduction_number`
#'
#'
#'
#' @export
#' @concept models
#'
#' @examples
#'
#' data = example_poisson_rt_smooth()
#' ip = example_ip()
#'
#' # first we need a set of incidence estimates
#' # we fit a poisson model to counts using a GAM:
#' model = mgcv::gam(count ~ s(time), family = "poisson", data=data)
#' ip_len = max(ip$tau)
#' newdata = dplyr::tibble(time = -ip_len:100)
#'
#' pred = stats::predict(model, newdata, se.fit = TRUE)
#'
#' # we can get prediction vcov from GAMs fairly easily
#' Xp = stats::predict(model, newdata, type = "lpmatrix")
#' pred_vcov = Xp %*% stats::vcov(model) %*% t(Xp)
#'
#' # Now we estimate rt
#' rt_est = rt_incidence_timeseries_implementation(
#'   time = newdata$time,
#'   mu = pred$fit,
#'   vcov = pred_vcov,
#'   ip = ip)
#'
#' # Lets compare epiestim on the same data
#' withr::with_seed(100, {
#'   epi = EpiEstim::estimate_R(
#'     data$count,
#'     method = "si_from_sample",
#'     si_sample = omega_matrix(ip),
#'     config = EpiEstim::make_config(
#'       method = "si_from_sample",
#'       t_start = 10:100-7,
#'       t_end = 10:100,
#'       n2 = 100)
#'   )
#' })
#'
#' ggplot2::ggplot()+
#'   ggplot2::geom_line(
#'     data=data %>% dplyr::filter(time<100),
#'     ggplot2::aes(x=time,y=rt), colour="black")+
#'   ggplot2::geom_line(
#'     data = rt_est, ggplot2::aes(x=time, y=mean_Rt_star, colour="gam+rt"))+
#'   ggplot2::geom_line(
#'     data = epi$R, ggplot2::aes(x=t_end, y=`Mean(R)`, colour="epiestim"))
#'
#' # mean bias of GAM+rt estimate:
#' mean(data$rt[seq_along(rt_est$mean_Rt_star)] - rt_est$mean_Rt_star)
#'
rt_incidence_timeseries_implementation = function(
  time,
  mu,
  vcov = NULL,
  sigma = NULL,
  ip = i_discrete_ip,
  tidy = FALSE,
  ...
) {
  # browser()
  if (length(time) != length(mu)) {
    stop("time and mu must be the same length")
  }
  orig_time = time
  time = as.integer(time)

  window = length(unique(ip$tau))
  max_tau = max(ip$tau)

  # the user has supplied a sigma vector rather than a vcov matrix. This
  # forces us to assume independence between estimates. This is sometimes OK
  # depending on the mass of the off diagonal, and is very quick.
  assume_indep = is.null(vcov)

  if (assume_indep) {
    # vcov is a vector of variances, we have no diagonal terms.
    long_vcov = dplyr::tibble(
      i = time,
      j = time,
      Sigma_ij = sigma^2
    )
    vcov = diag(sigma^2)
  } else {
    # vcov is a variance covariance matrix. make it into a long format dataframe
    tmp = matrix(time, length(time), length(time))
    long_vcov = dplyr::tibble(
      i = as.vector(tmp),
      j = as.vector(t(tmp)),
      Sigma_ij = as.numeric(vcov)
    ) %>%
      dplyr::filter(
        # all covariances are assumed positive.
        Sigma_ij > 0 &
          # we are only interested in the covariance matrix around the diagonal
          abs(i - j) <= window
      )
    sigma = sqrt(diag(vcov))
  }

  # A note on naming in this function:
  # everything is aligned to the paper. variables with big Sigma are capitalised
  # and these are variances or covariances (on the log scale).
  # small sigma are log normal sdlog parameters. sigma2 will be a squared version
  # of that. omega are infectivity profile probabilities.

  # nrow(tmp) = nrow(ip) * length(time)

  tmp = ip %>%
    dplyr::select(boot, tau, omega_tau = probability) %>%
    dplyr::ungroup() %>%
    dplyr::cross_join(dplyr::tibble(
      time_minus_tau = time,
      mu = as.numeric(mu),
      sigma = as.numeric(sigma)
    )) %>% # grouped by ip boot
    dplyr::mutate(
      # by using this as the summarising time we perform a convolution
      # of tau and time-tau items. time column is going to be the time(s) at
      # which we are estimating R_t
      time = time_minus_tau + as.integer(tau),
      log_m_tau = mu + log(omega_tau) + 0.5 * sigma^2
    )

  # table(tmp$time)
  # setdiff(as.integer(orig_time), tmp$time)
  # nrow(tmp_mu_t) = dplyr::n_distinct(ip$boot) * length(time)

  # The cross join copies a mu and sigma for each bootstrap and tau
  # value and we need something with bootstrap only.
  tmp_mu_t = tmp %>%
    # dplyr::group_by(boot, time) %>%
    dplyr::filter(tau == 0) %>%
    dplyr::select(boot, time, mu_t = mu, sigma_t = sigma)

  # setdiff(as.integer(orig_time), tmp_mu_t$time)

  # nrow(tmp_S) = dplyr::n_distinct(ip$boot) * (length(time)-(window-1))

  tmp_S = tmp %>%
    dplyr::group_by(boot, time) %>%
    dplyr::summarise(
      log_S_plus = .logsumexp(log_m_tau),
      c_max = 1 - sum(exp(2 * (log_m_tau - log_S_plus))),
      log_V_plus = .logsumexp(log_m_tau + 2 * log(sigma)) - log_S_plus,
      n = dplyr::n() #,
      # .by = c(boot, time)
    ) %>%
    # make sure that we are dealing with full windows of data.
    # this prevents estimates for negative serial intervals
    # based on not enough input at end of time-series and beginning
    # of data where the window is incomplete. This
    dplyr::filter(n == window) %>%
    dplyr::select(-n)

  # setdiff(as.integer(orig_time), tmp_S$time)

  # nrow(tmp) = nrow(tmp_S) * window

  tmp = tmp %>%
    dplyr::inner_join(
      tmp_S %>% dplyr::select(boot, time, log_S_plus),
      by = dplyr::join_by(boot, time)
    ) %>%
    dplyr::arrange(
      time_minus_tau,
      time,
      boot
    )

  # browser()
  # typeof(tmp$time)
  # typeof(long_vcov$i)
  # typeof(long_vcov$j)

  # nrow(tmp2) = nrow(tmp_S) * window

  tmp2 = long_vcov %>%
    dplyr::inner_join(
      tmp %>%
        dplyr::select(
          boot,
          time,
          i = time_minus_tau,
          # i2 = i - window,
          log_m_i = log_m_tau,
          log_S_plus
        ),
      by = c("i")
    ) %>%
    dplyr::inner_join(
      tmp %>%
        dplyr::transmute(
          boot,
          time,
          j = time_minus_tau,
          # j2 = j - window,
          log_m_j = log_m_tau
        ),
      by = dplyr::join_by(j, time, boot),
    )

  # xtmp = tmp %>%
  #   dplyr::select(
  #     boot,
  #     time,
  #     i = time_minus_tau,
  #     # i2 = i - window,
  #     log_m_i = log_m_tau,
  #     log_S_plus
  #   ) %>%
  #   dplyr::inner_join(
  #     tmp %>%
  #       dplyr::transmute(
  #         boot,
  #         time,
  #         j = time_minus_tau,
  #         # j2 = j - window,
  #         log_m_j = log_m_tau
  #       ),
  #     by = dplyr::join_by(boot, time),
  #     relationship = "many-to-many",
  #   )

  # tmp2 = xtmp %>%
  #   dplyr::ungroup() %>%
  #   dplyr::inner_join(
  #     long_vcov,
  #     by = c("i", "j")
  #   )

  tmp_sigma_Z2 = tmp2 %>%
    dplyr::summarise(
      log_sigma_Z2 = .logsumexp(log_m_i + log_m_j + log(Sigma_ij)),
      .by = c(boot, time, log_S_plus)
    ) %>%
    dplyr::mutate(
      log_sigma_Z2 = log_sigma_Z2 - 2 * log_S_plus
    ) %>%
    dplyr::select(-log_S_plus)

  tmp_sigma_0Z = tmp2 %>%
    # This filter selects the subset of covariances related to the estimate
    # time, this is the equivalent of taking a single row (or column) from the covariance
    # matrix.
    dplyr::filter(time == i) %>%
    dplyr::rename(tau = j, log_m_tau = log_m_j, Sigma_0tau = Sigma_ij) %>%
    # dplyr::group_by(boot, time, log_S_plus) %>% already grouped
    dplyr::summarise(
      log_Sigma_0Z = .logsumexp(log_m_tau + log(Sigma_0tau)),
      .by = c(boot, time, log_S_plus)
    ) %>%
    dplyr::mutate(
      log_Sigma_0Z = log_Sigma_0Z - log_S_plus
    ) %>%
    dplyr::select(-log_S_plus)

  tmp_Rt =
    tmp_mu_t %>%
    dplyr::inner_join(tmp_S, by = c("boot", "time")) %>%
    dplyr::inner_join(tmp_sigma_Z2, by = c("boot", "time")) %>%
    dplyr::inner_join(tmp_sigma_0Z, by = c("boot", "time")) %>%
    dplyr::mutate(
      mu_Rt = mu_t - log_S_plus + exp(log_sigma_Z2) / 2,
      sigma_Rt2 = sigma_t^2 + exp(log_sigma_Z2) - 2 * exp(log_Sigma_0Z),
      mean_Rt = exp(mu_Rt + sigma_Rt2 / 2),
      var_Rt = (exp(sigma_Rt2) - 1) * exp(2 * mu_Rt + sigma_Rt2),
    )

  if (assume_indep) {
    tmp_Rt = tmp_Rt %>%
      dplyr::mutate(
        log_risk_bias = log_V_plus - log_sigma_Z2,
        log_scale_bias = c_max * exp(log_V_plus) / 2,
        max_Rt_bias = exp(mu_Rt) * (exp(log_scale_bias) - 1),
        bias_flag = max_Rt_bias > 0.05 & log_risk_bias > log(5)
      )

    if (mean(tmp_Rt$bias_flag[-(1:max_tau)]) > 0.01) {
      .message_once(
        "Estimates were assumed to be independent, but more that 1% of estimates\n",
        "are at risk of Rt underestimation by more that 0.05 (absolute).\n",
        "We advise re-running supplying a full variance-covariance matrix, or\n",
        "a value to the `raw` parameter, or setting `quick=FALSE`."
      )
    }

    # ggplot(tmp_Rt)+geom_point(aes(x=time, y=max_Rt_bias))
    # ggplot(tmp_Rt)+geom_point(aes(x=time, y=exp(log_risk_bias)))
    # quantile(tmp_Rt$max_Rt_bias, c(0.5,0.99))
  }

  # combine the Rt estimates for each infectivity profile bootstrap. This is
  # either done by moments (mean_Rt_star, var_Rt_star, etc) or as a mixture
  # distribution with sets of mu and sigma parameters (mu_Rt_mix, sigma_Rt_mix).

  tmp_Rt_mix = tmp_Rt %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(
      mean_Rt_star = mean(mean_Rt),
      var_Rt_star = mean(var_Rt + mean_Rt^2) - mean_Rt_star^2,
      mu_Rt_mix = list(mu_Rt),
      sigma_Rt_mix = list(sqrt(sigma_Rt2))
    ) %>%
    dplyr::mutate(
      meanlog_Rt_star = log(mean_Rt_star) -
        log(var_Rt_star / mean_Rt_star^2 + 1) / 2,
      sdlog_Rt_star = sqrt(log(1 + var_Rt_star / mean_Rt_star^2))
    )

  if (anyNA(tmp_Rt_mix)) {
    # Does happen in situations where there are negative serial intervals
    # browser()
  }

  # ggplot() + geom_line(data=data, aes(x=time,y=rt),colour="red") + geom_line(data = tmp_Rt_mix,aes(x=time,y=mean_star))

  if (tidy) {
    out = .process_rt_timeseries(tmp_Rt_mix, orig_time, ...)
    if (anyNA(out)) {
      # browser()
    }
    return(out)
  }
  return(tmp_Rt_mix)
}


.process_rt_timeseries = function(df, input_time, approx = TRUE) {
  out = df %>%
    # out = tmp_Rt_mix %>%
    dplyr::rename(rt.fit = meanlog_Rt_star, rt.se.fit = sdlog_Rt_star) %>%
    dplyr::mutate(time = as.time_period(time, input_time))

  if (isTRUE(approx)) {
    # We approximate the mixture with one lognormal with same mean and SD as
    # the mixture.
    out = out %>%
      .result_from_fit(
        type = "rt",
        qfn = function(p) stats::qlnorm(p, .$rt.fit, .$rt.se.fit)
      ) %>%
      .keep_cdf(
        type = "rt",
        meanlog = .$rt.fit,
        sdlog = .$rt.se.fit,
        link = "log"
      )
  } else {
    if (isFALSE(approx)) {
      approx = "exact"
    }
    out = out %>%
      .result_from_fit(
        type = "rt",
        # This is one estimate of Rt, as a mixture distribution.
        qfn = function(p) {
          purrr::map2_dbl(
            .$mu_Rt_mix,
            .$sigma_Rt_mix,
            ~ .qmixlnorm(p, meanlogs = .x, sdlogs = .y, method = approx)
          )
        }
      ) %>%
      .keep_cdf(
        type = "rt",
        meanlog = .$mu_Rt_mix,
        sdlog = .$sigma_Rt_mix,
        link = "log"
      )
  }
  out = out %>%
    dplyr::select(-c(mean_Rt_star, var_Rt_star, mu_Rt_mix, sigma_Rt_mix))

  return(out)
}

## Reference implementation (matrix) ----

#' Reference implementation of the Rt from modelled incidence algorithm
#'
#' This function estimates the reproduction number for a specific point in time
#' given a time series of log-normally distributed incidence estimates, a set of
#' infectivity profiles. This version of the algorithm only works for a single
#' time point and is not optimised for running over a whole time series. For that
#' please see [rt_incidence_timeseries_implementation()].
#'
#' N.B. the description of this algorithm is given here:
#' https://ai4ci.github.io/ggoutbreak/articles/rt-from-incidence.html
#'
#' @param mu_t a vector of meanlog parameters of a lognormal distribution of
#'   modelled incidence. This is a vector of length `k`.
#' @param vcov_ij a log scale variance-covariance matrix of predictions. It is
#'   optional but if not given and `sigma_t` is given then this will be inferred
#'   assuming independence between estimates. This should be a matrix with
#'   dimensions `k * k`
#' @param omega a matrix (or vector) representing the infectivity profile as a
#'   discrete time probability distribution. This must be a `k * n` matrix or a
#'   vector of length `k`. Each of the `n` columns is a individual estimate of
#'   the infectivity profile (N.B. this is the same format as `EpiEstim`). In
#'   `EpiEstim` the first row of this matrix must be zero and represents a
#'   delay of zero. This constraint does not apply here.
#' @param sigma_t if `vcov_ij` is not given then this must be supplied as the
#'   sdlog of the log normal incidence estimate
#' @param tau_offset In most cases the infectivity profile has support for
#'   delays from `0:(k-1)` and the Rt estimate is made at time `(k-1)`. If there
#'   is a negative component of a serial interval being used as a proxy then the
#'   support will be `-tau_offset:(k-tau_offset-1)`.
#'
#' @returns a list with the following items:
#' - time_Rt: the time point of the Rt esitmate (usually `k-1`)
#' - mean_Rt_star: the mean of the Rt estimate
#' - var_Rt_star: the variance of the Rt estimate
#' - meanlog_Rt_star: log normal parameter for approximate distribution of Rt
#' - sdlog_Rt_star: log normal parameter for approximate distribution of Rt
#' - mu_Rt_mix: a vector of log normal parameters for more exact mixture
#'   distribution of Rt
#' - sigma_Rt_mix: a vector of log normal parameters for more exact mixture
#'   distribution of Rt
#' - quantile_Rt_fn: A log-normal mixture quantile function for Rt
#'
#' Quantiles can either be obtained from the quantile function or from e.g.
#' `qlnorm(p, meanlog_Rt_star, sdlog_Rt_star)`
#'
#' @export
#' @concept models
#'
#' @examples
#'
#' data = example_poisson_rt_smooth()
#'
#' omega = omega_matrix(example_ip())
#' k = nrow(omega)
#'
#' pred_time = 50
#' index = pred_time-k:1+1
#'
#' # we will try and estimate the Rt at time k+10:
#' print(data$rt[pred_time])
#'
#' # first we need a set of incidence estimates
#' # in the simplest example we use a GLM:
#'
#' newdata = dplyr::tibble(time = 1:100)
#'
#'
#'
#' model = stats::glm(count ~ splines::bs(time,df=8), family = "poisson", data=data)
#' pred = stats::predict(model, newdata, se.fit = TRUE)
#'
#' # I've picked a df to make this example work. In real life you would need
#' # to validate the incidence model is sensible and not over fitting
#' # before using it to estimate RT:
#' # ggplot2::ggplot()+
#' #   ggplot2::geom_point(data=data, ggplot2::aes(x=time, y=count))+
#' #   ggplot2::geom_line(
#' #     data = newdata %>% dplyr::mutate(fit = exp(pred$fit)
#' #     ), ggplot2::aes(x=time,y=fit))
#'
#' mu_t = pred$fit[index]
#' sigma_t = pred$se.fit[index]
#'
#' # prediction vcov is not simple from GLM models.
#' rt_est = rt_incidence_reference_implementation(mu_t=mu_t,sigma_t = sigma_t, omega = omega)
#'
#' # quantiles from a mixture distribution:
#' rt_est$quantile_Rt_fn(c(0.025,0.5,0.975))
#' # and from the rough estimate based on matching of moments:
#' stats::qlnorm(c(0.025,0.5,0.975), rt_est$meanlog_Rt_star, rt_est$sdlog_Rt_star)
#'
#' # GLM do not produce vcov matrices we can estimate them if we have access to
#' # the data:
#' vcov_glm = vcov_from_residuals(data$count[1:100], pred$fit, pred$se.fit)$vcov_matrix
#' vcov_glm_ij = vcov_glm[index, index]
#'
#' # Using an estimated vcov we get very similar answers:
#' rt_est_vcov = rt_incidence_reference_implementation(mu_t=mu_t,vcov_ij = vcov_glm_ij, omega = omega)
#' rt_est_vcov$quantile_Rt_fn(c(0.025,0.5,0.975))
#' # In theory assuming independence leads to excess uncertainty and possible
#' # underestimation bias. In most situations though this appears low risk.
#'
#' # Lets do the same with a GAM:
#' model2 = mgcv::gam(count ~ s(time), family = "poisson", data=data)
#' pred2 = stats::predict(model2, newdata, se.fit = TRUE)
#'
#' # we can get prediction level vcov from GAMs easily
#' Xp = stats::predict(model2, newdata, type = "lpmatrix")
#' pred_vcov = Xp %*% stats::vcov(model2) %*% t(Xp)
#' vcov_ij = pred_vcov[index, index]
#' mu_t2 = pred2$fit[index]
#'
#' rt_est2 = rt_incidence_reference_implementation(mu_t=mu_t2, vcov_ij=vcov_ij, omega = omega)
#' rt_est2$quantile_Rt_fn(c(0.025,0.5,0.975))
#'
#' # How does this compare to EpiEstim:
#' # N.B. setting seed to make deterministic
#' withr::with_seed(100, {
#'   epi = EpiEstim::estimate_R(
#'     data$count,
#'     method = "si_from_sample",
#'     si_sample = omega,
#'     config = EpiEstim::make_config(
#'       method = "si_from_sample",
#'       t_start = pred_time-7,
#'       t_end = pred_time,
#'       n2 = 100)
#'   )
#' })
#'
#' epi$R %>%
#'   dplyr::select(`Quantile.0.025(R)`, `Median(R)`, `Quantile.0.975(R)`)
#'
#'
rt_incidence_reference_implementation = function(
  mu_t,
  vcov_ij = diag(sigma_t^2),
  omega,
  sigma_t = NULL,
  tau_offset = 0
) {
  ## Parameters
  # mu_t: a vector of meanlog parameters of a lognormal distribution of
  #   modelled incidence. This is a vector of length `k`.
  # vcov_ij: a log scale variance-covariance matrix of predictions. It is
  #   optional but if not given and `sigma_t` is given then this will be inferred
  #   assuming independence between estimates. This should be a matrix with
  #   dimensions `k * k`
  # omega: a matrix (or vector) representing the infectivity profile as a
  #   discrete time probability distribution. This must be a `k * n` matrix or a
  #   vector of length `k`. Each of the `n` columns is a individual estimate of
  #   the infectivity profile (N.B. this is the same format as `EpiEstim`). In
  #   `EpiEstim` the first row of this matrix must be zero and represents a
  #   delay of zero. This constraint does not apply here.
  # sigma_t: if `vcov_ij` is not given then this must be supplied as the
  #   sdlog of the log normal incidence estimate
  # tau_offset: In most cases the infectivity profile has support for
  #   delays from `0:(k-1)` and the Rt estimate is made at time `(k-1)`. If there
  #   is a negative component of a serial interval being used as a proxy then the
  #   support will be `-tau_offset:(k-tau_offset-1)`.

  omega_m = as.matrix(omega)
  k = nrow(omega_m)
  if (k != length(mu_t)) {
    stop("omega must have the same number of rows as length of mu_t.")
  }
  if (!all(dim(vcov_ij) == k)) {
    stop(
      "vcov_ij must be a square matrix with the same dimensions as mu_t, and nrow(omega)"
    )
  }

  # switch direction of omega to match time-series. This eliminates need for
  # t-\tau indexes
  omega_m = apply(omega_m, MARGIN = 2, rev)
  sigma_t = sqrt(diag(vcov_ij))

  # The tau_offset defines where the zero point of the infectivity profile is.
  # the variable origin is where this is in the time-series. Usually this will
  # be the last entry.
  # This enables us to handle negative time points in infectivity profile, if
  # for example we are using serial interval as a proxy. In this case we can't
  # estimate Rt right at the end, and the estimate is offset by the number of
  # negative entries in the serial interval distribution.
  time_Rt = length(mu_t) - tau_offset
  mu = mu_t[time_Rt]
  sigma = sigma_t[time_Rt]

  tmp = apply(omega_m, MARGIN = 2, function(omega) {
    # we approximate the denominator of the renewal equation by
    # implementing the lognormal approximation for sum of (weighted) lognormals:
    # This is referenced on the wikipedia page for lognormal but is also
    # from Lo 2013 (10.2139/ssrn.2220803).

    log_m_tau = mu_t + sigma_t^2 / 2 + log(omega)
    log_S_plus = .logsumexp(log_m_tau)

    # enforce constraint that covariance must be positive (n.b. this is actually
    # only needed for logsumexp and could be relaxed with a signed logsumexp)
    log_Sigma_ij = log(pmax(vcov_ij, 0))
    log_Sigma_0tau = log_Sigma_ij[time_Rt, ]

    # parameters of denominator of renewal equation as a lognormal
    log_sigma_Z2 = .logsumexp(outer(log_m_tau, log_m_tau, "+") + log_Sigma_ij) -
      2 * log_S_plus
    mu_Z = log_S_plus - 1 / 2 * exp(log_sigma_Z2)

    # Calculate the ratio of lognormals including potential correlation
    log_Sigma_0Z = .logsumexp(log_m_tau + log_Sigma_0tau) - log_S_plus
    mu_Rt = mu - mu_Z
    sigma_Rt2 = sigma^2 + exp(log_sigma_Z2) - 2 * exp(log_Sigma_0Z)

    return(c(mu_Rt, sigma_Rt2))
  })

  # Combine results from multiple infectivity profiles using a
  # mixture distribution:
  mu_Rt = tmp[1, ]
  sigma_Rt2 = tmp[2, ]

  # log normal moments:
  means = exp(mu_Rt + sigma_Rt2 / 2)
  vars = (exp(sigma_Rt2) - 1) * exp(2 * mu_Rt + sigma_Rt2)

  # parameters of an approximating log-normal matched by moments:
  mean_star = mean(means)
  var_star = mean(vars + means^2) - mean_star^2
  mu_star = log(mean_star) - log(var_star / mean_star^2 + 1) / 2
  sigma_star = sqrt(log(1 + var_star / mean_star^2))

  return(list(
    time_Rt = time_Rt,
    mean_Rt_star = mean_star,
    var_Rt_star = var_star,
    meanlog_Rt_star = mu_star,
    sdlog_Rt_star = sigma_star,
    mu_Rt_mix = mu_Rt,
    sigma_Rt_mix = sqrt(sigma_Rt2),
    # A log-normal mixture quantile function:
    quantile_Rt_fn = function(p) {
      .qmixlnorm(p, mu_Rt, sqrt(sigma_Rt2))
    }
  ))
}
