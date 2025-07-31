#' Reproduction number from modelled incidence
#'
#' Calculate a reproduction number estimate from modelled incidence using the
#' methods described in the vignette "Estimating the reproduction number from
#' modelled incidence" and using a set of empirical generation time
#' distributions. This assumes that modelled incidence has the same time unit as
#' the `ip` distribution, and that this is daily, if this is not the case then
#' [rescale_model()] may be able to fix it.
#'
#' @iparam df modelled incidence estimate
#' @iparam ip an infectivity profile (aka generation time distribution)
#' @param approx use a faster, but approximate, estimate of quantiles
#' @param .progress show a CLI progress bar
#'
#' @return `r i_reproduction_number`
#' @export
#' @concept models
#' @examples
#' tmp = ggoutbreak::test_poisson_rt_smooth %>%
#'   poisson_locfit_model() %>%
#'   rt_from_incidence(approx=FALSE)
#'
#' tmp2 = ggoutbreak::test_poisson_rt_smooth %>%
#'   poisson_locfit_model() %>%
#'   rt_from_incidence(approx=TRUE)
#'
#' plot_data = dplyr::bind_rows(
#'   tmp %>% dplyr::mutate(class = "exact"),
#'   tmp2 %>% dplyr::mutate(class = "approx"),
#' ) %>% dplyr::group_by(class)
#'
#' if (interactive()) {
#'   plot_rt(plot_data, date_labels="%b %y")+
#'    sim_geom_function(ggoutbreak::test_poisson_rt_smooth)+
#'    ggplot2::coord_cartesian(ylim=c(0.5,3))+
#'    ggplot2::facet_wrap(~class)
#' }
#'
rt_from_incidence = function(
  df = i_incidence_model,
  ip = i_discrete_ip,
  approx = TRUE,
  .progress = interactive()
) {
  #, assume_start = TRUE) {

  ip = interfacer::ivalidate(ip)

  env = rlang::current_env()
  if (.progress) {
    cli::cli_progress_bar(
      "Rt (modelled incidence)",
      total = dplyr::n_groups(df),
      .envir = env
    )
  }

  modelled = interfacer::igroup_process(df, function(df, .groupdata, ...) {
    .stop_if_not_daily(df$time)

    tmp_ip = .select_ip(ip, .groupdata)
    mu = df$incidence.fit
    sigma = df$incidence.se.fit
    min_tau = min(tmp_ip$tau)
    omega = tmp_ip %>% .omega_matrix(epiestim_compat = FALSE)
    rt = .estimate_rt_timeseries(
      mu,
      sigma,
      omega,
      min_tau,
      cor_ij = NULL,
      approx = approx
    )

    # # omega is a matrix
    # omega = tmp_ip %>% .omega_matrix(epiestim_compat = FALSE)
    # window = nrow(omega)
    # # relax assumption that time in infectivity profile starts at 1.
    # # negative serial intervals should be OK
    # start = min(tmp_ip$tau)
    # end = nrow(df) + min(c(start, 0))
    #
    # rt = lapply(1:end, function(i) {
    #   if (i < window + start) {
    #     pad = .ln_pad(
    #       window - i + start,
    #       df$incidence.fit[1],
    #       df$incidence.se.fit[1],
    #       spread = 1.1
    #     )
    #     mu_t = c(pad$mu, df$incidence.fit[1:(i - start)])
    #     sigma_t = c(pad$sigma, df$incidence.se.fit[1:(i - start)])
    #
    #     # omega = omega[1:(i-1)]/sum(omega[1:(i-1)])
    #     # mu_t = df$incidence.fit[1:(i-1)]
    #     # sigma_t = df$incidence.se.fit[1:(i-1)]
    #   } else {
    #     mu_t = df$incidence.fit[(i - window - start + 1):(i - start)]
    #     sigma_t = df$incidence.se.fit[(i - window - start + 1):(i - start)]
    #   }
    #   return(.internal_r_t_estim(
    #     mu = df$incidence.fit[i],
    #     sigma = df$incidence.se.fit[i],
    #     omega = omega,
    #     mu_t = mu_t,
    #     sigma_t = sigma_t,
    #     cor_ij = NULL,
    #     approx = approx
    #   ))
    # })
    #
    # if (length(rt) < nrow(df)) {
    #   rt = c(rt, rep(list(NULL), nrow(df) - length(rt)))
    # }

    new_data = df %>%
      dplyr::mutate(rt = rt) %>%
      tidyr::unnest(rt, keep_empty = TRUE)

    if (.progress) {
      cli::cli_progress_update(.envir = env)
    }

    return(new_data)
  })

  if (.progress) {
    cli::cli_progress_done()
  }

  return(modelled)
}

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

.internal_r_t_estim = function(
  mu,
  sigma,
  omega,
  mu_t,
  sigma_t,
  cor_ij = NULL,
  approx = TRUE
) {
  # This function estimates the reproduction number for a specific point in time
  # given a current log-normally distributed incidence estimate, a set of
  # infectivity profiles, and a set of historical incidence estimates. N.B. the
  # description of this algorithm is given here:
  # https://ai4ci.github.io/ggoutbreak/articles/rt-from-incidence.html
  # - mu is a central estimate of the log of an incidence rate
  # - sigma is a standard error of the log an incidence rate
  # - omega is a matrix of infectivity profiles with each column representing one
  # infectivity profile probability distribution.
  # - mu_t is a central estimate of the log of an incidence rate in the days leading up
  # to the given time point
  # - sigma_t is a standard error of the log an incidence rate in
  # the days leading up to the given time point.
  # - cor_ij the predicted correlation matrix. If missing time points are assumed independent.
  # can be generated from cov2cor(vcov), or we can generate candidates based on
  # time differences and bandwidth.
  # - approx indicates that we should approximate the quantiles using the
  # arithmetic mean of the mixture distribution quantiles (quick), rather than solving
  # the mixture cumulative distribution function (slow)

  omega_m = as.matrix(omega)

  # switch direction of omega to match timeseries. This eliminates need for
  # t-\tau indexes
  omega_m = apply(omega_m, MARGIN = 2, rev)

  # for each infectivity profile we
  # approximate the denominator of the renewal equation by
  # implementing the lognormal approximation for sum of (weighted) lognormals:
  # This is referenced on the wikipedia page for lognormal but is also
  # from Lo 2013 (10.2139/ssrn.2220803).
  tmp = apply(omega_m, MARGIN = 2, function(omega) {
    # We keep everything in log space for numerical stability.
    log_S_t = .logsumexp(mu_t + sigma_t^2 / 2 + log(omega))
    log_T_t_tau = mu_t + sigma_t^2 / 2 + log(omega) + log(sigma_t)

    # Are individual samples correlated or not (default is not)
    if (!is.null(cor_ij)) {
      # assuming the terms are correlated does not make much of a difference to
      # outcome but requires a matrix the size of length(omega)^2
      # log_T_t_tau_minus_log_sigma = mu_t + sigma_t^2 / 2 + log(omega)
      # log_T_t_matrix = outer(log_T_t_tau_minus_log_sigma, log_T_t_tau_minus_log_sigma,FUN = "+")
      # n = length(omega)
      # lambda = 1 / cor
      # idx = 0:(n^2 - 1)
      # i = idx %/% n
      # j = idx %% n
      #
      # log_var_Zt_ij = log_T_t_tau[i + 1] +
      #   log_T_t_tau[j + 1] -
      #   lambda * abs(i - j)

      log_T_t_matrix = outer(
        log_T_t_tau,
        log_T_t_tau,
        FUN = "+"
      )
      # cor_t is the same as cor_{ij}*\sigma_i*\sigma_j
      # negative values are ignored
      log_var_Zt_ij = log_T_t_matrix + log(pmax(cor_ij, 0))
    } else {
      # cor_ij is zero for i <> j

      log_var_Zt_ij = 2 * log_T_t_tau
    }

    log_var_Zt = .logsumexp(log_var_Zt_ij) - 2 * log_S_t

    var_Zt = exp(log_var_Zt)
    mu_Zt = log_S_t - var_Zt / 2

    mu_Rt = mu - mu_Zt
    var_Rt = sigma^2 + var_Zt

    return(c(mu_Rt, var_Rt))
  })

  # Combine results from multiple infectivity profiles using a
  # mixture distribution
  mu_Rt = tmp[1, ]
  var_Rt = tmp[2, ]
  sigma_Rt = sqrt(var_Rt)
  means = exp(mu_Rt + var_Rt / 2)
  vars = (exp(var_Rt) - 1) * exp(2 * mu_Rt + var_Rt)
  mean_star = mean(means)
  var_star = mean(vars + means^2) - mean_star^2

  out = tibble::tibble(
    rt.fit = log(mean_star) - log(var_star / mean_star^2 + 1) / 2,
    #rt.fit = log(mean_star^2/sqrt(mean_star^2+var_star)),
    rt.se.fit = sqrt(log(1 + var_star / mean_star^2))
  )

  if (approx) {
    # We approximate the mixture with one lognormal with same mean and SD as
    # the mixture.
    out = out %>%
      .result_from_fit(
        type = "rt",
        qfn = \(p) stats::qlnorm(p, .$rt.fit, .$rt.se.fit)
      ) %>%
      .keep_cdf(
        type = "rt",
        meanlog = .$rt.fit,
        sdlog = .$rt.se.fit,
        link = "log"
      )
  } else {
    out = out %>%
      .result_from_fit(
        type = "rt",
        # This is one estimate of Rt, as a mixture distribution.
        qfn = \(p) {
          .qmixlnorm(p, meanlogs = mu_Rt, sdlogs = sigma_Rt, method = "exact")
        }
      ) %>%
      .keep_cdf(
        type = "rt",
        meanlog = list(mu_Rt),
        sdlog = list(sigma_Rt),
        link = "log"
      )
  }

  return(out)
}

# create a set of lognormals based on mu and sigma with increasing SD
.ln_pad = function(length, mu, sigma, spread = 1.1) {
  if (length(mu) > 1) {
    l = length(mu)
    mus = stats::lm(y ~ x, dplyr::tibble(y = rev(mu[1:l]), x = -(l - 1):0)) %>%
      stats::predict(dplyr::tibble(x = 1:length))
    # sigmas = rep(sigma[1], length)
    sigmas = sigma[1] * 1:length * spread
  } else {
    mean = exp(mu[1] + sigma[1]^2 / 2)
    sd = sqrt((exp(sigma[1]^2) - 1) * exp(2 * mu[1] + sigma[1]^2))
    means = rep(mean, length.out = length)
    sds = sd * spread^(1:length)
    mus = log(means) - 1 / 2 * log(sds^2 / means^2 + 1)
    sigmas = sqrt(log(sds^2 / means^2 + 1))
  }
  return(list(mu = rev(mus), sigma = rev(sigmas)))
}


# mu the incidence on the log scale
# sigma the incidence sd on the log scale
# omeag a matrix style infectivity profile,
# min_tau the smallest value of the tau column in the infectivity profile
# cor_ij the correlation matrix (e.g. output from .gam_glm_cor or .time_based_correlation)
.estimate_rt_timeseries = function(
  mu,
  sigma,
  omega,
  min_tau,
  cor_ij = NULL,
  approx = TRUE
) {
  window = nrow(omega)
  padding = .ln_pad(window, mu[1:5], sigma[1], 1.1)

  # relax assumption that time in infectivity profile starts at 1.
  # negative serial intervals should be OK
  end_offset = -min_tau # offset from index of start of data window, if tau is negative this will be positive
  start_offset = end_offset - window + 1 # smaller than end offset
  end = length(mu) - max(c(end_offset, 0)) # the end is the last index value for which we can derive a window
  cor_ij_pad = matrix(0, window, window)
  diag(cor_ij_pad) = 1

  rt = lapply(1:end, function(i) {
    #i=78
    w = start_offset:end_offset + i + window
    mu_t = c(padding$mu, mu)[w]
    sigma_t = c(padding$sigma, sigma)[w]
    mu_i = mu[i]
    sigma_i = sigma[i]

    if (is.null(cor_ij)) {
      # disable the correlation matrix and assume independence
      tmp_cor_ij = NULL
    } else if (all(dim(cor_ij) == window)) {
      # The correlation matrix is the right size because it is a fixed size
      # one generated by .time_based_correlation()
      tmp_cor_ij = cor_ij
    } else if (all(dim(cor_ij) == length(mu))) {
      # The correlation matrix is the same size as the data because it was
      # extracted from the vcov of the prediction.
      # we need to select the correct part of the matrix for each time window
      # and pad out the bits that are before the data,
      w2 = max(c(1, i + start_offset)):(i + end_offset)
      w3 = window + 1 - length(w2):1
      tmp_cor_ij = cor_ij_pad
      tmp_cor_ij[w3, w3] = cor_ij[w2, w2]
    }

    .internal_r_t_estim(
      mu_i,
      sigma_i,
      omega,
      mu_t,
      sigma_t,
      tmp_cor_ij,
      approx = approx
    )
  })

  if (length(rt) < length(mu)) {
    # If there are negative serial intervals then there is
    rt = c(rt, rep(list(NULL), length(mu) - length(rt)))
  }

  return(rt)
}
