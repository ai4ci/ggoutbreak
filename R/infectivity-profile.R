#' Make an infectivity profile from published data
#'
#' The infectivity profile is typically fitted to data by MCMC and reported
#' as median and 95% credible intervals, of the mean, and the SD of (usually_ a
#' gamma distribution. This function generates a discrete infectivity probability
#' distribution representing the chance that an infectee was infected on any specific
#' day after the infector was infected (given that the infectee was infected).
#'
#' `EpiEstim` generates these distributions by sampling from a truncated normal
#' distribution for both mean and sd. The means and sds thus produced are
#' discretised using a gamma distribution offset by 1 day, to enforce that the
#' probability of infection on day zero is zero.
#'
#' This constraint changes the shape of the distribution somewhat and may cause
#' a small bias (although there is no ground truth to evaluate). A different
#' sampling and discretisation strategy is provided. The sampler uses log-normal
#' distributions for both mean and SD with a degree of correlation. The
#' discretizer assigns probabilities direct from the CDF of the gamma
#' distribution without offset. This results in non zero values for the
#' probability at time zero and can only be used with Rt estimation methods that
#' can handle zero/negative serial intervals (e.g. `rt_from_incidence` or
#' `rt_from_renewal`, or `rt_from_growth_rate`).
#'
#' @param median_of_mean,lower_ci_of_mean,upper_ci_of_mean Parameters of the
#'   infectivity profile mean.
#' @param median_of_sd,lower_ci_of_sd,upper_ci_of_sd Parameters of the
#'   infectivity profile SD.
#' @param correlation the correlation between mean and sd. this is optional
#'   and will be inferred if not changed form the default `NA` value.
#' @param n_boots The number of samples to generate.
#' @param epiestim_sampler Use `EpiEstim` to generate the random samples using
#'   independent truncated normal distributions for mean and SD based on
#'   parameters above. If `FALSE` then it will use a log normal distributions
#'   with correlation.
#' @param epiestim_compat Use `EpiEstim` to generate the infectivity profiles.
#'   A true value here results in an infectivity profile with probability of 0
#'   for day 0.
#' @param z_crit the width of the confidence intervals (defaults to 95%).
#'
#' @return a long format infectivity profile data frame, or a list of dataframes
#'   if input is a vector.
#' @export
#' @concept delay_distribution
#'
#' @examples
#' # COVID-19 estimates from Ganyani et al 2020.
#' tmp = make_gamma_ip(5.2, 3.78, 6.78, 1.72, 0.91, 3.93,
#'   epiestim_sampler=FALSE, epiestim_compat=FALSE)
#'
#' tmp %>%
#'   dplyr::group_by(boot) %>%
#'   dplyr::summarise(
#'     mean = sum(tau*probability),
#'     sd = sqrt(sum((tau-sum(tau*probability))^2*probability))
#'   ) %>%
#'   dplyr::summarise(
#'     mean = sprintf("%1.2f [%1.2f-%1.2f]",
#'       stats::quantile(mean,0.5),
#'       stats::quantile(mean,0.025),
#'       stats::quantile(mean,0.975)),
#'     sd = sprintf("%1.2f [%1.2f-%1.2f]",
#'       stats::quantile(sd,0.5),
#'       stats::quantile(sd,0.025),
#'       stats::quantile(sd,0.975))
#'   )
#'
#' if(interactive()) {
#'   plot_ip(tmp, alpha=0.1) +
#'     ggplot2::coord_cartesian(xlim=c(0,15))
#' }
#'
#' means = c(3,4,5)
#' ips = make_gamma_ip(means)
#'
#' if (interactive()) {
#'   purrr::map2(ips,means, ~ .x %>% dplyr::mutate(label = sprintf("Mean: %1.2f",.y))) %>%
#'     purrr::map( ~ plot_ip(.x,alpha=0.1)+ggplot2::facet_wrap(~label))
#' }
#'
make_gamma_ip = function(
    median_of_mean, lower_ci_of_mean = median_of_mean, upper_ci_of_mean = median_of_mean,
    median_of_sd = sqrt(median_of_mean), lower_ci_of_sd = median_of_sd, upper_ci_of_sd = median_of_sd, correlation = NA,
    n_boots=100, epiestim_compat = FALSE, epiestim_sampler = epiestim_compat, z_crit = 0.95
) {

  interfacer::recycle(
    median_of_mean, lower_ci_of_mean, upper_ci_of_mean,
    median_of_sd, lower_ci_of_sd, upper_ci_of_sd
  )

  if (length(median_of_mean) > 1) {
    return(lapply(
      seq_along(median_of_mean),
      function(i) make_gamma_ip(
        median_of_mean[i], lower_ci_of_mean[i], upper_ci_of_mean[i],
        median_of_sd[i], lower_ci_of_sd[i], upper_ci_of_sd[i],
        correlation, n_boots, epiestim_compat, epiestim_sampler,
        z_crit
      )
    ))
  }

  if(lower_ci_of_mean != median_of_mean && upper_ci_of_mean == median_of_mean &&
     median_of_sd == sqrt(median_of_mean) && lower_ci_of_sd == median_of_sd && upper_ci_of_sd == median_of_sd) {
    .warn_once("Two unnamed parameters supplied to `make_gamma_ip()`. Interpreting input as one mean and sd.")
    median_of_sd = lower_ci_of_mean
    n_boots = 1
  }


    if (n_boots < 1) {
      stop("`n_boots` must 1 or greater")
    } else if (n_boots == 1 || (
      lower_ci_of_mean == median_of_mean && upper_ci_of_mean == median_of_mean &&
      lower_ci_of_sd == median_of_sd && upper_ci_of_sd == median_of_sd
    )) {

      tmp = tibble::tibble(
        mean = median_of_mean,
        sd = median_of_sd
      ) %>% dplyr::mutate(
        shape = (mean^2)/(sd^2),
        rate = mean/(sd^2)
      )

    } else {

      quantiles = tibble::tibble(
        lmean = log(c(median_of_mean, lower_ci_of_mean, upper_ci_of_mean)),
        lsd = log(c(median_of_sd, lower_ci_of_sd, upper_ci_of_sd)),
        z = stats::qnorm(c(0.5, 0.5-z_crit/2, 0.5+z_crit/2)))

      # this linear model fits a (log)normal distribution to provided quantiles.
      # lm coefficients - intercept is mean and z gradient is sd.
      lmMean = stats::lm(formula = lmean~z, quantiles)$coeff
      lmSd = stats::lm(formula = lsd~z, quantiles)$coeff

      mean_of_mu = lmMean[1]
      sd_of_mu = lmMean[2]
      mean_of_sigma = lmSd[1]
      sd_of_sigma = lmSd[2]

      # Convert lognormal mean parameters to real scale
      mean_of_mean = exp(mean_of_mu + sd_of_mu^2/2)
      sd_of_mean = sqrt((exp(sd_of_mu^2)-1)*exp(2*mean_of_mu + sd_of_mu^2))

      # Convert lognormal sd parameters to real scale
      mean_of_sd = exp(mean_of_sigma + sd_of_sigma^2/2)
      sd_of_sd = sqrt((exp(sd_of_sigma^2)-1)*exp(2*mean_of_sigma + sd_of_sigma^2))

      if (epiestim_sampler) {
        tmp = .epiestim_sampler(
          n_boots,
          mean_si = mean_of_mean,
          std_mean_si = sd_of_mean,
          std_si = mean_of_sd,
          std_std_si = sd_of_sd
        )
      } else {
        tmp = .ggoutbreak_sampler(
          n_boots,
          mean_of_mu = mean_of_mu,
          mean_of_sigma = mean_of_sigma,
          sd_of_mu = sd_of_mu,
          sd_of_sigma = sd_of_sigma,
          correlation = correlation
        )
      }

    }

    if (epiestim_compat) {
      tmp2 = tmp %>% .epiestim_discretise()
    } else {
      tmp2 = tmp %>% .ggoutbreak_discretise()
    }


  return(tmp2 %>% dplyr::select(tau,a0,a1,probability,boot) %>% dplyr::group_by(boot))
}







#' Recover a long format infectivity profile from an `EpiEstim` style matrix
#'
#' @param omega a matrix of probabilities, starting at time zero, with columns
#'   representing one possible infectivity profile, with the fist value being
#'   the probability at time zero to 0.5. Or a vector of probabilities for one
#'   single profile, resulting in 1 bootstrap.
#' @param normalise is this a probability mass function? In which case we make the
#'   sum of this equal to one (the default). If `FALSE` then the input matrix,
#'   or vector is clipped so that its maximum value is one. If this is a number
#'   then the PMF is scaled to this value.
#'
#' @return a long format `ip` delay distribution
#' @export
#' @concept delay_distribution
#'
#' @examples
#' format_ip(make_empirical_ip(c(0,0,1,1,1,2,2,2,1,1)))
make_empirical_ip = function(omega, normalise=TRUE) {
  r = if (is.matrix(omega)) nrow(omega) else length(omega)
  c = if (is.matrix(omega)) ncol(omega) else 1
  tau = rep(0:(r-1), c)
  boot = as.vector(sapply(1:c, rep, times=r))
  probability = as.vector(omega)
  ip = tibble::tibble(
    tau = tau,
    probability = probability,
    a0 = rep(c(0,seq(0.5,length.out = r-1)),c),
    a1 = rep(seq(0.5,length.out = r),c),
    boot = boot
  )
  if (isFALSE(normalise)) {
    ip = ip %>%
      dplyr::group_by(boot) %>%
      dplyr::mutate(probability = ifelse(probability>1,1,probability))
  } else {
    normalise = as.numeric(normalise)
    ip = ip %>%
      dplyr::group_by(boot) %>%
      dplyr::mutate(probability = probability/sum(probability)*normalise)
  }
  return(ip)
}


#' Re-sample an empirical IP distribution direct from data
#'
#' Suits larger contact tracing data sets where there is a delay between
#' 2 events which may or may not be precisely known.
#'
#' @param tau the delay between first and second events
#' @param min_tau the minimum delay for interval censored delays
#' @param max_tau the maximum delay for interval censored delays
#' @param add_noise adds noise to each date point for each replicate
#' @param truncate what is the minimum realistic value of the parameter
#' @param n_boots number of replicates to generate
#' @param seed a random number seed for reproducibility
#'
#' @return a long format `ip` delay distribution
#' @export
#' @concept delay_distribution
#'
#' @examples
#' tau = rgamma2(100, 5,2)
#' ip = make_resampled_ip(min_tau = tau-1, max_tau = tau+1, seed = 100)
#' if(interactive()) {
#'   plot_ip(ip,alpha=0.1)
#' }
make_resampled_ip = function(tau, min_tau=pmax(tau-0.5,truncate), max_tau=tau+0.5, add_noise = TRUE, truncate = 0, n_boots=100, seed = Sys.time()) {
  withr::with_seed(seed,{
    dplyr::bind_rows(lapply(1:n_boots, \(i) {

      tmp = tibble::tibble(
        min = min_tau,
        max = max_tau
      ) %>%
      dplyr::slice_sample(prop=1, replace=TRUE) %>%
      dplyr::filter(max>min)

      if (add_noise) {
        tmp = tmp %>% dplyr::mutate(
          min = min-1,
          max = max+1
        )
      }

      tmp %>%
        dplyr::transmute(
          tau = floor(0.5+stats::runif(dplyr::n(), min = min, max = max))
        ) %>%
        dplyr::filter(tau>=truncate) %>%
        dplyr::summarise(count = dplyr::n(), .by = c(tau)) %>%
        dplyr::mutate(count = count+ifelse(add_noise, stats::runif(dplyr::n(),min=-1,max=1))) %>%
        dplyr::mutate(count = ifelse(count<0,0,count)) %>%
        tidyr::complete(tau = tidyr::full_seq(tau,1), fill=list(count=0)) %>%
        dplyr::mutate(
          a0 = pmax(tau - 0.5,truncate),
          a1 = tau + 0.5
        ) %>%
        dplyr::mutate(probability = count/sum(count), boot=i) %>%
        dplyr::select(-count)
    })) %>% dplyr::group_by(boot)
  })
}



#' Make an infectivity profile from posterior samples
#'
#' The infectivity profile is typically fitted to data by MCMC as a gamma
#' distribution. This function generates a discrete infectivity probability
#' distribution representing the chance that an infectee was infected on any
#' specific day after the infector was infected (given that the infectee was
#' infected), from posterior samples.
#'
#' If using `EpiEstim` and `coarseDataTools::dic.fit.mcmc` the output of the
#' MCMC will be a S4 object with a `samples` slot, containing a dataframe of
#' `shape=var1` and `scale=var2` columns. to use this output with
#' `make_posterior_ip` invoke it like this:
#'
#' `do.call(make_posterior_ip, SI_fit_clever@samples %>% dplyr::rename(shape=var1, scale=var2))`
#'
#' N.b. only one combination of mean and sd, shape and rate, or shape and scale,
#' are required.
#'
#' @param ... not used, must be empty
#' @param mean a vector of gamma distribution means
#' @param sd a vector of gamma distribution sds
#' @param shape a vector of gamma distribution shape parameters
#' @param rate a vector of gamma distribution rate parameters
#' @param scale a vector of gamma distribution scale parameters
#' @param epiestim_compat Use `EpiEstim` to generate the infectivity profiles.
#'   A true value here results in an infectivity profile with probability of 0
#'   for day 0.
#' @param n_boots if there are more posterior samples than this limit then a maximum
#'   of `n_boots` ip distributions will be created (randomly sampled).
#'
#' @return a long format `ip` delay distribution
#' @export
#' @concept delay_distribution
#'
#' @examples
#'
#' tmp = make_posterior_ip(
#'   mean = stats::rnorm(100,5,0.1),
#'   sd = stats::rnorm(100,1.5,0.1)
#' )
#' tmp %>% dplyr::glimpse()
#' if (interactive()) plot_ip(tmp)
#'
make_posterior_ip = function(..., mean, sd, shape, rate, scale, epiestim_compat = FALSE, n_boots=100) {

  rlang::check_dots_empty()

  interfacer::resolve_missing(
    shape = mean^2/sd^2,
    scale = sd^2/mean,
    rate = 1/scale,
    scale = 1/rate,
    mean = shape*scale,
    sd = sqrt(shape)*scale,
  )

  tmp = tibble::tibble(
    shape = shape, rate=rate, mean=mean, sd=sd
  )

  if (nrow(tmp) > n_boots) tmp = dplyr::slice_sample(tmp, n = n_boots)

  if (epiestim_compat) {
    tmp2 = tmp %>% .epiestim_discretise()
  } else {
    tmp2 = tmp %>% .ggoutbreak_discretise()
  }

  return(tmp2 %>% dplyr::group_by(boot))

}


## Utils ----

#' Generate a single infectivity profile from multiple bootstraps
#'
#' @iparam ip the infectivity profile to summarise. `a0` and `a1` columns are
#'   optional if `tau` is given.
#'
#' @return an infectivity profile
#' @export
#' @concept delay_distribution
summarise_ip = function(ip = i_empirical_ip) {

  ip = interfacer::ivalidate(ip, .imap = interfacer::imapper(
    a0 = pmax(tau-0.5,0), a1=tau+0.5))

  ip_summ = .summary_ip_no_chk(ip)

  return(ip_summ)

}

# without the ivalidate check
.summary_ip_no_chk = function(ip) {
  if (dplyr::n_groups(ip) == 1) return(ip %>% dplyr::mutate(boot = 1))
  ip %>%
    dplyr::group_by(tau,a0,a1) %>%
    dplyr::summarise(probability = mean(probability)) %>%
    dplyr::mutate(boot = 1) %>%
    dplyr::group_by(boot)
}

#' Print a summary of an infectivity profile
#'
#' @iparam ip the infectivity profile to summarise. `a0` and `a1` columns are
#'   optional if `tau` is given.
#'
#' @return an infectivity profile description
#' @export
#' @concept delay_distribution
#' @examples
#' format_ip(ganyani_ip)
#'
format_ip = function(ip = i_empirical_ip) {
  ip = interfacer::ivalidate(ip, .imap = interfacer::imapper(
    a0 = pmax(tau-0.5,0), a1=tau+0.5))

  boots = dplyr::n_distinct(ip$boot)
  times = dplyr::n_distinct(ip$tau)

  ip_summ = ip %>%
    dplyr::group_by(boot) %>%
    dplyr::summarise(
      mean = sum(probability*(a1+a0)/2),
      sum = sum(probability),
      #TODO: recheck this
      sd = suppressWarnings(sqrt(sum(probability*(a1^3-a0^3)/3)-(sum(probability*(a1+a0)/2)^2)))
    )

  is_pdf = all(abs(ip_summ$sum-1) < sqrt(.Machine$double.eps))

  if (is_pdf) {
    if (boots>1) {
      ip_summ = ip_summ %>%
        dplyr::summarise(
          median_of_sum = stats::quantile(sum, 0.5),
          lower_ci_of_sum = stats::quantile(sum, 0.025),
          upper_ci_of_sum = stats::quantile(sum, 0.975),
          median_of_mean = stats::quantile(mean, 0.5),
          lower_ci_of_mean = stats::quantile(mean, 0.025),
          upper_ci_of_mean = stats::quantile(mean, 0.975),
          median_of_sd = stats::quantile(sd, 0.5),
          lower_ci_of_sd = stats::quantile(sd, 0.025),
          upper_ci_of_sd = stats::quantile(sd, 0.975)
        )
        return(sprintf("PDF: mean: %1.3g [%1.3g \u2014 %1.3g]; sd: %1.3g [%1.3g \u2014 %1.3g]; %d bootstraps",
                ip_summ$median_of_mean,ip_summ$lower_ci_of_mean,ip_summ$upper_ci_of_mean,
                ip_summ$median_of_sd,ip_summ$lower_ci_of_sd,ip_summ$upper_ci_of_sd,
                boots
        ))

    } else {
        return(sprintf("mean: %1.3g; sd: %1.3g",
                       ip_summ$mean,
                       ip_summ$sd
        ))
    }

  } else {
    if (boots>1) {
      ip_summ = ip_summ %>%
        dplyr::summarise(
          median_of_sum = stats::quantile(sum, 0.5),
          lower_ci_of_sum = stats::quantile(sum, 0.025),
          upper_ci_of_sum = stats::quantile(sum, 0.975),
          median_of_mean = stats::quantile(mean, 0.5),
          lower_ci_of_mean = stats::quantile(mean, 0.025),
          upper_ci_of_mean = stats::quantile(mean, 0.975)
        )
      return(sprintf("total: %1.3g [%1.3g \u2014 %1.3g]; mean: %1.3g [%1.3g \u2014 %1.3g]; %d bootstraps",
                     ip_summ$median_of_sum,ip_summ$lower_ci_of_sum,ip_summ$upper_ci_of_sum,
                     ip_summ$median_of_mean/times,ip_summ$lower_ci_of_mean/times,ip_summ$upper_ci_of_mean/times,
                     boots
      ))
    } else {
      return(sprintf("total: %1.3g; mean: %1.3g;",
                     ip_summ$sum,
                     ip_summ$mean/times
      ))
    }
  }

}


## Internal infectivity profile utilities ----

#' Generate a infectivity profile matrix from a long format
#'
#' Makes sure that this starts at zero. No guarantee matric columns adds up to 1.
#'
#' @param ip long format infectivity profile
#'
#' @return a maxrix
#' @noRd
#'
#' @examples
#' .omega_matrix(ggoutbreak::covid_ip)
.omega_matrix = function(ip = i_discrete_ip, epiestim_compat = TRUE) {

  ip = interfacer::ivalidate(ip, .prune = TRUE)

  if (epiestim_compat) ip = ip %>% dplyr::filter(tau >= 1)

  # Make sure matrix includes one entry for tau=0: The pivot_wider will do the rest
  if (min(ip$tau) > 0) {
    ip = ip %>% dplyr::bind_rows(tibble::tibble(
      tau = 0:(min(ip$tau)-1),
      probability = 0,
      boot = ip$boot[1]
    ))
  }

  omega = ip %>%
    tidyr::pivot_wider(
      names_from = boot,
      names_prefix = "boot.",
      values_from = probability,
      values_fill = 0  # missing values have a probability of zero.
    ) %>%
    dplyr::arrange(tau) %>%
    dplyr::select(-tau) %>%
    as.matrix()
  return(omega)
}

#' Create samples from uncertain distribution using code from EpiEstim
#'
#' @param n1 number of samples needed
#' @param mean_si,std_mean_si,min_mean_si,max_mean_si Mean SI params
#' @param std_si,std_std_si,min_std_si,max_std_si SD SI params
#' @noRd
#'
#' @return a data frame with mean and sd columns
#'
#' @examples
#' tmp = .epiestim_sampler(
#'   100,
#'   mean_si = 5,
#'   std_mean_si = 2,
#'   std_si = 3.5,
#'   std_std_si = 0.5
#' )
#' tmp %>% dplyr::glimpse()
.epiestim_sampler = function(n1,
                             mean_si, std_mean_si, min_mean_si = 1, max_mean_si = mean_si+3*std_mean_si,
                             std_si, std_std_si, min_std_si = 0, max_std_si = std_si+3*std_std_si
) {
  mean_si_sample <- rep(-1, n1)
  std_si_sample <- rep(-1, n1)

  for (k in seq_len(n1)) {
    while (mean_si_sample[k] < min_mean_si ||
           mean_si_sample[k] > max_mean_si) {
      mean_si_sample[k] <- stats::rnorm(1, mean = mean_si,
                                        sd = std_mean_si)
    }
    while (std_si_sample[k] < min_std_si ||
           std_si_sample[k] > max_std_si) {
      std_si_sample[k] <- stats::rnorm(1, mean = std_si,
                                       sd = std_std_si)
    }
  }

  return(tibble::tibble(
    mean = mean_si_sample,
    sd = std_si_sample
  ) %>% dplyr::mutate(
    shape = (mean^2)/(sd^2),
    rate = mean/(sd^2)
  ))
}


#' Create samples from uncertain distribution using correlated log normals
#'
#' @param n1 number of samples needed
#' @param mean_of_mu,std_of_mu Log scaled mean SI params,
#' @param mean_of_sigma,std_of_sigma Log scaled SD SI params
#' @param correlation The correlation between log(mean) and log(sd) for the SI estimates
#' @noRd
#'
#' @return a data frame with mean and sd columns
#'
#' @examples
#' tmp = .ggoutbreak_sampler(
#'   100,
#'   mean_si = 5,
#'   std_mean_si = 2,
#'   std_si = 3.5,
#'   std_std_si = 0.5
#' )
#' tmp %>% dplyr::glimpse()
.ggoutbreak_sampler = function(n1, mean_of_mu, sd_of_mu, mean_of_sigma, sd_of_sigma, correlation = NA) {

  if (is.na(correlation)) {
    # This heuristic is the result of a linear model on a test data set.
    correlation = 0.64 - 0.15*(mean_of_mu) + 0.18*(mean_of_sigma) - 0.02*(mean_of_mu*mean_of_sigma)
  }
  correlation = min(c(1,max(c(0,correlation))))

  # means here is mean of mu=log(mean) and mean of sigma=log(sd)
  means = c(mean_of_mu,mean_of_sigma)
  names(means) = c("lmean","lsd")

  # sds here is sd of mu=log(mean) and sd of sigma=log(sd)
  sds = c(sd_of_mu,sd_of_sigma)
  names(sds) = c("lmean","lsd")

  corMatrix = matrix(c(1,correlation,correlation,1),nrow = 2)
  covMatrix = diag(sds) %*% corMatrix %*% diag(sds)
  colnames(covMatrix) = names(sds)
  rownames(covMatrix) = names(sds)

  simulated = MASS::mvrnorm(n=n1, mu=means, Sigma = covMatrix) %>% as.data.frame()
  simulated = simulated %>% dplyr::mutate(
    mean = exp(lmean),
    sd = exp(lsd),
    shape = (mean^2)/(sd^2),
    rate = mean/(sd^2))
  return(simulated)
}

#' Discretise using `Epiestim::discr_si` function
#'
#' @param params_df a dataframe with mean, sd, shape and rate columns
#'
#' @return an infectivity profile in long dataframe format.
#' @noRd
#'
#' @examples
#' tmp = .epiestim_sampler(
#'   100,
#'   mean_si = 5,
#'   std_mean_si = 2,
#'   std_si = 3.5,
#'   std_std_si = 0.5
#' )
#' tmp2 = tmp %>% .epiestim_discretise()
.epiestim_discretise = function(params_df) {

  max_tau = params_df %>% dplyr::mutate(
    tau = purrr::map2_dbl(shape, rate, ~stats::qgamma(0.999,.x,.y))
  ) %>% dplyr::summarise(max_tau = ceiling(max(tau))) %>% dplyr::pull(max_tau)

  out = params_df %>%
    dplyr::filter(mean>1) %>%
    dplyr::mutate(
      discr_si = purrr::map2(
        mean, sd, ~ tibble::tibble(
          tau = 0:max_tau,
          a0 = c(0,seq(0.5,length.out = max_tau)),
          a1 = seq(0.5,length.out = max_tau+1),
          probability = EpiEstim::discr_si(k=0:max_tau, mu = .x,sigma = .y)
        ) %>% dplyr::mutate(probability = probability/sum(probability))
      ),
      boot = dplyr::row_number()
    ) %>% dplyr::select(-mean,-sd) %>% tidyr::unnest(discr_si)

  return(out)
}


#' Discretise using gamma cdf function
#'
#' This produces a infectivity profile with non zero probability of infection at time zero
#'
#' @param params_df a dataframe with shape,rate,mean and sd columns
#'
#' @return an infectivity profile in long dataframe format.
#' @noRd
#'
#' @examples
#' tmp = .epiestim_sampler(
#'   100,
#'   mean_si = 5,
#'   std_mean_si = 2,
#'   std_si = 3.5,
#'   std_std_si = 0.5
#' )
#' tmp2 = tmp %>% .ggoutbreak_discretise()
#' tmp2 %>% dplyr::group_by(boot) %>%
#'   dplyr::summarise(mean = sum(tau*probability)) %>%
#'   dplyr::summarise(mean_of_mean = mean(mean), sd_of_mean=stats::sd(mean))
.ggoutbreak_discretise = function(params_df) {

  max_tau = params_df %>% dplyr::mutate(
    tau = purrr::map2_dbl(shape, rate, ~stats::qgamma(0.999,.x,.y))
  ) %>% dplyr::summarise(max_tau = ceiling(max(tau))) %>% dplyr::pull(max_tau)

  out = params_df %>%
    dplyr::mutate(
      discr_si = purrr::map2(
        shape, rate, ~ tibble::tibble(
          tau = 0:max_tau,
          a0 = c(0,seq(0.5,length.out = max_tau)),
          a1 = seq(0.5,length.out = max_tau+1),
          probability = dplyr::lead(stats::pgamma(a0, .x ,.y),default = 1) - stats::pgamma(a0, .x ,.y)
        )
      ),
      boot = dplyr::row_number()
    ) %>% dplyr::select(-mean,-sd,-shape,-rate) %>% tidyr::unnest(discr_si)

  return(out)
}

