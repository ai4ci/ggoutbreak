
#' Reproduction number from modelled incidence
#'
#' Calculate a reproduction number estimate from growth rate using the methods
#' described in the vignette "Estimating the reproduction number from modelled
#' incidence" and using an empirical generation time distribution.
#'
#' @iparam df Count data
#' @iparam ip Infectivity profile
#'
#' @return `r i_reproduction_number`
#' @export
#' @concept models
#' @examples
#' df = ggoutbreak::england_covid %>%
#'   time_aggregate(count=sum(count)) %>%
#'     poisson_locfit_model()
#'
#'
#' if (FALSE) {
#'   # not run
#'   tmp2 = df %>% rt_from_incidence()
#' }
#'
rt_from_incidence = function(df = i_incidence_model, ip = i_infectivity_profile) { #, assume_start = TRUE) {

  ip = interfacer::ivalidate(ip)

  # omega is a matrix
  omega = ip %>%
    tidyr::pivot_wider(names_from = boot, values_from = probability) %>%
    dplyr::arrange(time) %>%
    dplyr::select(-time) %>%
    as.matrix()

  window = nrow(omega)

  interfacer::igroup_process(df, function(df,omega,window,...) {

    rt = lapply(1:nrow(df), function(i) {
      if (i<=window+1) {
        return(tibble::tibble())
        # if (!assume_start || i<5) return(tibble::tibble())

        # pad_mu = rep(df$incidence.fit[1],window-i+1)
        # pad_sigma = rep(df$incidence.se.fit[1],window-i+1)

        # mu_t = c(pad_mu,df$incidence.fit[1:(i-1)])
        # sigma_t = c(pad_sigma,df$incidence.se.fit[1:(i-1)])
        # omega = omega[1:(i-1)]/sum(omega[1:(i-1)])
        # mu_t = df$incidence.fit[1:(i-1)]
        # sigma_t = df$incidence.se.fit[1:(i-1)]

      } else {
        mu_t = df$incidence.fit[(i-window):(i-1)]
        sigma_t = df$incidence.se.fit[(i-window):(i-1)]
      }
      return(.internal_r_t_estim(
        mu = df$incidence.fit[i],
        sigma = df$incidence.se.fit[i],
        omega = omega,
        mu_t = mu_t,
        sigma_t = sigma_t,
        cor = FALSE
      ))
    })

    df2 = df %>%
      dplyr::mutate(rt = rt) %>%
      tidyr::unnest(rt,keep_empty = TRUE)

    return(df2)

  })

}




.logsumexp = function(x, na.rm = FALSE) {
  if (!na.rm & any(is.na(x))) return(NA)
  x = x[!is.na(x)]
  if (all(x == -Inf)) return(-Inf)
  c = max(x)
  # remove exp(-Inf) zero terms
  x = x[x != -Inf]
  return(c+log(sum(exp(x-c))))
}

.internal_r_t_estim = function(mu, sigma, omega, mu_t, sigma_t, cor = TRUE) {

  # omega maybe matrix or vector
  omega_m = as.matrix(omega)
  # switch direction of omega to match timeseries. This eliminates need for
  # t-\tau indexes
  omega_m = apply(omega_m, MARGIN=2, rev)

  # for each infectivity profile:
  tmp = apply(omega_m, MARGIN=2, function(omega) {

    # implementing the lognormal approximation for sum of lognormals:
    # This is referenced on the wikipedia page for lognormal but is also
    # from Lo 2013 (10.2139/ssrn.2220803).
    # keep everything in log for numerical stability.
    log_S_t = .logsumexp(mu_t + sigma_t^2/2 + log(omega))
    log_T_t_tau = mu_t + sigma_t^2/2 + log(omega) + log(sigma_t)

    # not going to create a matrix here use an indexed vector instead
    if (cor) {
      # Assuming the terms are correlated does not make much of a difference to
      # outcome but requires a matrix the f omega^2
      n = length(omega)
      idx = 0:(n^2-1)
      i = idx %/% n
      j = idx %% n
      log_cor_ij = c(0,log(omega))[abs(i-j)+1]
      log_var_Zt_ij = log_cor_ij + log_T_t_tau[i+1] + log_T_t_tau[j+1]
    } else {
      # cor_ij is zero for i <> j
      log_var_Zt_ij = 2*log_T_t_tau
    }

    log_var_Zt = .logsumexp(log_var_Zt_ij) - 2*log_S_t

    var_Zt = exp(log_var_Zt)
    mu_Zt = log_S_t - var_Zt/2

    return(c(mu_Rt=mu-mu_Zt,var_Rt=sigma^2+var_Zt))
  })

  if (ncol(tmp) == 1) {
    mu_star = tmp[1]
    sigma2_star = tmp[2]
    mean_star = exp(mu_star+sigma2_star/2)
    var_star = (exp(sigma2_star)-1) * exp(2*mu_star + sigma2_star)
  } else {
    # Combine results from multiple infectivity profiles using a
    # lognormal approximation to a mixture of lognormals by matching
    # moments.
    means = exp(tmp[1,]+tmp[2,]/2)
    vars = (exp(tmp[2,])-1) * exp(2*tmp[1,]+tmp[2,])
    mean_star = mean(means)
    var_star = mean(vars+means^2)-mean_star^2
    mu_star = log(mean_star / sqrt((var_star/mean_star^2)+1))
    sigma2_star = log((var_star/mean_star^2)+1)
  }
  sigma_star = sqrt(sigma2_star)

  return(tibble::tibble(
    rt.mu = mu_star,
    rt.sigma = sigma_star,
    rt.fit = mean_star,
    rt.se.fit = sqrt(var_star),
    rt.0.025 = stats::qlnorm(0.025, mu_star, sigma_star),
    rt.0.5 = stats::qlnorm(0.5, mu_star, sigma_star),
    rt.0.975 = stats::qlnorm(0.975, mu_star, sigma_star)
  ))

}
