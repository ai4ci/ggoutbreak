#' Wallinga-Lipsitch reproduction number from growth rates
#'
#' Calculate a reproduction number estimate from growth rate using the Wallinga
#' and Lipsitch 2007 estimation using empirical generation time distribution.
#' This uses resampling to transmit uncertainty in growth rate estimates. This
#' also handles time-series that are not on a daily cadence (although this is
#' experimental). The reproduction number estimate is neither a instantaneous
#' (backward looking) nor case (forward looking) reproduction number but
#' somewhere between the two, as the method looks at a flux of infection at a
#' single point in time.
#'
#' This method is quite slow compared to some others and the default is non
#' deterministic.
#'
#' @iparam df Growth rate estimates
#' @iparam ip Infectivity profile
#' @param bootstraps - the number of bootstraps to take to calculate for each point.
#' @param seed a random number generator seed
#' @param .progress show a CLI progress bar
#'
#' @return `r i_reproduction_number`
#' @export
#' @concept models
#' @examples
#' data = ggoutbreak::test_poisson_rt_smooth
#'
#' tmp = data %>%
#'   poisson_locfit_model() %>%
#'   rt_from_growth_rate(ip=ggoutbreak::test_ip)
#'
#' if (interactive()) {
#'   plot_rt(tmp, date_labels="%b %y")+sim_geom_function(data,colour="red")
#' }
#'
rt_from_growth_rate = function(
  df = i_growth_rate,
  ip = i_empirical_ip,
  bootstraps = 1000,
  seed = Sys.time(),
  .progress = interactive()
) {
  df = interfacer::ivalidate(df)
  ip = interfacer::ivalidate(
    ip,
    .imap = interfacer::imapper(a0 = pmax(tau - 0.5, 0), a1 = tau + 0.5)
  )

  # e.g. converting a weekly to a daily growth rate
  # exp(r_wk) = exp(r_daily)^7
  # r_wk = r_daily*7
  .daily_unit = .step(df$time)

  ip_boots = ip %>% dplyr::n_groups()
  boots_per_ip = max(ceiling(bootstraps / ip_boots), 10)
  boots = boots_per_ip * ip_boots

  df = df %>%
    dplyr::mutate(
      rt = purrr::map2(
        growth.fit,
        growth.se.fit,
        .progress = .progress,
        function(mean_r, sd_r) {
          mean_r = .daily_unit * mean_r
          sd_r = .daily_unit * sd_r

          # rather than bootstrap samples we sample quantiles of growth rate
          # to ensure a representative distribution
          # qnts = seq(0,1,length.out = boots_per_ip+3)[2:(boots_per_ip+2)]
          # r_samples = tibble::tibble(r = stats::qnorm(p=qnts,mean_r,sd_r)) %>%
          #   dplyr::mutate(r_i = dplyr::row_number())

          r_samples = withr::with_seed(seed, {
            tibble::tibble(
              r = stats::rnorm(boots, mean_r, sd_r),
              boot = rep(unique(ip$boot), boots_per_ip)
            ) %>%
              dplyr::group_by(boot) %>%
              dplyr::mutate(r_i = dplyr::row_number())
          })

          tmp = ip %>%
            dplyr::inner_join(r_samples, by = "boot") %>%
            dplyr::rename(y = probability) %>%
            dplyr::group_by(boot, r_i) %>%
            dplyr::arrange(boot, r_i) %>%
            dplyr::mutate(
              R = r /
                sum(
                  y *
                    (exp(-r * a0) - exp(-r * a1)) /
                    (a1 - a0)
                )
            ) %>%
            dplyr::mutate(R = ifelse(r == 0, 1, R))

          R_summ = tmp %>%
            dplyr::ungroup() %>%
            dplyr::summarise(
              rt.fit = mean(R, na.rm = TRUE),
              rt.se.fit = stats::sd(R, na.rm = TRUE),
              rt.samples = list(R)
            ) %>%
            .result_from_fit(
              "rt",
              # This format is needed because quantile is not vectorised on data:
              qfn = function(p) {
                purrr::map_dbl(.$rt.samples, function(data) {
                  if (all(is.na(data))) {
                    return(NA_real_)
                  }
                  stats::quantile(data, p, na.rm = TRUE)
                })
              }
            ) %>%
            .keep_cdf(type = "rt", .$rt.samples, link = "log") %>%
            dplyr::select(-rt.samples)

          return(R_summ)
        }
      )
    )

  out = df %>% tidyr::unnest(rt)
  interfacer::ireturn(out, i_reproduction_number)
}


#' Calculate the reproduction number from a growth rate estimate and an infectivity profile
#'
#' This function uses a single empirical distribution for the infectivity
#' profile / generation time. If multiple are provided then the average central
#' value is chosen (i.e. this does not propagate uncertainty in infectivity profile)
#'
#' @param r a growth rate (may be a vector)
#' @param y an empirical infectivity profile either as a probability vector or as a dataframe of format:
#'   `r i_empirical_ip`
#' @param a1 the end time of the infectivity profile probability estimate (defaults to 0.5,1.5,2.5,...).
#' @param a0 the start time of the infectivity profile probability estimate (defaults to 0,0.5,1.5,...).
#'
#' @return a reproduction number estimate based on `r`
#' @export
#' @concept models
#'
#' @examples
#'
#' # using a probability vector.
#' wallinga_lipsitch(r=seq(-0.1,0.1,length.out=9), y=stats::dgamma(1:50, 5,2))
#'
#' # using an infectivity profile
#' wallinga_lipsitch(r=seq(-0.1,0.1,length.out=9), y=test_ip)
#'
wallinga_lipsitch = function(
  r,
  y = i_empirical_ip,
  a1 = seq(0.5, length.out = length(y)),
  a0 = dplyr::lag(a1, default = 0)
) {
  if (is.data.frame(y)) {
    ip = interfacer::ivalidate(
      y,
      .imap = interfacer::imapper(a0 = pmax(tau - 0.5, 0), a1 = tau + 0.5)
    )
    ip = .summary_ip_no_chk(ip)
    a0 = ip$a0
    a1 = ip$a1
    y = ip$probability
  }

  y = y / sum(y)
  tmp = sapply(r, function(r2) {
    R = r2 /
      sum(
        y *
          (exp(-r2 * a0) - exp(-r2 * a1)) /
          (a1 - a0)
      )
    return(R)
  })
  return(ifelse(r == 0, 1, tmp))
}


#' Calculate a growth rate from a reproduction number and an infectivity profile,
#'
#' This solves the relationship between $R_t$ and growth rates as described by
#' Wallinga and Lipsitch, to get a growth rate from $R_t$ and infectivity
#' profile.
#'
#' This function uses a single empirical distribution for the infectivity
#' profile / generation time. If multiple are provided then the average central
#' value is chosen (i.e. this does not propagate uncertainty in infectivity profile)
#'
#' @param Rt a vector of reproduction numbers
#' @param y an empirical infectivity profile as a probability vector or as a
#'   dataframe of format:
#'   `r i_empirical_ip`
#' @param a1 the end time of the infectivity profile probability estimate (defaults to 0.5,1.5,2.5,...).
#' @param a0 the start time of the infectivity profile probability estimate (defaults to 0,0.5,1.5,...).
#'
#' @return an vector of growth rates
#' @export
#' @concept models
#'
#' @examples
#' inv_wallinga_lipsitch(Rt=seq(0.5,2.5,length.out=9), y=test_ip)
#'
inv_wallinga_lipsitch = function(
  Rt,
  y = i_empirical_ip,
  a1 = seq(0.5, length.out = length(y)),
  a0 = dplyr::lag(a1, default = 0)
) {
  if (is.data.frame(y)) {
    ip = interfacer::ivalidate(
      y,
      .imap = interfacer::imapper(a0 = pmax(tau - 0.5, 0), a1 = tau + 0.5)
    )
    ip = .summary_ip_no_chk(ip)
    a0 = ip$a0
    a1 = ip$a1
    y = ip$probability
  }

  Tc = sum(y * (a1 - a0) * (a0 + a1) / 2) # mean generation interval

  sapply(Rt, function(R) {
    r_delta = log(R) / Tc
    r_exp = (R - 1) / Tc
    f = function(r) wallinga_lipsitch(r, y, a1, a0) - R
    if (R == 1) {
      return(0)
    }
    stats::uniroot(f, interval = c(r_delta, r_exp), extendInt = "yes")$root
  })
}
