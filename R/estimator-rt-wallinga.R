
#' Wallinga-Lipsitch reproduction number
#'
#' Calculate a reproduction number estimate from growth rate using the Wallinga
#' 2007 estimation using empirical generation time distribution. This uses
#' resampling to transmit uncertainty in growth rate estimates
#'
#' @iparam df Growth rate estimates
#' @iparam ip Infectivity profile
#' @param bootstraps - the number of bootstraps to take to calculate for each point.
#'
#' @return `r i_reproduction_number`
#' @export
#' @concept models
#' @examples
#' tmp = ggoutbreak::england_covid %>%
#'   time_aggregate(count=sum(count))
#'
#'
#' if (FALSE) {
#'   # not run
#'   tmp2 = tmp %>%
#'     poisson_locfit_model() %>%
#'     rt_from_growth_rate()
#' }
#'
rt_from_growth_rate = function(df = i_growth_rate, ip = i_infectivity_profile, bootstraps = 2000) {

  df = interfacer::ivalidate(df)
  ip = interfacer::ivalidate(ip)

  # e.g. converting a weekly to a daily growth rate
  # exp(r_wk) = exp(r_daily)^7
  # r_wk = r_daily*7
  .daily_unit = .step(df$time)

  ip_boots = ip %>% dplyr::n_groups()
  boots_per_ip = max(bootstraps %/% ip_boots,10)

  df = df %>% dplyr::mutate(
    rt = purrr::map2(growth.fit, growth.se.fit, .progress = interactive(), function(mean_r, sd_r) {

      mean_r = .daily_unit*mean_r
      sd_r = .daily_unit*sd_r

      # rather than bootstrap samples we sample quantiles of growth rate
      # to ensure a representative distribution
      # qnts = seq(0,1,length.out = boots_per_ip+3)[2:(boots_per_ip+2)]
      # r_samples = tibble::tibble(r = stats::qnorm(p=qnts,mean_r,sd_r)) %>%
      #   dplyr::mutate(r_i = dplyr::row_number())

      r_samples = tibble::tibble(
          r = stats::rnorm(boots_per_ip*ip_boots, mean_r, sd_r),
          boot = rep(unique(ip$boot),boots_per_ip)
        ) %>%
        dplyr::group_by(boot) %>%
        dplyr::mutate(r_i = dplyr::row_number())

      tmp = ip %>%
        dplyr::inner_join(r_samples, by="boot") %>%
        dplyr::rename(y = probability, a = time) %>%
        # get rid of a=0, y=0 if given.
        dplyr::filter(a > 0) %>%
        dplyr::group_by(boot,r_i) %>%
        dplyr::arrange(boot,r_i,a) %>%
        dplyr::mutate(
          R = r/sum(
            y
              *
            (exp(-r*dplyr::lag(a,default=0))-exp(-r*a))
              /
            (a - dplyr::lag(a,default=0))
          )
        )

      R_summ = tmp %>% dplyr::ungroup() %>% dplyr::summarise(
        rt.fit = mean(R, na.rm=TRUE),
        rt.se.fit = stats::sd(R, na.rm=TRUE),
        rt.0.025 = stats::quantile(R, probs=0.025, na.rm=TRUE),
        rt.0.5 = stats::quantile(R, probs=0.5, na.rm=TRUE),
        rt.0.975 = stats::quantile(R, probs=0.975, na.rm=TRUE),
      )
      return(R_summ)
    })
  )

  out = df %>% tidyr::unnest(rt)
  interfacer::ireturn(out, i_reproduction_number)

}




#' Calculate the reproduction number from a growth rate estimate and an infectivity profile
#'
#' @param r a growth rate (may be a vector)
#' @param y an empirical infectivity profile as a probability vector, starting at `P(0<t,a[1])`
#' @param a the end time of the estimate (defaults to single days).
#'
#' @return a reproduction number estimate based on `r`
#' @export
#'
#' @examples
#' wallinga_lipsitch(r=seq(-0.1,0.1,length.out=9), y=dgamma(1:50, 5,2))
wallinga_lipsitch = function(r, y, a=1:length(y)) {

  y = y/sum(y)
  tmp = sapply(r, function(r2) {
    R = r2/sum(
      y
      *
        (exp(-r2*dplyr::lag(a,default=0))-exp(-r2*a))
      /
        (a - dplyr::lag(a,default=0))
    )
    return(R)
  })
  return(ifelse(r==0,1,tmp))
}


