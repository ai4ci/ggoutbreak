


#' Identify estimate lags in a model
#'
#' A specific parameter or set of parameters can be estimated by a `pipeline`.
#' This function applies the `pipeline` to a synthetic epidemic with sawtooth
#' incidence resulting from a stepped growth rate function. The lag between
#' synthetic input and estimate is assessed by minimising the root mean square
#' error of input and estimated based on different lag offsets.
#'
#' @param pipeline a function taking an input dataset and an infectivity profile as
#'  inputs and producing an estimate as output. This is the whole parametrised
#'  pipeline including any other inputs. This can be a `purrr` style function.
#' @iparam ip the infectivity profile.
#' @param lags a vector with the delays to test. Defaults to -10 to +30 days
#'
#' @return a lag analysis dataframe containing the `estimate` type and the `lag`
#'   in days that the estimate is behind the actual observation
#' @export
#'
#' @examples
#' pipeline = ~ .x %>% poisson_locfit_model() %>% rt_from_incidence(ip = .y)
#' lag_analysis = quantify_lag(pipeline)
#'
#' quantify_lag(~ rt_epiestim(.x,ip = .y))
#'
quantify_lag = function(pipeline, ip = i_empirical_ip, lags = -10:30) {

  ip = summarise_ip(ip)
  # deterministic sawtooth function based on IP
  data = sim_test_data(ip = ip)

  pipeline = rlang::as_function(pipeline)

  est = pipeline(data,ip)

  tmp = .long_quantiles(est) %>% dplyr::filter(.p == 0.5) %>% dplyr::select(-.p)
  tmp2 = data %>%
    dplyr::select(-count,-denom) %>%
    tidyr::pivot_longer(cols = c(growth,incidence,rt,proportion,relative.growth),names_to = ".type",values_to = ".ref") %>%
    dplyr::filter(!is.na(.ref))


  test = tmp %>% dplyr::inner_join(tmp2,by=c("time",".type")) %>% dplyr::group_by(.type)

  lag_data = dplyr::bind_rows(lapply(lags, function(lag) {
    test %>% dplyr::summarise(
        lagged_rmse = sqrt(mean((
          .lag(.ref,n = lag,default = NA)-.value)^2,na.rm = TRUE)),
        lag= lag
      )
  })) %>% dplyr::group_by(estimate = .type) %>%
    dplyr::mutate(lagged_rmse = lagged_rmse/min(lagged_rmse))

  lag_summ = lag_data %>% dplyr::arrange(lagged_rmse) %>%
    dplyr::filter(dplyr::row_number()==1) %>% dplyr::select(estimate,lag)

  p = ggplot2::ggplot(lag_data, ggplot2::aes(x=lag,y=lagged_rmse,colour=estimate)) +
    ggplot2::geom_point()  +
    ggplot2::geom_line() + ggplot2::xlab("estimate lag (days)") +
    ggplot2::ylab("relative RMSE")

  return(structure(
    lag_summ,
    plot = p,
    data = lag_data
  ))

}

.lag = function(x,n,...) {
  if (n<0) return(dplyr::lead(x,n=-n,...))
  else return(dplyr::lag(x,n=n,...))
}

.long_quantiles = function(est) {
  grps = est %>% dplyr::groups()
  est %>%
    dplyr::select(
      !!!grps,
      time,
      tidyselect::matches("\\.0\\.[0-9]+$")
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches("\\.0\\.[0-9]+$"),
      names_pattern = "^(.*)\\.(0\\.[0-9]+$)",
      names_to = c(".type",".p"),
      values_to = ".value"
    ) %>% dplyr::mutate(
      .p = as.numeric(.p)
    )
}

#' Calculate scoring statistics from predictions using `scoringutils`.
#'
#' This performs a range of quantile based, and if cumulative distribution
#' functions are available, continuous scoring metrics for each estimate time-point.
#'
#' @param est a dataframe of estimates of incidence, growth rate of reproduction
#'   number based off a simulation or data with known parameters.
#' @param obs a dataframe of the ground truth, sharing the same grouping as
#'   `est` with at least one column(s) named `XXX.obs` with `XXX` being one of
#'   `rt`,`growth` or `incidence` or any other column group predicted in `est`.
#' @param lags a data frame of estimate types and lags as output by [quantify_lag()]
#'   if multiple models are included then the columns must match those in `obs`.
#'
#' @return a dataframe of scoring metrics
#' @export
#'
#' @examples
#' tmp2 = test_bpm %>% sim_summarise_linelist()
#'
#' withr::with_options(list("ggoutbreak.keep_cdf"=TRUE),{
#'    est = tmp2 %>% poisson_locfit_model() %>% rt_from_incidence()
#' })
#'
#' obs = tmp2 %>% dplyr::mutate(rt.obs = rt.weighted)
#' score_estimate(est,obs) %>% dplyr::glimpse()
#'
score_estimate = function(est, obs, lags = NULL) {

  if (is.null(lags)) lags = tibble::tibble(
    estimate = c("incidence","rt","growth","proportion","relative.growth"),
    lag = 0
  )

  join_cols = intersect(colnames(est),colnames(obs))
  join_cols_2 = intersect(colnames(obs),colnames(lags))

  obs_cols = setdiff(colnames(obs),colnames(est))
  obs_type = stringr::str_extract(obs_cols,"(.*)\\.obs",1)
  obs_type = obs_type[!is.na(obs_type)]

  long_est = .long_quantiles(est)


  long_obs = obs %>%
    dplyr::select(tidyselect::all_of(join_cols), tidyselect::ends_with(".obs")) %>%
    tidyr::pivot_longer(cols=tidyselect::ends_with(".obs"),names_to = ".type",values_to = ".ref",names_pattern = "([^\\.]+)\\.obs") %>%
    dplyr::left_join(lags, by=c(join_cols_2,".type"="estimate")) %>%
    # shift the observed to meet the estimate.
    dplyr::mutate(time=time+lag)

  if (any(stringr::str_detect(colnames(est),"\\.cdf$"))) {

    long_cdf = est %>%
      dplyr::select(tidyselect::all_of(join_cols), tidyselect::ends_with(".cdf")) %>%
      tidyr::pivot_longer(cols=tidyselect::ends_with(".cdf"),names_to = ".type",values_to = ".cdf",names_pattern = "([^\\.]+)\\.cdf")


    crps_data = long_cdf %>%
      dplyr::inner_join(long_obs, by=c(join_cols,".type"))

    crps_data = crps_data %>%
      dplyr::group_modify(function(d,g,...) {
        d %>% dplyr::mutate(
          crps = .crps(.$.ref, .$.cdf),
          bias = .bias(.$.ref, .$.cdf)
        ) %>% dplyr::select(
          -.cdf
        )
      })
  } else {
    crps_data = long_obs
  }


  scores = long_est %>%
    dplyr::inner_join(long_obs, by=c(join_cols,".type")) %>%
    dplyr::rename(true_value = .ref, prediction = .value, quantile = .p)

  if(!"model" %in% colnames(scores)) scores = scores %>% dplyr::mutate(model="undefined")

  scores = scores %>%
    scoringutils:::score(metrics = setdiff(scoringutils::available_metrics(),c("crps","bias"))) %>%
    scoringutils::summarise_scores() %>%
    tibble::as_tibble() %>%
    dplyr::left_join(crps_data %>% dplyr::select(-lag), by=c(join_cols,".type")) %>%
    dplyr::rename(true_value = .ref, estimate=.type) %>%
    group_by(model,!!!(est %>% groups()),estimate)

  return(scores)


}



# vectorised CRPS calculation given list of CDFs and true values
# tmp2 = .cdf_generator(mean=list(c(1,2,3),c(3,4,5)),sd=list(c(1,1,1),c(2,2,2)))
# .crps(c(2,2), tmp2, min=-100,max=100)
.crps = function(true, cdf, min=-Inf, max=Inf) {
  if (is.function(cdf)) cdf=list(cdf)
  if (rlang::is_missing(min) || rlang::is_missing(max)) stop("must define a min/max range for crps.",call. = FALSE)
  interfacer::check_numeric(min,max)
  interfacer::recycle(true,cdf,min,max)
  purrr::map_dbl(seq_along(true), \(i) {
    x=true[[i]]
    fn=cdf[[i]]
    tryCatch(
      stats::integrate(f=\(y) fn(y)^2, lower=min[i], upper=x, stop.on.error=FALSE)$value +
        + stats::integrate(f=\(y) (fn(y)-1)^2, lower=x, upper=max[i],stop.on.error=FALSE)$value ,
      error = function(e) { NA}
    )
  })
}

.bias = function(true, cdf) {
  interfacer::recycle(true,cdf,min,max)
  purrr::map_dbl(seq_along(true), \(i) {
    x=true[[i]]
    fn=cdf[[i]]
    return(1-2*fn(x))
  })
}
