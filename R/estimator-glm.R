# GLM simple spline models ----

# degrees freedom from window parameter
.df_from_window = function(window, timeseries, classes=1) {
  tmp = ceiling(nrow(timeseries) / classes / window)
  if (tmp < 1 ) tmp = 1
  return(tmp)
}

.inv_logit = function(x) exp(x)/(1+exp(x))

#TODO: figure out how to extrac spline gradient
# tmp = summary(model)
# v = tmp$terms %>% attr("predvars")
# The call for the splines including the knots
# v[[3]]
# slpine coefficients & SE:
# tmp$coefficients
# tmp$standard.errors
# https://stats.stackexchange.com/questions/422068/how-to-predict-by-hand-in-r-using-splines-regression
# https://www.r-bloggers.com/2014/06/simultaneous-confidence-intervals-for-derivatives-of-splines-in-gams/


#' Binomial time-series model.
#'
#' This uses a generalised linear model to fit a quasi-binomial model with a time
#' varying rate as a natural cubic spline with approx one degree of freedom per
#' `window` units of the time series.
#'
#' @iparam d Proportion model input
#' @inheritParams poisson_locfit_model
#'
#' @return `r i_proportion_model`
#' @export
#' @concept models
#' @examples
#'
#' ggoutbreak::england_covid_proportion %>%
#'   dplyr::filter(date < "2021-01-01") %>%
#'   time_aggregate() %>%
#'   ggoutbreak::proportion_glm_model(window=5) %>%
#'   dplyr::glimpse()
#'
proportion_glm_model = function(d = i_proportion_input, ..., window = 14, frequency = "1 day", .progress=interactive()) { #, output_unit = "1 day") {

  env = rlang::current_env()
  if (.progress) cli::cli_progress_bar("proportion (GLM)", total = dplyr::n_groups(d), .envir = env)

  modelled = interfacer::igroup_process(d, function(d, ..., window, frequency) {

    output_times = date_seq.time_period(d$time, period = frequency)
    df = .df_from_window(window,timeseries = d)

    # see http://www.simonqueenborough.info/R/statistics/glm-binomial
    # for this next bit:
    d = d %>% dplyr::filter(denom != 0)
    y = cbind(d$count, d$denom - d$count)

    #' # TODO: deal with error conditions
    #' # "observations with zero weight not used for calculating dispersion

    if (df < 2) df=2
    model = stats::glm(y ~ splines::ns(time, df = df), family = stats::quasibinomial, data = d, singular.ok = TRUE)
    new_data = tibble::tibble(time = output_times)
    # response is transformed:
    # proportion_estimate = stats::predict(model, newdata = new_data, type="response")
    # this prediction in in the logit space:
    est2 = stats::predict(model, newdata = new_data, se.fit = TRUE)
    # it is possible to get CIs out here but they are the logit transformed ones.
    new_data = new_data %>%
      .result_from_fit(type = "proportion", est2$fit, est2$se.fit, model$family$linkinv) %>%
      .keep_cdf(type = "proportion", mean=est2$fit, sd=est2$se.fit, trans_fn = .logit)

    # return(interfacer::ireturn(new_data,i_proportion_model))

    if (.progress) cli::cli_progress_update(.envir = env)

    return(new_data)

  })

  if (.progress) cli::cli_progress_done()

  return(modelled)
}


#' Poisson time-series model.
#'
#' This uses a generalised linear model to fit a quasi-poisson model with a time
#' varying rate as a natural cubic spline with approx one degree of freedom per
#' `window` units of the time series.
#'
#' @iparam d Count model input
#' @inheritParams poisson_locfit_model
#'
#' @return `r i_incidence_model`
#' @export
#' @concept models
#' @examples
#' ggoutbreak::england_covid %>%
#'  dplyr::filter(date < "2021-01-01") %>%
#'  time_aggregate(count=sum(count)) %>%
#'  ggoutbreak::poisson_glm_model(window=21) %>%
#'  dplyr::glimpse()
poisson_glm_model = function(d = i_incidence_input, ..., window = 14, frequency = "1 day", .progress=interactive()) {

  #TODO: Extract the gradient from the spline.
  env = rlang::current_env()
  if (.progress) cli::cli_progress_bar("incidence (GLM)", total = dplyr::n_groups(d), .envir = env)

  modelled = interfacer::igroup_process(d, function(d, ..., window, frequency) {

    output_times = date_seq.time_period(d$time, period = frequency)
    # We normalise the spline degrees of freedom by data length
    # to stop extra wigglyness in shorter timeseries and excess smoothing in
    # longer ts.
    df = .df_from_window(window,timeseries = d)

    model = stats::glm(count ~ splines::ns(time, df = df), family = stats::quasipoisson, data = d, singular.ok = TRUE)
    new_data = tibble::tibble(time = output_times)
    # response is transformed:
    rate_estimate = stats::predict(model, newdata = new_data, type="response")
    # this prediction in in the logit space:
    est2 = stats::predict(model, newdata = new_data, se.fit = TRUE)
    # it is possible to get CIs out here but they are the logit transformed ones.

    new_data = new_data %>%
      .result_from_fit(type = "incidence", est2$fit, est2$se.fit, model$family$linkinv) %>%
      .keep_cdf(type = "incidence", meanlog = est2$fit, sdlog = est2$se.fit) %>%
      .tidy_fit("incidence", incidence.se.fit > 4)

    if (.progress) cli::cli_progress_update(.envir = env)

    return(new_data)

  })

  if (.progress) cli::cli_progress_done()

  return(modelled %>% .normalise_from_raw(d))
}
