# GLM simple spline models ----

.df_from_window = function(window, timeseries, classes=1) {
  tmp = ceiling(nrow(timeseries) / classes / window)
  if (tmp < 1 ) tmp = 1
  return(tmp)
}



.inv_logit = function(x) exp(x)/(1+exp(x))




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
#' # TODO: find out cause of the warnings
#' # "observations with zero weight not used for calculating dispersion"
#' suppressWarnings(
#'   ggoutbreak::england_covid %>%
#'    ggoutbreak::proportion_glm_model(window=21) %>%
#'    dplyr::glimpse()
#' )
#'
proportion_glm_model = function(d = i_proportion_input, ..., window = 14, frequency = "1 day") { #, output_unit = "1 day") {

  interfacer::igroup_process(d, function(d, ..., window, frequency) {

    # see http://www.simonqueenborough.info/R/statistics/glm-binomial
    # for this next bit:
    y = cbind(d$count, d$denom - d$count)

    output_times = date_seq.time_period(d$time, period = frequency)
    df = .df_from_window(window,timeseries = d)

    if (df < 2) df=2
    model = stats::glm(y ~ splines::ns(time, df = df), family = stats::quasibinomial, data = d)
    new_data = tibble::tibble(time = output_times)
    # response is transformed:
    # proportion_estimate = stats::predict(model, newdata = new_data, type="response")
    # this prediction in in the logit space:
    est2 = stats::predict(model, newdata = new_data, se.fit = TRUE)
    # it is possible to get CIs out here but they are the logit transformed ones.
    new_data = new_data %>%
      .result_from_fit(type = "proportion", est2$fit, est2$se.fit, model$family$linkinv)
    return(interfacer::ireturn(new_data,i_proportion_model))

  })
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
#'
#' tmp = ggoutbreak::england_covid %>%
#'  ggoutbreak::poisson_glm_model(window=21) %>%
#'  dplyr::glimpse()
poisson_glm_model = function(d = i_incidence_input, ..., window = 14, frequency = "1 day") {

  # TODO:
  # Extract the gradient from the spline.

  interfacer::igroup_process(d, function(d, ..., window, deg, frequency, predict) {

    output_times = date_seq.time_period(d$time, period = frequency)
    # We normalise the spline degrees of freedom by data length
    # to stop extra wigglyness in shorter timeseries and excess smoothing in
    # longer ts.
    df = .df_from_window(window,timeseries = d)

    model = stats::glm(count ~ splines::ns(time, df = df), family = stats::quasipoisson, data = d)
    new_data = tibble::tibble(time = output_times)
    # response is transformed:
    rate_estimate = stats::predict(model, newdata = new_data, type="response")
    # this prediction in in the logit space:
    est2 = stats::predict(model, newdata = new_data, se.fit = TRUE)
    # it is possible to get CIs out here but they are the logit transformed ones.

    new_data = new_data %>%
      .result_from_fit(type = "incidence", est2$fit, est2$se.fit, model$family$linkinv) %>%
      .tidy_fit("incidence", incidence.se.fit > 4)
    return(interfacer::ireturn(new_data,i_incidence_model))

  })
}
