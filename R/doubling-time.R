#' Doubling time from growth rate
#'
#' The unit of doubling times is always days.
#'
#' @iparam x proportion or incidence growth rates
#' @param ... not used
#'
#' @return the same dataframe with additional columns for doubling time
#'   or relative doubling time plus confidence intervals.
#' @export
#' @concept models
#' @examples
#'
#' example_poisson_rt_smooth() %>%
#'   poisson_locfit_model(window=21) %>%
#'   doubling_time() %>%
#'   dplyr::glimpse()
#'
#'
doubling_time = function(x, ...) {
  interfacer::idispatch(
    x,
    doubling_time.incidence = i_incidence_rate,
    doubling_time.proportion = i_proportion_rate
  )
}

# internal function for dispatch
doubling_time.incidence = function(x, ...) {
  time_unit = 1 / .step(x$time)
  x %>%
    dplyr::mutate(
      doubling_time.0.5 = time_unit * log(2) / growth.0.5,
      doubling_time.0.025 = time_unit * log(2) / growth.0.975,
      doubling_time.0.975 = time_unit * log(2) / growth.0.025
    )
}

# internal function for dispatch
doubling_time.proportion = function(x, ...) {
  time_unit = 1 / .step(x$time)
  x %>%
    dplyr::mutate(
      relative.doubling_time.0.5 = time_unit * log(2) / relative.growth.0.5,
      relative.doubling_time.0.025 = time_unit * log(2) / relative.growth.0.975,
      relative.doubling_time.0.975 = time_unit * log(2) / relative.growth.0.025
    )
}
