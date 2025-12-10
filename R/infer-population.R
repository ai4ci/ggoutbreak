#' Infers a daily baseline population for a timeseries
#'
#' This function augments any timeseries with a population denominator. The
#' population data may be static estimates, or a set of estimates at time points.
#' The population data may be grouped in which case the grouping might be geographical
#' area or age group or gender for example.
#' The two inputs must have compatible grouping (i.e. all the groups in
#' the population data must be present in the timeseries).
#'
#' @iparam pop The population data must be grouped in the same way as `df`. It
#'   might also have a `time` column as a `time_period` if the population is not
#'   static
#' @iparam df A time series, or a grouped collection of time series.
#'
#' @return the `df` timeseries with additional `population` column
#' @export
#' @concept models
#'
#' @examples
#'
#' # The COVID data already has a population column so we are just double checking:
#' data = example_england_covid_by_age() %>%
#'   dplyr::rename(pop_old = population)
#'
#' demog = ukc19::uk_population_2019_by_5yr_age %>% dplyr::group_by(code, name, class)
#'
#' data %>%
#'   infer_population(demog) %>%
#'   dplyr::glimpse()
infer_population = function(
  df = i_timeseries,
  pop = i_population_data
) {
  df = interfacer::ivalidate(df)
  if (is.numeric(pop)) {
    return(df %>% dplyr::mutate(population = pop))
  }

  pop = interfacer::ivalidate(pop)

  df = df %>% .impute_or_join(pop, population)

  return(df)
}

## Utility functions ----

# Common behaviour for functions that are given a baseline value
# that may or may not be a time series and may or may not align
# exactly with the input data. It checks the grouping and time_period
# units are the same.
.impute_or_join = function(modelled, base, baseline_col) {
  baseline_col = rlang::ensym(baseline_col)

  if (interfacer::is_col_present(modelled, !!baseline_col)) {
    return(modelled)
  }

  grps = dplyr::groups(modelled)

  shared_cols = intersect(
    colnames(base),
    c(dplyr::group_vars(modelled), "time")
  )
  base = base %>% dplyr::select(dplyr::all_of(shared_cols), !!baseline_col)

  if (!all(dplyr::group_vars(base) %in% shared_cols)) {
    message(
      "different column groupings in `modelled` and `base` parameters.\n",
      "regrouping `base` data to be compatible with `modelled` grouping"
    )
    base = base %>% dplyr::group_by(across(dplyr::all_of(shared_cols)))
  }

  if (interfacer::is_col_present(base, time)) {
    if (!.metadata_matches(base$time, modelled$time)) {
      if (.get_meta(base$time)$unit != .get_meta(modelled$time)$unit) {
        message(
          "inputs have time columns with different units and are being rescaled to a common value."
        )
      }
      base$time = .convert_units(base$time, modelled$time)
    }

    if (all(modelled$time %in% base$time)) {
      # The timeseries is aligned with the df timeseries.
      # we don't need to interpolate anything
      modelled = modelled %>%
        dplyr::left_join(
          base,
          by = c(shared_cols)
        )
    } else {
      # base is given as a timeseries we need to impute values
      adj = base %>%
        dplyr::reframe(
          impute_fn = list(.loessfn(x = time, y = !!baseline_col))
        )
      modelled = modelled %>%
        dplyr::group_modify(function(d, g, ...) {
          base_fn = adj %>%
            dplyr::semi_join(g, by = colnames(g)) %>%
            dplyr::pull(impute_fn)

          if (length(base_fn) == 0) {
            base_fn = function(x) rep(NA, length(x))
          } else {
            base_fn = base_fn[[1]]
          }

          d %>% dplyr::mutate(!!baseline_col := base_fn(time))
        })
    }
  } else {
    # base is static over time.
    if (length(dplyr::group_vars(base)) > 0) {
      modelled = modelled %>%
        dplyr::left_join(base, by = shared_cols)
    } else {
      modelled = modelled %>% dplyr::cross_join(base)
    }
  }
  return(modelled %>% dplyr::group_by(!!!grps))
}


# tmp = .loessfn(x=seq(0,5,0.01), y=seq(0,5,0.01)^2)
# tmp(xout=1:4)
.loessfn = function(x, y, window = 14) {
  tofit = dplyr::tibble(x = x, y = y)
  tmp = stats::loess(y ~ x, tofit, span = .nn_from_window(window, tofit))
  return(function(xout) {
    yout = stats::predict(tmp, dplyr::tibble(x = xout))
    return(unname(yout))
  })
}
