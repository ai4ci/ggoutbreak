#' Summarise data from a line list to a time-series of counts.
#'
#' This principally is designed to take a record of single events and produce a
#' summary time-series count of events by group, class and date. The default
#' behaviour is to guess the cadence of the input data and
#' summarise the event line list to a (set of) regular time-series counts for use
#' in incidence and growth rate estimates.
#'
#' If the data is given with a `class` column the time series are interpreted as
#' having a denominator, consisting of all the different classes within a time period.
#' This may be subtypes (e.g. variants, serotypes) or markers for test positivity.
#' In either case the resulting time series will have counts for all classes and
#' denominators for the combination.
#'
#' There is flexibility for other kinds of summarisation if the raw data is not
#' count based (e.g. means of continuous variables) but in this case a the `slider`
#' package is usually going to be better, as time summarise will only look at non
#' overlapping time periods with fixed lengths.
#'
#' There is another use case where an existing  timeseries on a particular
#' frequency is aggregated to another less frequent basis (e.g. moving from a
#' daily timeseries to a weekly one). In this case the input will contain a
#' `count` column. In this mode no checks are made that the more frequent events
#' are all present before summarisation so the result may include different numbers
#' of input periods (e.g. going from weeks to months may be 4 or 5 weeks in each
#' month)
#'
#' @param df a line list of data you want to summarise, optionally grouped.
#'   If this is grouped then each group is treated independently. The remaining
#'   columns must contain a `date` column and may contain a `class` column. If a
#'   `count` column is present the counts will be summed, otherwise each
#'   individual row will be counted as a single event (as a linelist)
#' @inheritParams cut_date
#' @param rectangular should the resulting time series be the same length for
#'   all groups. This is only the case if you can be sure that your data is complete
#'   for all subgroups, otherwise missing data will be treated as zero counts. This
#'   is important if leading and trailing missing data in one subgroup can be due
#'   to a reporting delay in that subgroup, in which case a rectangular time series
#'   will erroneously fill in zero counts for this missing data.
#' @param ... a spec for a dplyr::summary(...) - optional, and if not provided a
#'   `count = dplyr::n()` or a `count = sum(count)` is performed.
#' @param .fill a list similar to tidyr::complete for values to fill
#'   variables with
#'
#' @concept time_period
#'
#' @return The output depends on whether or not the input was grouped and
#' had a `class` column. The most detailed output will be:
#'
#' `r i_proportion_data`
#'
#' or a more minimal output if the input is only a plain list of dated events:
#'
#' `r i_incidence_data`
#'
#' @export
time_summarise = function(df = i_dated, unit, anchor = "start", rectangular = FALSE, ..., .fill = list(count = 0)) {

  df = interfacer::ivalidate(df)

  # TODO: a line list with no data at the end is either missing data or there were no observations.
  # A date at which the observations are complete up to may be needed for each
  # group.

  if (.has_cols(df,"time")) warning("time_summarise ignores existing time_period columns")

  has_class = .has_cols(df,"class")
  grps = df %>% dplyr::groups()

  start_date = .start_from_anchor(df$date, anchor)

  df = df %>% dplyr::mutate(time = cut_date(date, unit=unit, anchor = start_date, output="time_period"))
  if (has_class) df = df %>% dplyr::group_by(class, .add = TRUE)
  df = df %>% dplyr::group_by(time, .add = TRUE)

  dots = dplyr::enexprs(...)

  if (length(dots)==0) {
    if (.has_cols(df,"count")) {
      dots=list(count=rlang::expr(sum(count, na.rm=TRUE)))
    } else {
      dots=list(count=rlang::expr(dplyr::n()))
    }
    .fill = list(count=0)
  }

  # when rectangular the full sequence is for all the obseverations
  times = date_seq.time_period(df$time)

  # df grouped by grps, class(?), and time at this point
  tmp = df %>%
    dplyr::summarise(!!!dots, .groups="drop_last") %>%
    dplyr::group_by(!!!grps) %>%
    dplyr::group_modify(function(d,g,...) {
      if (!rectangular) times = date_seq.time_period(d$time)
      if (has_class) {
        dtmp = d %>% tidyr::complete(class, time = times, fill = .fill)
        if (!.has_cols(d, "denom") & .has_cols(d,"count")) {
          # calculate a classwise denominator if class and count are present and no denom column
          # created by the summarise.
          dtmp = dtmp %>%
            dplyr::group_by(time) %>%
            dplyr::mutate(denom = sum(count, na.rm = TRUE)) %>%
            dplyr::ungroup()
        }
      } else {
        dtmp = d %>% tidyr::complete(time = times, fill = .fill)
      }

    })

  if (has_class) {
    tmp = tmp %>% dplyr::group_by(!!!grps, class)
  } else {
    tmp = tmp %>% dplyr::group_by(!!!grps)
  }

  return(interfacer::ireturn(tmp, i_incidence_data))
}



#' Aggregate time series data preserving the time series
#'
#' @param df an optionally grouped time series. Grouping should not include the time
#'   column. The grouping works differently from `dplyr::summarise` in that the last level
#'   of non-time groups is lost in this operation, so the subgroup you wish to aggregate
#'   should be included in the grouping.
#' @param ... A set of `dplyr::summarise` statements, or additional parameters
#'   for `.fns`
#' @param .cols Optional tidyselect column specification for `dplyr::across`. if
#'   `.fns` is given and the `.cols` parameter is not specified then the columns
#'   to summarise are automatically identified. In doing this any `Date` columns
#'   are dropped. If this in not what you want then `.cols` or `...` must be given
#' @param .fns Optional a set of function specifications as per `dplyr::across`
#' @param .groups as per `dplyr::summarise`
#'
#' @return the summarised time series preserving the `time` column, and with the grouping
#'   structure involving one fewer levels that the input
#' @export
#' @concept time_period
#'
#' @examples
#' ggoutbreak::england_covid %>%
#'   time_aggregate(count = sum(count), denom = sum(denom)) %>%
#'   dplyr::glimpse()
#'
#' ggoutbreak::england_covid %>%
#'   time_aggregate(.fns=mean) %>%
#'   dplyr::glimpse()
time_aggregate = function(df = i_timestamped, ..., .groups = NULL, .cols = NULL, .fns = NULL) {
  .cols = rlang::enexpr(.cols)
  df = interfacer::ivalidate(df)
  grps = df %>% dplyr::group_vars()
  # TODO: automatically deal with class column?
  agg_grps = grps[grps != "time"] %>% utils::head(-1) %>% c("time") %>% unique() %>% lapply(as.symbol)
  df = df %>% dplyr::group_by(!!!agg_grps)


  if (!is.null(.fns)) {
    if (is.null(.cols)) {
      .cols = setdiff(colnames(df),c(grps,"time"))
      is_date = sapply(.cols, function(x) is.Date(df[[x]]))
      .cols = .cols[!is_date]
      out = df %>% dplyr::summarise(dplyr::across(.cols=dplyr::all_of(.cols), .fns = .fns, ..., ), .groups=.groups)
    } else {
      out = df %>% dplyr::summarise(dplyr::across(.cols=!!.cols, .fns = .fns, ...), .groups=.groups)
    }
  } else {
    dots = rlang::enexprs(...)
    if (length(dots) == 0) {
      dots = list(count = rlang::expr(sum(count)))
      if (.has_cols(df, "denom")) {
        dots = c(dots,list(denom = rlang::expr(sum(denom))))
      }
      if (.has_cols(df, "population")) {
        dots = c(dots,list(population = rlang::expr(sum(population))))
      }
    }
    out = df %>% dplyr::summarise(!!!dots, .groups=.groups)
  }

  return(out)
}
