## Date utility ----
#
# rlang::on_load({
#   requireNamespace("lubridate",quietly = TRUE)
# })

#' Check whether vector is a date
#'
#' @param x a vector to check
#'
#' @return TRUE if dates, FALSE otherwise
#' @export
#'
#' @concept time_period
#'
#' @examples
#' is.Date(Sys.Date())
is.Date = function(x) {
  class(x) == "Date"
}

#' The minimum of a set of dates
#'
#' `min.Date` returns an integer and `Inf` for a set of `NA` dates. This
#' is usually inconvenient.
#'
#' @param x a vector of dates
#' @param ... ignored
#'
#' @return a date. `9999-12-31` if there is no well defined minimum.
#' @export
#'
#' @concept time_period
#'
#' @examples
#' min_date(NA)
min_date = function(x, ...) {
  tmp = as.Date(suppressWarnings(min(x, na.rm = TRUE)), "1970-01-01")
  if (!is.finite(tmp)) {
    tmp = as.Date("9999-12-31")
  }
  return(tmp)
}

#' The maximum of a set of dates
#'
#' `max.Date` returns an integer and `-Inf` for a set of `NA` dates. This
#' is usually inconvenient.
#'
#' @param x a vector of dates
#' @param ... ignored
#'
#' @return a date. `0001-01-01`` if there is no well defined minimum.
#' @export
#'
#' @concept time_period
#'
#' @examples
#' max_date(NA)
max_date = function(x, ...) {
  tmp = as.Date(suppressWarnings(max(x, na.rm = TRUE)), "1970-01-01")
  if (!is.finite(tmp)) {
    tmp = as.Date("0001-01-01")
  }
  return(tmp)
}


#' Format date as dmy
#'
#' @param date a date to convert
#'
#' @return the formatted date
#' @export
#'
#' @concept time_period
#'
#' @examples
#' fdmy(Sys.Date())
fdmy = function(date) {
  format(date, "%d %b %Y")
}

## Time_period ----

#' @export
as.numeric.time_period = function(x, ...) {
  class(x) = "numeric"
  attr(x, "start_date") = NULL
  attr(x, "unit") = NULL
  return(x)
}

#' Convert to a time period class
#'
#' Time periods are just a zero based numeric representation of dates with a
#' time unit baked in. This allows variable length periods (e.g. days or weeks),
#' and fractional days to be represented in a consistent(ish) way
#'
#' @param x a vector of numbers (may be integer or real) or a time_period
#' @param unit the length of one unit of time. This will be either a integer
#'   number of days, or a specification such as "1 week", or another `time_period`.
#'   If `x` is a `time_period`, and the unit is different then from that of `x`
#'   this will return a new `time_period` using the new units.
#' @param start_date the zero time date as something that can be coerced to a
#'   date. If the `x` input is already a `time_period` and this is different to its
#'   `start_date` then it will be recalibrated to use the new start date.
#' @param anchor only relevant is `x` is a vector of dates and `start_date` is
#'   not specified, this is a date, or "start" or "end" or a weekday name e.g.
#'   "mon". With the vector of dates in `x` it will find a reference date for
#'   the time-series. If this is `NULL` and `start_date` is also `NULL` it will
#'   fall back to `getOption("day_zero","2019-12-29")`
#' @param ... used for subtype implementations
#'
#' @return a `time_period` class, consisting of a vector of numbers, with
#'   attributes for time period and `start_date`
#' @export
#'
#' @concept time_period
#'
#' @example inst/examples/time-period-example.R
as.time_period = function(
  x,
  unit = NULL,
  start_date = NULL,
  anchor = NULL,
  ...
) {
  UseMethod("as.time_period")
}

#' @export
as.time_period.default = function(x, ...) {
  stop("Cannot convert a ", class(x)[[1]], " to a `time_period`")
}

# Converting a time_period to another one
#' @export
as.time_period.time_period = function(x, unit = NULL, start_date = NULL, ...) {
  # TODO: a bit of redundancy between this and .convert_units?

  times = x
  orig_unit = attributes(times)$unit
  orig_start_date = as.Date(attributes(times)$start_date)
  if (is.null(orig_unit) || is.null(orig_start_date)) {
    stop("time period has been corrupted")
  }
  if (is.null(unit)) {
    unit = orig_unit
  }
  if (is.time_period(unit)) {
    # If a second timeseries is given it is a reference
    start_date = .get_meta(unit)$start_date
    unit = .get_meta(unit)$unit
    if (is.null(unit) || is.null(start_date)) {
      stop("comparison time period has been corrupted")
    }
  } else {
    if (is.null(start_date)) start_date = orig_start_date
  }

  start_date = as.Date(start_date)
  if (orig_unit != unit || orig_start_date != start_date) {
    # time period needs conversion from one periodicity to another
    dates = time_to_date(times)
    new_times = date_to_time(dates, unit, start_date)
    names(new_times) = names(x)
    return(new_times)
  } else {
    return(times)
  }
}

.make_unit = function(unit) {
  if (lubridate::is.period(unit)) {
    return(unit)
  }
  if (is.numeric(unit) && unit >= 1) {
    return(lubridate::days(unit))
  }
  if (is.numeric(unit) && unit < 1) {
    return(lubridate::seconds(floor(unit * 24 * 60 * 60)))
  }
  return(lubridate::period(unit))
}

# conversion of date will guess units and use default start date
#' @export
as.time_period.Date = function(
  x,
  unit = NULL,
  start_date = NULL,
  anchor = NULL,
  ...
) {
  dates = x
  if (is.null(start_date)) {
    start_date = .start_from_anchor(dates, anchor)
  }
  if (is.null(unit)) {
    unit = .day_interval(dates) %>% .make_unit()
    .message_once(
      "No unit given. Guessing a sensible value from the dates gives: ",
      unit
    )
  }

  tmp = date_to_time(dates, unit = unit, start_date = start_date)
  names(tmp) = names(x)
  return(tmp)
}

# numerics need a start date and unit.
#' @export
as.time_period.numeric = function(
  x,
  unit,
  start_date = getOption("day_zero", "2019-12-29"),
  ...
) {
  if (is.time_period(unit)) {
    start_date = .get_meta(unit)$start_date
    unit = .get_meta(unit)$unit
  } else {
    start_date = as.Date(start_date)
    unit = unit %>% .make_unit()
  }
  times = as.numeric(x)

  tmp = structure(
    times,
    start_date = start_date,
    unit = unit,
    class = unique(c("time_period", class(times)))
  )
  names(tmp) = names(x)
  return(tmp)
}

#' Convert time period to dates
#'
#' @param x a time_period
#' @param ... not used
#'
#' @return a vector of dates representing the start of each of the input
#'   `time_period` entries
#'
#' @concept time_period
#'
#' @export
as.Date.time_period = function(x, ...) {
  return(time_to_date(x))
}

#' @describeIn as.Date.time_period Convert to a vector of POSIXct
#' @export
as.POSIXct.time_period = function(x, ...) {
  unit = attributes(x)$unit
  start_date = attributes(x)$start_date
  return(as.POSIXct.numeric(
    as.numeric(x) * as.numeric(unit),
    origin = start_date
  ))
}


#' Type coercion to a `time_period` class
#'
#' @param x a vector to be coerced to a time period
#'
#' @return a `time_period` or an error
#' @export
#' @keywords internal
#'
#' @examples
#' type.time_period(1:100)
type.time_period = function(x) {
  if (is.time_period(x)) {
    return(x)
  }
  unit = if (is.numeric(x)) "1 day" else NULL
  tryCatch(
    as.time_period(x, unit = unit),
    error = function(e) stop("couldn't coerce to a time_period")
  )
}

#' @describeIn as.time_period Combine `time_period`
#' @param recursive concatenate recursively
#' @export
c.time_period = function(..., recursive = F) {
  inputs = list(...)
  if (!all(sapply(inputs, function(t) is.time_period(t) || is.numeric(t)))) {
    return(unlist(lapply(inputs, as.numeric)))
  }
  inputs = inputs[sapply(inputs, is.time_period)]
  unit = lapply(inputs, function(x) attributes(x)$unit) %>% unique()
  start_date = sapply(inputs, function(x) attributes(x)$start_date) %>%
    unique() %>%
    as.Date("1970-01-01")
  if (length(unit) > 1) {
    stop("Cannot join time_periods with differing units.")
  }
  if (length(start_date) > 1) {
    stop("Cannot join time_periods with differing start dates.")
  }

  tmp = c(unlist(lapply(list(...), unclass)))
  as.time_period.numeric(tmp, unit[[1]], start_date)
}

# Subsetting functions ----

#' @describeIn as.time_period Subset a `time_period`
#' @export
`[.time_period` = function(x, ...) {
  y = `[`(as.numeric(x), ...)
  return(.clone_time_period(y, x))
}

#' @describeIn as.time_period Assign values to a subset of a `time_period`
#' @param value the value
#' @export
`[<-.time_period` = function(x, ..., value) {
  y = `[<-`(as.numeric(x), ..., as.numeric(value))
  return(.clone_time_period(y, x))
}

#' @describeIn as.time_period Get a value in a `time_period`
#' @export
`[[.time_period` = function(x, ...) {
  y = `[[`(as.numeric(x), ...)
  return(.clone_time_period(y, x))
}

#' @describeIn as.time_period Assign a value in a `time_period`
#' @param value the value
#' @export
`[[<-.time_period` = function(x, ..., value) {
  y = `[[<-`(as.numeric(x), ..., as.numeric(value))
  return(.clone_time_period(y, x))
}

#' @describeIn as.time_period Create a sequence using `time_period`s
#' @inheritParams base::seq
#' @export
seq.time_period = function(
  from,
  to = from,
  ...
) {
  y = seq.default(from = as.numeric(from), to = as.numeric(to), ...)
  return(.clone_time_period(y, from))
}

## Mathematical functions ----

#' @export
`Ops.time_period` = function(e1, e2) {
  # e2 is a numeric not an time series...
  if (is.time_period(e2)) {
    e2 = .convert_units(e2, e1)
  }
  y = get(.Generic)(as.numeric(e1), as.numeric(e2))
  if (!is.numeric(y)) {
    return(y)
  }
  return(.clone_time_period(y, e1))
}

#' @export
`Math.time_period` = function(x, ...) {
  y = get(.Generic)(as.numeric(x), ...)
  return(.clone_time_period(y, x))
}

#' @export
`Summary.time_period` = function(..., na.rm = FALSE) {
  inputs = c(...)

  # Lubridate periods cannot be made unique
  unit = attributes(inputs)$unit
  start_date = attributes(inputs)$start_date

  tmp = c(unlist(lapply(list(...), unclass)))
  y = get(.Generic)(tmp, na.rm = na.rm)
  if (!is.numeric(y)) {
    return(y)
  }
  return(as.time_period(y, unit[[1]], start_date))
}

.convert_units = function(x, original) {
  orig_unit = attributes(original)$unit
  orig_start_date = attributes(original)$start_date
  as.time_period.time_period(x, orig_unit, orig_start_date)
}

.clone_time_period = function(x, original) {
  orig_unit = attributes(original)$unit
  orig_start_date = attributes(original)$start_date
  # If for some reason this has broken fall back to a numeric.
  if (is.null(orig_unit) || is.null(orig_start_date)) {
    return(as.numeric(x))
  }
  return(structure(
    as.numeric(x),
    start_date = orig_start_date,
    unit = orig_unit,
    class = unique(c("time_period", class(original), class(x)))
  ))
}

#' @describeIn as.time_period Check is a `time_period`
#' @export
is.time_period = function(x) {
  return("time_period" %in% class(x))
}

#' @describeIn as.time_period Print a time_period
#' @export
print.time_period = function(x, ...) {
  unit = attributes(x)$unit
  start_date = attributes(x)$start_date
  cat(sprintf(
    "time unit: %s, origin: %s (a %s)\n",
    .fmt_unit(x),
    start_date,
    weekdays(start_date)
  ))
  print(as.numeric(x), ...)
}

#' Label a time period
#'
#' Create a set of labels for a time period based on the start and duration of
#' the period. The format is configurable using the start and end dates and the
#' `dfmt` and `ifmt` parameters, however if the time period has names then these
#' are used in preference.
#'
#' @param object a set of decimal times as a time_period
#' @param dfmt a `strptime` format specification for the format of the date
#' @param ifmt a glue spec referring to `start` and `end` of the period as a
#'   formatted date
#' @param na.value a label for `NA` times
#' @param ... not used
#'
#' @return a set of character labels for the time
#' @export
#'
#' @concept time_period
#'
#' @examples
#' eg = as.time_period(Sys.Date()+0:10*7, anchor="start")
#' labels(eg)
#' labels(eg, ifmt="{start}", dfmt="%d/%b/%y")
#' labels(eg, ifmt="until {end}", dfmt="%d %b %Y")
#'
#' # labels retained in constructor:
#' eg2 = Sys.Date()+0:10*7
#' names(eg2) = paste0("week ",0:10)
#' labels(eg2)
#' labels(as.time_period(eg2, anchor="start"))
labels.time_period = function(
  object,
  ...,
  dfmt = "%d/%b",
  ifmt = "{start} \u2014 {end}",
  na.value = "Unknown"
) {
  x = object
  if (!is.null(names(x))) {
    return(names(x))
  }
  return(.time_labels(x, ..., dfmt = dfmt, ifmt = ifmt, na.value = na.value))
}

.get_meta = function(x) {
  out = attributes(x)[c("unit", "start_date")]
  if (is.null(out$unit) || is.null(out$start_date)) {
    stop("missing time period metadata")
  }
  return(out)
}

.metadata_matches = function(x, y) {
  return(identical(.get_meta(x), .get_meta(y)))
}

.fmt_unit = function(x, ...) {
  tmp = if (is.time_period(x)) attributes(x)$unit else lubridate::as.period(x)
  tf = function(u, d) if (d == 1) u else sprintf("%d %ss", d, u)
  return(dplyr::case_when(
    tmp@year != 0 &&
      tmp@month == 0 &&
      tmp@day == 0 &&
      tmp@hour == 0 &&
      tmp@minute == 0 &&
      tmp@.Data == 0 ~
      tf("year", tmp@year),
    tmp@year == 0 &&
      tmp@month != 0 &&
      tmp@day == 0 &&
      tmp@hour == 0 &&
      tmp@minute == 0 &&
      tmp@.Data == 0 ~
      tf("month", tmp@month),
    tmp@year == 0 &&
      tmp@month == 0 &&
      tmp@day != 0 &&
      tmp@hour == 0 &&
      tmp@minute == 0 &&
      tmp@.Data == 0 &&
      tmp@day %% 7 == 0 ~
      tf("week", tmp@day %/% 7),
    tmp@year == 0 &&
      tmp@month == 0 &&
      tmp@day != 0 &&
      tmp@hour == 0 &&
      tmp@minute == 0 &&
      tmp@.Data == 0 &&
      tmp@day %% 7 != 0 ~
      tf("day", tmp@day),
    tmp@year == 0 &&
      tmp@month == 0 &&
      tmp@day == 0 &&
      tmp@hour != 0 &&
      tmp@minute == 0 &&
      tmp@.Data == 0 ~
      tf("hour", tmp@hour),
    TRUE ~ as.character(tmp)
  ))
}


.fmt_pop = function(x) {
  if (x >= 1000000) {
    return(sprintf("%.0fM", x / 1000000))
  }
  if (x >= 1000) {
    return(sprintf("%.0fK", x / 1000))
  }
  sprintf("%.0f", x)
}

#' Convert a set of dates to numeric timepoints
#'
#' Using a start_date and a unit specification
#'
#' @importMethodsFrom lubridate /
#' @importMethodsFrom lubridate *
#' @importMethodsFrom lubridate +
#' @importMethodsFrom lubridate -
#'
#' @param dates a vector of dates to convert
#' @param unit a specification of the unit of the resulting time series.
#'   Will be determined from periodicity of dates if not specified. If another
#'   `time_period` is given as the unit then the
#' @param start_date the origin of the conversion. Defaults to the beginning of the COVID pandemic
#'
#' @return a vector of class `time_period`
#' @export
#'
#' @concept time_period
#'
#' @examples
#' times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
#' dates = time_to_date(times)
date_to_time = function(
  dates,
  unit = .day_interval(dates),
  start_date = getOption("day_zero", "2019-12-29")
) {
  #TODO: do we want to either allow anchor as a parameter or export .start_from_anchor?

  if (is.time_period(unit)) {
    start_date = .get_meta(unit)$start_date
    unit = .get_meta(unit)$unit
  } else {
    if (!lubridate::is.period(unit)) {
      if (is.numeric(unit)) {
        unit = lubridate::period(sprintf("%1.0f days", unit))
      } else {
        unit = lubridate::period(unit)
      }
    }

    start_date = as.Date(start_date)
  }

  return(as.time_period.numeric(
    lubridate::interval(start_date, dates) / unit,
    start_date = start_date,
    unit = unit
  ))
}

#' Convert a set of time points to dates
#'
#' @param timepoints a set of numeric time points
#' @param unit the period / unit of the time points, which will be extracted from time points if possible
#' @param start_date the zero day of the time series, will be extracted from time points if possible
#'
#' @return a vector of dates
#' @export
#'
#' @concept time_period
#'
#' @examples
#' times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
#' dates = time_to_date(times)
time_to_date = function(
  timepoints,
  unit = attr(timepoints, "unit"),
  start_date = attr(timepoints, "start_date")
) {
  if (length(timepoints) == 0) {
    return(as.Date(numeric()))
  }

  if (is.null(unit)) {
    stop("Cannot determine unit from timepoints input. You must specify unit.")
  }
  if (!lubridate::is.period(unit)) {
    if (is.numeric(unit)) {
      unit = lubridate::period(unit, unit = "day")
    } else {
      unit = lubridate::period(unit)
    }
  }
  start_date = as.Date(start_date)

  timepoints = as.numeric(timepoints)
  if (all(timepoints == floor(timepoints), na.rm = TRUE)) {
    return(start_date + unit * as.numeric(timepoints))
  }

  # Interpolate fractional time periods.
  x = sort(unique(c(floor(timepoints), ceiling(timepoints))))
  y = start_date + unit * as.numeric(x)
  # as.Date(floor(stats::approx(x,y,xout=timepoints)$y),"1970-01-01")
  as.Date(stats::approx(x, y, xout = timepoints)$y, "1970-01-01")
}

# Suppose the metadata has been stripped off a time_period. Here we can reconstruct it
# from a numeric vector of times and a date vector.
.infer_units = function(times, dates) {
  if (is.time_period(times)) {
    return(times)
  }
  dates = unique(sort(dates))
  times = unique(sort(times))
  interval = as.numeric(stats::na.omit(dates - dplyr::lag(dates))) /
    as.numeric(stats::na.omit(times - dplyr::lag(times)))
  if (all(interval %% 7 == 0)) {
    n = .gcd(interval %/% 7)
    unit = lubridate::as.period(n, unit = "week")
  } else if (round(min(interval, na.rm = TRUE)) >= 365) {
    n = .gcd(interval %/% 365)
    unit = lubridate::as.period(n, unit = "year")
  } else if (round(min(interval, na.rm = TRUE)) >= 28) {
    n = .gcd(round(interval / 30))
    unit = lubridate::as.period(sprintf("%d month", n))
  } else {
    n = .gcd(interval)
    unit = lubridate::as.period(n, unit = "day")
  }

  ends = dates - floor(times) * unit
  starts = dates - ceiling(times) * unit
  possible_start_dates = stats::na.omit(
    starts + round(as.numeric(ends - starts) * (1 + floor(times) - times))
  )

  if (length(unique(possible_start_dates)) != 1) {
    stop(
      "Could not infer start date from dates and times, having units of: ",
      unit,
      ", possibilities are ",
      paste0(sort(unique(possible_start_dates)), collapse = ", ")
    )
  }
  return(as.time_period.numeric(
    times,
    start_date = unique(possible_start_dates),
    unit = unit
  ))
}


## Date cutting functions ----

# create a new time_period on the same scale as the old one but
# as an ordered sequence of one day intervals. this is used during conversions
# of irregular line lists to ordered time series, but mostly as an output for
# incidence / growth rate estimates.
.daily_times = function(times) {
  start_time = min(times, na.rm = TRUE)
  end_time = max(times, na.rm = TRUE)
  start_date = attributes(times)$start_date
  orig_unit = attributes(times)$unit
  dates = as.Date(
    seq(
      time_to_date(start_time, orig_unit, start_date),
      time_to_date(end_time, orig_unit, start_date),
      by = 1
    ),
    "1970-01-01"
  )

  as.time_period(
    dates,
    orig_unit,
    start_date
  )
}


#' Create the full sequence of values in a vector
#'
#' This is useful if you want to fill in missing values that should have been observed but weren't. For example, date_seq(c(1, 2, 4, 6), 1) will return 1:6.
#'
#' @param x a numeric or date vector
#' @param period Gap between each observation. The existing data will be checked to ensure that it is actually of this periodicity.
#' @param ... for subtype methods
#'
#' @return a vector of the same type as the input
#' @export
#'
#' @concept time_period
#'
#' @examples
#' date_seq(c(1, 2, 4, 5, 10), 1)
date_seq = function(x, period, ...) {
  UseMethod("date_seq")
}


#' @inherit date_seq
#' @param tol Numerical tolerance for checking periodicity.
#' @export
#' @concept time_period
date_seq.numeric = function(x, period = 1, tol = 1e-06, ...) {
  #dplyr::check_number_decimal(period)
  #dplyr::check_number_decimal(tol, min = 0)
  if (length(unique(x)) < 2) {
    return(x)
  }
  rng <- range(x, na.rm = TRUE)
  if (
    any(
      ((x - rng[1]) %% period > tol) & (period - (x - rng[1]) %% period > tol),
      na.rm = TRUE
    )
  ) {
    stop("x is not a regular sequence.")
  }
  if (period - ((rng[2] - rng[1]) %% period) <= tol) {
    rng[2] <- rng[2] + tol
  }
  seq(rng[1], rng[2], by = period)
}

#' Expand a date vector to the full range of possible dates
#'
#' Derive from a vector of observation dates, a complete ordered sequence of
#' periods in a regular time series, where the length of the periods is
#' specified, as a number of days, weeks, years etcetera. E.g. this can convert a
#' random set of dates to a ordered complete list of 1 week intervals (or 2
#' month intervals) spanning the same range as the dates. This has some
#' interesting problems regarding where to put breaks within a month or week.
#' Often this is either based on a specific date (e.g. yearly periods starting
#' at `2020-01-01`) or a day of week (e.g. 2 weekly periods staring on a Sunday)
#' or maybe relative to the input time series (weekly ending on the last date of
#' the data). There is also a problem when we consider data that may have
#' incomplete starting and end periods, which may not be comparable to other
#' periods, and we may need to exclude these from the result.
#'
#' @param x a vector of dates, possibly including NA values
#' @param period the gap between observations as a number of days or as a natural
#'   language definition of the period such as "1 week", '2 weeks', '1 month', etcetera.
#'   If not given this will be derived from the dates.
#' @param anchor defines a day that appears in the sequence (if it were to
#'   extend that far). Given either as a date, or "start", "end" or a day of the
#'   week, e.g. "mon".
#' @param complete truncate incomplete start and end periods
#' @param ... ignored
#'
#' @return a vector of dates for regular periods between the minimum and maximum of
#'   dates, with the boundaries defined by the anchor.
#' @export
#'
#' @concept time_period
#'
#' @examples
#' date_seq(as.Date(c("2020-01-01","2020-02-01","2020-01-15","2020-02-01",NA)), "2 days")
date_seq.Date = function(
  x,
  period = .day_interval(x),
  anchor = "start",
  complete = FALSE,
  ...
) {
  dates = x
  if (all(is.na(x))) {
    browser()
    stop("No non-NA dates provided to date_seq")
  }
  start_date = .start_from_anchor(dates, anchor)
  period = .make_unit(period)
  start_date = as.Date(start_date)
  dates = trunc(as.Date(dates))
  start = min_date(dates)
  end = max_date(dates)
  outer_start_time = floor(lubridate::interval(start_date, start + 1) / period)
  outer_end_time = ceiling(lubridate::interval(start_date, end - 1) / period)
  out = start_date + period * (outer_start_time:outer_end_time)
  # ends = start_date + period * (outer_start_time:outer_end_time+1) - 1
  if (complete) {
    out = out[out >= start & out <= end]
  }
  # seq_time = (outer_start_time:outer_end_time)
  # return(structure(out, end = ends))
  return(out)
}


#' Expand a `time_period` vector to the full range of possible times
#'
#' Derive from a vector of observation `time_periods`, a complete ordered sequence of
#' periods in a regular time series, where the length of the periods is
#' specified, as a number of days, weeks, years etc. E.g. this can convert a
#' random set of times to a ordered complete list of 1 week intervals (or 2
#' month intervals) spanning the same range as the dates. This has some
#' interesting problems regarding where to put breaks within a month or week.
#' Often this is either based on a specific date (e.g. yearly periods starting
#' at `2020-01-01`) or a day of week (e.g. 2 weekly periods staring on a sunday)
#' or maybe relative to the input time series (weekly ending on the last date of
#' the data). There is also a problem when we consider data that may have
#' incomplete starting and end periods, which may not be comparable to other
#' periods, and we may need to exclude these from the result.
#'
#' @param x a time period vector
#' @param period the gap between observations as a number of days or as a natural
#'   language definition of the period such as "1 week", '2 weeks', '1 month', etc.
#'   If not given this will be derived from the dates.
#' @param complete truncate incomplete start and end periods
#' @param ... ignored
#'
#' @return a vector of `time_periods` for regular periods between the minimum and maximum of
#'   dates, with the boundaries defined by the anchor.
#' @export
#'
#' @concept time_period
#'
#' @examples
#' tmp = as.time_period(c(0,10,100), 7, "2020-01-01")
#' date_seq(tmp, "7 days")
#' date_seq(tmp, "1 day")
date_seq.time_period = function(
  x,
  period = attributes(x)$unit,
  complete = FALSE,
  ...
) {
  if (all(is.na(x))) {
    stop("No non-NA times provided to date_seq")
  }

  period = .make_unit(period)
  if (period == attributes(x)$unit) {
    # no change of unit.

    new_times = date_seq.numeric(as.numeric(x), .step(as.numeric(x)))
    return(.clone_time_period(new_times, x))
  }

  times = x
  start_date = attributes(times)$start_date
  orig_unit = attributes(times)$unit
  dates = date_seq.Date(
    time_to_date(range(x, na.rm = TRUE)),
    period = period,
    anchor = start_date,
    complete = complete,
    ...
  )
  date_to_time(dates, unit = orig_unit, start_date = start_date)
}


# guess the intervals between dates in a vector
.day_interval = function(dates) {
  dates = sort(unique(dates))
  if (length(dates) < 4) {
    return(1)
  }
  interval = .gcd(stats::na.omit(as.numeric(abs(dates - dplyr::lag(dates)))))
  return(interval)
}

# greatest common denominator
.gcd2 = function(a, b) {
  if (b == 0) a else Recall(b, a %% b)
}

.gcd <- function(...) {
  Reduce(.gcd2, c(...))
}

.step = function(x) {
  y = sort(unique(x))
  dy = stats::na.omit(y[-1] - utils::head(y, -1))
  return(.gcd(dy))
}

# lubridate period round trip to string
# .period_to_string = function(p) {
#   if (!lubridate::is.period(p)) {
#     if (is.numeric(p)) p = lubridate::as.period(p, unit="day")
#     else p = lubridate::as.period(p)
#   }
#   return(as.character(p))
# }

#' Places a set of dates within a regular time series
#'
#' The counterpart to date_seq_dates(). Take an original set of data and place
#' it within a regular time series where the periodicity of the time series may
#' be expressed as numbers of days, weeks, months quarters, or years, and the
#' periods are defined by an anchoring date, day of the week or by reference to
#' the start or end of the input dates. This can either return the periods as
#' dates or factors (e.g. for plotting) or as a `time_period` for analysis that
#' relies on a numeric representation of the date or duration from the anchor.
#'
#' @param dates a set of dates
#' @param unit a period e.g. "1 week"
#' @param anchor one of a date, "start" or "end" or a weekday name e.g. "mon"
#'   this will always be one of the start of the time periods we are cutting
#'   into
#' @param output return the result as either a "date" (the default), an ordered
#'   "factor" with the date ranges as a label, or as a "time_period". The result is
#'   named with labels referring to the
#' @param dfmt the `strptime` format for the dates in the labels
#' @param ifmt a `sprintf` format for the period label containing `%s` exactly twice.
#' @param ... ignored
#'
#' @return a set of dates, times or a factor level, representing the start of
#'   the period the date falls into, where the period is defined by the duration
#'   and the anchor
#' @export
#'
#' @concept time_period
#'
#' @examples
#' dates = as.Date(c("2020-01-01","2020-02-01","2020-01-15","2020-02-03",NA))
#' fs = ggoutbreak::date_seq(dates, "2 days")
#' dates - cut_date(dates, "2 days")
#' cut_date(dates,unit="2 days", output="time_period")
#'
#' # A weekly set of dates:
#' dates2 = Sys.Date() + floor(stats::runif(50,max=10))*7
#'
#' # in this specific situation the final date is not truncated because the
#' # input data is seen as an exact match for the whole output period.
#' cut_date(dates2, "1 week", "sun", output="factor")
#' cut_date(dates2, dfmt = "%d/%b", output="factor", unit = "2 weeks", anchor="sun")
#'
cut_date = function(
  dates,
  unit,
  anchor = "start",
  output = c("date", "factor", "time_period"),
  dfmt = "%d/%b/%y",
  ifmt = "{start} \u2014 {end}",
  ...
) {
  output = match.arg(output)
  start_date = .start_from_anchor(dates, anchor)

  times = floor(date_to_time(dates, unit, start_date))
  out = .labelled_date_from_times(times, dfmt, ifmt)

  if (output == "date") {
    return(out)
  }

  if (output == "time_period") {
    names(times) = names(out)
    return(times)
  }

  # TODO: cutting dates in this way really gives you a time_period but with
  # factor rather than integer / numeric. to decide whether this is worth pursuing
  if (output == "factor") {
    # create a whole full time sequence from dates plus one period
    complete_dates = date_seq.Date(dates, unit, start_date)
    complete_times = date_to_time(complete_dates, unit, start_date)
    # create a label from that sequence and use it as levels for names
    labelled_dates = .labelled_date_from_times(complete_times, dfmt, ifmt)
    if (anyDuplicated(names(labelled_dates))) {
      stop(
        "The formatting of the labels (`dfmt` and `ifmt`) does not result in unique factor levels."
      )
    }
    out = factor(names(out), levels = names(labelled_dates), ordered = TRUE)
    return(out)
  }

  stop("output format not known")
}

.start_from_anchor = function(dates, anchor) {
  default_set = !is.null(getOption("day_zero"))
  default_start_date = as.Date(getOption("day_zero", "2019-12-29"))
  if (is.null(anchor)) {
    if (!default_set) {
      .message_once(
        "No `start_date` (or `anchor`) specified. Using default (N.b. set `options('day_zero'=XXX)` to change): ",
        default_start_date
      )
    }
    return(default_start_date)
  }
  start_date = try(as.Date(anchor), silent = TRUE)
  if (!is.Date(start_date)) {
    anchor = anchor %>% tolower() %>% substr(1, 3)
    start_date = if (anchor == "sta") {
      min_date(dates)
    } else if (anchor == "end") {
      max_date(dates) + 1
    } else {
      min_date(dates) -
        7 +
        which(
          substr(tolower(weekdays(min_date(dates) - 6 + 0:6)), 1, 3) == anchor
        )
    }
    if (length(start_date) != 1) {
      if (!default_set) {
        .message_once(
          "`anchor` was not valid (a date or one of 'start', 'end', or a weekday name). Using default (N.b. set `options('day_zero'=XXX)` to change): ",
          default_start_date
        )
      }
      return(default_start_date)
    }
  }
  return(start_date)
}

.labelled_date_from_times = function(
  times,
  dfmt = "%d/%b",
  ifmt = "{start} \u2014 {end}",
  na.value = "Unknown"
) {
  out = time_to_date(times)
  names(out) = .time_labels(
    times,
    dfmt = dfmt,
    ifmt = ifmt,
    na.value = na.value
  )
  return(out)
}

# get a set of labels based on the values of a time_period.
#
.time_labels = function(
  times,
  dfmt = "%d/%b",
  ifmt = "{start} \u2014 {end}",
  na.value = "Unknown"
) {
  if (any(stats::na.omit(abs(floor(times) - times)) > 0.01)) {
    diff = 0
    .message_once("labelling applied to non-integer times.")
  } else {
    diff = 1
  }

  start_dates = time_to_date(times)
  end_dates = start_dates + attributes(times)$unit - lubridate::days(diff)

  start_label = format(start_dates, format = dfmt)
  end_label = format(end_dates, format = dfmt)

  if (!all(start_label == end_label)) {
    label = glue::glue_data(list(start = start_label, end = end_label), ifmt)
  } else {
    label = start_label
  }

  label[is.na(times)] = na.value
  return(label)
}

# Base data functions ----

#' @export
#' @inherit base::weekdays
#' @concept time_period
weekdays.time_period = function(x, abbreviate = FALSE) {
  return(weekdays(as.Date(x), abbreviate = abbreviate))
}

#' @inherit base::months
#' @export
#' @concept time_period
months.time_period = function(x, abbreviate = FALSE) {
  return(months(as.Date(x), abbreviate = abbreviate))
}

#' @inherit base::quarters
#' @export
#' @concept time_period
quarters.time_period = function(x, ...) {
  return(quarters(as.Date(x), ...))
}

#' @inherit base::julian
#' @export
#' @concept time_period
julian.time_period = function(x, ...) {
  return(julian(as.Date(x), ...))
}

#TODO: interpolate_time_periods
#TODO: linelist_to_timeseries
#TODO: aggregate_timeseries from timeseries
#TODO: decompose_timeseries
#TODO: timeseries missing values / ragged ends
#TODO: timeseries anomaly detection
