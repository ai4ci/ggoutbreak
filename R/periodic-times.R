## VCTRS reimplementation ----

#' S3 time period class
#'
#' Time periods are just a zero based numeric representation of dates with a
#' time unit baked in. This allows variable length periods (e.g. days or weeks),
#' and fractional days to be represented in a consistent(ish) way
#'
#' @inheritParams as.time_period
#'
#' @return a `time_period` class, consisting of a vector of numbers, with
#'   attributes for time period and `start_date`
#'
#' @import vctrs
#' @concept time_period
#'
#' @param x description
#' @keywords internal
#' @name S3_time_period
#' @unit
#' default = new_time_period(as.numeric(1:10))
#' shifted = new_time_period(as.numeric(1:10), start_date = as.Date("1970-01-10"))
#' stretched = new_time_period(as.numeric(1:10), unit = lubridate::weeks(1))
#'
#' testthat::expect_equal(
#'   format(default),
#'   c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
#' )
#'
#' testthat::expect_equal(
#'   vec_ptype_full(default),
#'   "time unit: day, origin: 1970-01-01 (a Thursday)"
#' )
#'
#' testthat::expect_equal(
#'   vec_ptype_full(shifted),
#'   "time unit: day, origin: 1970-01-10 (a Saturday)"
#' )
#'
#' testthat::expect_equal(
#'   vec_ptype_full(stretched),
#'   "time unit: week, origin: 1970-01-01 (a Thursday)"
#' )
#'
#' testthat::expect_equal(vec_ptype_abbr(default), "t[day]")
#'
#' dshifted = vec_cast(default, shifted)
#' testthat::expect_equal(
#'   as.Date(dshifted) == as.Date(default),
#'   c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
#' )
#'
#' dsquashed = vec_cast(default, stretched)
#' testthat::expect_equal(vec_cast(dsquashed, double()), c(
#'   0.142857142857143,
#'   0.285714285714286,
#'   0.428571428571429,
#'   0.571428571428571,
#'   0.714285714285714,
#'   0.857142857142857,
#'   1,
#'   1.14285714285714,
#'   1.28571428571429,
#'   1.42857142857143
#' ))
#'
#' testthat::expect_equal(
#'   diff(vec_cast(stretched, Sys.Date())),
#'   structure(
#'     c(7, 7, 7, 7, 7, 7, 7, 7, 7),
#'     class = "difftime",
#'     units = "days"
#'   )
#' )
#'
#' # Cast numeric to time period check equality
#' testthat::expect_equal(all(c(shifted, vec_cast(11:20, shifted)) == 1:20), TRUE)
#'
#' # Different cadence can be matched:
#' testthat::expect_equal(
#'   vec_match(default, stretched),
#'   c(NA, NA, NA, NA, NA, NA, 1L, NA, NA, NA)
#' )
#'
#' # Matching is within tolerance
#' testthat::expect_equal(all(default == default + 0.0000001), TRUE)
#'
#' # comparison
#' testthat::expect_equal(
#'   shifted > rev(shifted),
#'   c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
#' )
#'
#' # sorting
#' testthat::expect_equal(
#'   as.numeric(sort(sample(shifted, 10))),
#'   c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' )
#'
#' # Arithmetic:
#' tmp = withr::with_seed(100, default + runif(10))
#' testthat::expect_equal(any(tmp == default), FALSE)
#' testthat::expect_equal(all(floor(tmp) == default), TRUE)
#'
#' # Basic operation returns the time_period
#' testthat::expect_equal(is.time_period(default - 1), TRUE)
#'
#' # modulo works:
#' testthat::expect_equal(default %% 3, c(1, 2, 0, 1, 2, 0, 1, 2, 0, 1))
#' # Other operations will work but the result has to be interpreted relative to origin
#' testthat::expect_equal(sin(default * pi / 2), c(
#'   1, 0, -1, 0, 1, 0, -1, 0, 1, 0
#' ))
#'
#'
#'
NULL


#' @describeIn S3_time_period Construct a new time period
#' @export
new_time_period = function(
  x = double(),
  start_date = as.Date(0),
  unit = lubridate::days(1)
) {
  if (!rlang::is_double(x)) {
    rlang::abort("`x` must be a double vector.")
  }
  if (!is.Date(start_date) || length(start_date) != 1) {
    rlang::abort("`start_date` must be a single date.")
  }
  if (!lubridate::is.period(unit) || length(unit) != 1) {
    rlang::abort("`unit` must be a single lubridate `period`.")
  }
  # make sure that start date's underlying class is a double and not an int.
  start_date = as.Date(as.numeric(start_date))
  vctrs::new_vctr(
    x,
    start_date = start_date,
    unit = unit,
    class = "time_period"
  )
}

.proto.time_period = function(original) {
  new_time_period(
    double(),
    start_date = attr(original, "start_date"),
    unit = attr(original, "unit")
  )
}

#' @export
format.time_period = function(x, ...) {
  return(formatC(round(vctrs::vec_data(x), digits = 2)))
}

#' @export
print.time_period = function(x, ...) {
  unit = attributes(x)$unit
  start_date = attributes(x)$start_date
  cat(vec_ptype_full.time_period(x))
  cat("\n")
  cat(format.time_period(x))
}

#' @export
vec_ptype_full.time_period = function(x, ...) {
  unit = attributes(x)$unit
  start_date = attributes(x)$start_date
  sprintf(
    "time unit: %s, origin: %s (a %s)",
    .fmt_unit(x),
    start_date,
    weekdays(start_date)
  )
}

#' @export
vec_ptype_abbr.time_period = function(x, ...) {
  sprintf("t[%s]", .fmt_unit(.get_meta(x)$unit))
}

### Casting and coercion ----

#' @export
vec_ptype2.time_period.time_period <- function(x, y, ...) .proto.time_period(x)

#' @export
vec_ptype2.time_period.double <- function(x, y, ...) .proto.time_period(x)

#' @export
vec_ptype2.double.time_period <- function(x, y, ...) double()

#' @export
vec_ptype2.time_period.integer <- function(x, y, ...) .proto.time_period(x)

#' @export
vec_ptype2.integer.time_period <- function(x, y, ...) integer()

#' @export
vec_ptype2.time_period.Date <- function(x, y, ...) .proto.time_period(x)

#' @export
vec_ptype2.Date.time_period <- function(x, y, ...) as.Date(NULL)

#' @export
vec_ptype2.POSIXct.time_period <- function(x, y, ...) as.POSIXct(NULL)

#' @export
vec_ptype2.character.time_period <- function(x, y, ...) character()


#' @export
vec_cast.time_period.time_period <- function(x, to, ...) {
  orig_unit = attributes(x)$unit
  orig_start_date = as.Date(attributes(x)$start_date)
  unit = attributes(to)$unit
  start_date = as.Date(attributes(to)$start_date)

  if (orig_unit != unit) {
    # time period needs conversion from one periodicity to another
    dates = time_to_date(x)
    new_times = date_to_time(dates, unit, start_date)
    names(new_times) = names(x)
    return(new_times)
  } else if (orig_start_date != start_date) {
    diff = as.numeric(as.time_period(orig_start_date, to))
    return(new_time_period(
      vec_data(x) + diff,
      start_date = start_date,
      unit = unit
    ))
  } else {
    return(x)
  }
}

# To time period

#' @export
vec_cast.time_period.double <- function(x, to, ...) {
  unit = attributes(to)$unit
  start_date = as.Date(attributes(to)$start_date)
  new_time_period(x, start_date, unit)
}

#' @export
vec_cast.time_period.integer <- function(x, to, ...) {
  unit = attributes(to)$unit
  start_date = as.Date(attributes(to)$start_date)
  new_time_period(as.numeric(x), start_date, unit)
}

#' @export
vec_cast.time_period.Date <- function(x, to, ...) {
  unit = attributes(to)$unit
  start_date = as.Date(attributes(to)$start_date)
  date_to_time(x, unit = unit, start_date = start_date)
}


# From time period

#' @export
vec_cast.double.time_period <- function(x, to, ...) {
  vctrs::vec_data(x)
}

#' @export
vec_cast.integer.time_period <- function(x, to, ...) {
  as.integer(round(vctrs::vec_data(x)))
}

#' @export
vec_cast.Date.time_period <- function(x, to, ...) {
  time_to_date(x)
}

#' @export
vec_cast.POSIXct.time_period = function(x, to, ...) {
  unit = attributes(x)$unit
  start_date = attributes(x)$start_date
  return(as.POSIXct.numeric(
    as.numeric(x) * as.numeric(unit),
    origin = start_date
  ))
}

#' @export
vec_cast.character.time_period <- function(x, to, ...) {
  return(labels(x, ...))
}

### Equality and comparison ----

#' @export
vec_proxy_equal.time_period = function(x, ...) {
  round(vec_data(x), digits = 5)
}

#' @export
vec_proxy_compare.time_period = function(x, ...) {
  round(vec_data(x), digits = 5)
}

### Arithmetic ----

#' @export
#' @method vec_arith time_period
vec_arith.time_period <- function(op, x, y, ...) {
  UseMethod("vec_arith.time_period", y)
}

#' @export
#' @method vec_arith.time_period time_period
vec_arith.time_period.time_period <- function(op, x, y, ...) {
  y = vec_cast(y, x)
  switch(
    op,
    "-" = vctrs::vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.time_period double
vec_arith.time_period.double <- function(op, x, y, ...) {
  unit = attributes(x)$unit
  start_date = as.Date(attributes(x)$start_date)
  switch(
    op,
    "+" = ,
    "-" = new_time_period(
      vctrs::vec_arith_base(op, x, y),
      start_date = start_date,
      unit = unit
    ),
    vctrs::vec_arith_base(op, x, y)
  )
}

#' @export
#' @method vec_arith.time_period integer
vec_arith.time_period.integer <- function(op, x, y, ...) {
  vec_arith.time_period.double(op, x, as.numeric(y))
}

#' @export
#' @method vec_math time_period
vec_math.time_period <- function(.fn, x, ...) {
  unit = attributes(x)$unit
  start_date = as.Date(attributes(x)$start_date)
  switch(
    .fn,
    "floor" = ,
    "trunc" = ,
    "ceiling" = ,
    "cummax" = ,
    "cummin" = ,
    "round" = new_time_period(
      vctrs::vec_math_base(.fn, x),
      start_date = start_date,
      unit = unit
    ),
    vctrs::vec_math_base(.fn, x)
    # cli::cli_abort("function not supported: {fn}() for {.cls {class(x)}}")
  )
}


## Defaults & cache ----

.time_period_defaults = new.env(parent = emptyenv())

#' Set or reset the default origin and unit for time periods
#'
#' This function is generally not needed, and will be called automatically
#' when the first date conversion is performed. If no other information is
#' given then the default origin will be decided by the start of the first use of the
#' `time_period` class in a session. This helps to keep defaults consistent
#' during a single run, if they are not specified.
#'
#' @inheritParams as.time_period
#' @param date A date, or something that can be cast to one, that represents
#'   day zero for an outbreak.
#'
#' @returns depending on the methods the original default start date / the
#'   original default unit, a list of both or the result of evaluating the
#'   expression `expr`
#' @concept time_period
#' @export
#'
#' @examples
#' # set default origin and cadence:
#' old = set_defaults("2025-01-01", "1 week")
#'
#' # this sets the default for interpreting underqualified time_periods:
#' print(as.time_period(1:10))
#'
#' # The default can always be overridden on a case by case basis:
#' print(as.time_period(1:10, unit="1 day"))
#'
#' # or for a whole expression:
#' with_defaults("2020-01-01", "1 day", {
#'   print(as.time_period(1:10))
#' })
#'
#' # components can be changed individually, firstly origin:
#' set_default_start("2025-01-01")
#' print(as.time_period(1:10))
#'
#' # now cadence:
#' set_default_unit("1 day")
#' print(as.time_period(1:10))
#'
#' # clear the values:
#' set_defaults(NULL,NULL)
#'
#' # A sufficiently qualified call will set the defaults:
#' defined = as.time_period(as.Date("2024-01-01")+0:10*7, anchor="start")
#' inherit = as.time_period(0:10)
#'
#' all(defined == inherit)
#'
#' # restoring the original values (which might be null)
#' set_defaults(old)
#'
set_defaults = function(start_date, unit) {
  if (is.list(start_date)) {
    unit = start_date$unit
    start_date = start_date$start_date
  }
  invisible(list(
    start_date = set_default_start(start_date),
    unit = set_default_unit(unit)
  ))
}

#' @describeIn set_defaults Set defaults temporarily and execute expression
#' @concept time_period
#' @param expr an expression to evaluate with the defaults set to the provided
#'   values.
#' @export
with_defaults = function(start_date, unit, expr) {
  oldstart = .time_period_defaults$start
  oldunit = .time_period_defaults$unit
  on.exit(set_defaults(oldstart, oldunit), add = TRUE)
  set_defaults(start_date, unit)
  invisible(eval(expr, envir = rlang::caller_env()))
}

### Start date defaults ----

#' @rdname set_defaults
#' @concept time_period
#' @export
set_default_start = function(date) {
  old = .time_period_defaults$start
  date = try(as.Date(date), silent = TRUE)
  if (!is.Date(date)) {
    stop("Default start date must be a valid date.")
  }
  # Make sure it is always a numeric date and not integer
  .time_period_defaults$start = as.Date(as.numeric(date))
  return(old)
}

# sets the default start date if it is not already there.
.create_default_start = function(date) {
  if (is.null(.time_period_defaults$start)) {
    set_default_start(date)
  }
  return(.time_period_defaults$start)
}

# gets the start date of the current set of dates. Uses specified start date,
# or combination of dates and anchor, or if these are not specified a cached
# value, based on previous conversions. Sets the cached value is if it is not
# already set.
.get_start_date = function(dates, anchor = NULL, start_date = NULL, ...) {
  if (is.time_period(dates)) {
    return(attr(dates, "start_date"))
  }

  if (length(.time_period_defaults$start) != 1) {
    # No default set yet
    if (is.null(start_date)) {
      if (is.null(anchor)) {
        # fallback use unset default:
        default_anchor = getOption("day_zero", "start")
        start_date = .start_from_anchor(dates, default_anchor)
        .message_once(sprintf(
          "No `start_date` or `anchor` specified. Inferring default from dates: %s (use `set_default_start(...)` to change)",
          format(start_date)
        ))
        set_default_start(start_date)
        return(start_date)
      } else {
        # if anchor given use it to get a start date
        start_date = .start_from_anchor(dates, anchor)
      }
    }
    set_default_start(start_date)
    return(.time_period_defaults$start)
  } else {
    # default already set
    if (is.null(start_date) && is.null(anchor)) {
      # .message_once(
      #   "No `start_date` or `anchor` specified. Using cached value (N.b. use `set_default_start(...)` to change): ",
      #   .time_period_defaults$start
      # )
      return(.time_period_defaults$start)
    }
    if (!is.null(anchor)) {
      # if anchor given use it to get a start date
      start_date = .start_from_anchor(dates, anchor)
    }
    # don't set this as the default
    return(as.Date(start_date))
  }
}

# Get the start date from data
.start_from_anchor = function(dates, anchor) {
  start_date = try(as.Date(anchor), silent = TRUE)
  if (!is.Date(start_date)) {
    if (!is.Date(dates)) {
      stop(
        "Cannot determine a date for `",
        anchor,
        "` for an input of type: ",
        class(dates)[1],
        "\n",
        "Do you need to specify a `start_date` or set `options(day_zero = ... DATE ...)`?",
        call. = FALSE
      )
    }
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
      stop(
        "`anchor` was not valid (a date or one of 'start', 'end', or a weekday name)."
      )
    }
  }
  return(start_date)
}


### Unit defaults ----

#' @describeIn set_defaults Set the default unit only
#' @export
set_default_unit = function(unit) {
  old = .time_period_defaults$unit
  if (is.null(unit)) {
    .time_period_defaults$unit = NULL
  } else {
    unit = .make_unit(unit)
    .time_period_defaults$unit = unit
  }
  return(old)
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

# sets the default unit if it is not already there.
.create_default_unit = function(unit) {
  if (is.null(.time_period_defaults$unit)) {
    set_default_start(unit)
  }
  return(.time_period_defaults$unit)
}

# gets the natural unit of the current set of dates. Uses specified unit or if
# this is not specified a cached value, based on previous conversions.
# Sets the cached value is if it is not already set.
# e.g.
# .get_unit(Sys.Date()+1:10*7) # should set to 1 week if not already
# .get_unit(Sys.Date()+10:20) # should be the same value
# .get_unit(Sys.Date()+10:20, unit="1 day") # should be different value
# set_default_unit("1 day")
.get_unit = function(dates, unit = NULL, ...) {
  if (length(unit) > 1) {
    stop("Multiple units given when one is expected")
  }
  if (length(.time_period_defaults$unit) != 1) {
    # No default defined
    if (is.null(unit)) {
      unit = .make_unit(.day_interval(dates))
      .message_once(sprintf(
        "No `unit` specified. Inferring default from input: %s (N.B. use `set_default_unit(...)` to change)",
        .fmt_unit(unit)
      ))
    }
    set_default_unit(unit)
    return(.time_period_defaults$unit)
  } else {
    # default already defined
    if (is.null(unit)) {
      unit = .time_period_defaults$unit
    }
    return(.make_unit(unit))
  }
}


#' Guess the intervals between a sequence of dates:
#' @param dates a set of dates
#' @returns a natural number of days between ordering
#' @keywords internal
#' @unit
#' r = c(1,rpois(10, 3)+1)
#' interval = 2
#' dates = as.Date("2025-01-01")+r*interval
#' testthat::expect_equal(.day_interval(dates) == interval, TRUE)
#'
#' interval = 0.5
#' dates = as.Date("2025-01-01")+r*interval
#' testthat::expect_equal(.day_interval(dates) == interval, TRUE)
#'
.day_interval = function(dates) {
  dates = sort(unique(dates))
  if (length(dates) < 4) {
    return(1)
  }
  lags = stats::na.omit(as.numeric(abs(dates - dplyr::lag(dates))))
  lags = round(lags, digits = 5)
  if (any(lags < 1)) {
    denom = min(lags)
    lags = lags / min(lags)
    lags = round(lags)
    interval = .gcd(lags) * denom
    return(interval)
  }
  lags = round(lags)
  interval = .gcd(lags)
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

## Date utility functions ----

# rlang::on_load({
#   requireNamespace("lubridate",quietly = TRUE)
# })

as_Date = function(x, ...) {
  x = as.Date(x, ...)
  if (typeof(unclass(x)) == "integer") {
    x = as.Date(as.numeric(x))
  }
  return(x)
}

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
  inherits(x, "Date")
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

# new_time_period = function(x, start_date, unit) {
#   return(
#     structure(
#       as.numeric(x),
#       start_date = as_Date(start_date),
#       unit = .make_unit(unit),
#       class = unique(c("time_period", class(as.numeric(x))))
#     )
#   )
# }

### Convert from a time period ----

#' @export
as.numeric.time_period = function(x, ...) {
  vctrs::vec_cast(x, double())
}

#' @export
as.integer.time_period = function(x, ...) {
  vctrs::vec_cast(x, integer())
}

#' @export
as.Date.time_period = function(x, ...) {
  return(vec_cast.Date.time_period(x))
}

#' @export
as.POSIXct.time_period = function(x, ...) {
  return(vec_cast.POSIXct.time_period(x))
}

### Convert to a time period class ----

#' Time period S3 class methods
#'
#' Time periods are just a zero based numeric representation of dates with a
#' time unit baked in. This allows variable length periods (e.g. days or weeks),
#' and fractional days to be represented in a consistent(ish) way between
#' things that want to deal in dates (like ggplot) and things that want to deal
#' in numbers (like model fitting)
#'
#' @param x a vector of dates, numbers (may be integer or real) or a `time_period`
#'   to convert to a `time_period`
#' @param unit the length of one unit of time. This will be either a integer
#'   number of days, or a specification such as "1 week", or another `time_period`.
#'   If `x` is a `time_period`, and the unit is different to that of `x`
#'   this will return a rescaled `time_period` using the new units.
#' @param start_date the zero time date as something that can be coerced to a
#'   date. If the `x` input is already a `time_period` and this is different to its
#'   `start_date` then `x` will be recalibrated to use the new start date.
#' @param anchor only relevant if `x` is a vector of dates, this is a date, or
#'   `"start"` or `"end"` or a weekday name e.g.
#'   `"mon"`. With the vector of dates in `x` it will use this anchor to find a reference date for
#'   the time-series. If not provided then the current defaults will be used.
#'   (see [set_defaults()])
#' @param dates a vector of dates to convert to a `time_period`
#' @param timepoints a `time_period` vector to convert to a set of dates.
#'
#' @concept time_period
#' @export
#'
#' @examples
#'
#' #' # 100 weeks from 2020-01-01
#'
#' tmp = as.time_period(0:100, 7, "2020-01-01")
#' as.Date(tmp)
#'
#' range(tmp)
#' min(tmp)
#' tmp2 = as.integer(as.Date(tmp))
#' # testthat::expect_true(all(na.omit(tmp2-lag(tmp2)) == 7))
#'
#' tmp2 = as.time_period(0:23, 1/24, "2020-01-01")
#' as.POSIXct(tmp2)
#'
#' # convert timeseries to new "unit"
#' tmp = as.time_period(0:100, 7, "2020-01-01")
#' tmp2 = as.time_period(tmp,1)
#' testthat::expect_equal(as.numeric(tmp2), 0:100*7)
#'
#' # 100 weeks from 2020-01-01
#'
#' tmp = as.time_period(0:100, 7, "2020-01-01")
#' as.Date(tmp)
#'
#' range(tmp)
#' min(tmp)
#' tmp2 = as.integer(as.Date(tmp))
#' # testthat::expect_true(all(na.omit(tmp2-lag(tmp2)) == 7))
#'
#' tmp2 = as.time_period(0:23, 1/24, "2020-01-01")
#' as.POSIXct(tmp2)
#'
#' # convert timeseries to new "unit"
#' tmp = as.time_period(0:100, 7, "2020-01-01")
#' tmp2 = as.time_period(tmp,1)
#' testthat::expect_equal(as.numeric(tmp2), 0:100*7)
#'
#' # Time to date
#' times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
#' dates = time_to_date(times)
#'
#' # Date to time
#' times = date_to_time(as.Date("2019-12-29")+0:100, "1 week")
#' dates = time_to_date(times)
#'
#' @unit
#' tmp = as.time_period(grates::as_epiweek("2019-W12", format = "yearweek")+0:2)
#' testthat::expect_equal(
#'   lubridate::epiweek(as.Date(tmp)),
#'   c(12, 13, 14)
#' )
#'
#' tmp = as.time_period(grates::as_isoweek("2019-W12", format = "yearweek")+0:2)
#' testthat::expect_equal(
#'   lubridate::isoweek(as.Date(tmp)),
#'   c(12, 13, 14)
#' )
#'
#' x = as.time_period(as.Date("2025-01-01")+0:2, anchor="start", unit="1 day")
#' y = as.time_period(as.Date("2025-01-07")+0:2, anchor="start", unit="1 day")
#' testthat::expect_equal(as.numeric(c(x, y)), c(0, 1, 2, 6, 7, 8))
#' testthat::expect_equal(as.numeric(c(y, x)), c(0, 1, 2, -6, -5, -4))
#'
#' z = as.time_period(as.Date("2025-01-01")+0:2*7, anchor="start", unit="1 week")
#' testthat::expect_equal(as.numeric(c(x, z)), c(0, 1, 2, 0, 7, 14))
#' testthat::expect_equal(
#'   as.numeric(c(z, x)),
#'   c(0, 1, 2, 0, 0.142857142857143, 0.285714285714286)
#' )
#'
#' testthat::expect_equal(
#'   as.Date(c(y, z)),
#'   structure(c(20095, 20096, 20097, 20089, 20096, 20103), class = "Date")
#' )
#'
#' seq(x[[1]], y[[1]])
#'
as.time_period = function(
  x,
  ...
) {
  UseMethod("as.time_period")
}

#' @export
as.time_period.default = function(x, ...) {
  x = try(as.Date(x))
  if (inherits(x, "try_error")) {
    stop("Cannot convert a ", class(x)[[1]], " to a `time_period`")
  }
  as.time_period(x, ...)
}

#' @rdname as.time_period
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

#' @rdname as.time_period
#' @export
as.time_period.Date = function(
  x,
  unit = NULL,
  anchor = NULL,
  ...
) {
  if (is.time_period(unit)) {
    start_date = .get_meta(unit)$start_date
    unit = .get_meta(unit)$unit
  } else {
    start_date = .get_start_date(
      x,
      anchor = anchor
    )
    unit = .get_unit(x, unit)
  }

  tmp = date_to_time(x, unit = unit, start_date = start_date)
  names(tmp) = names(x)
  return(tmp)
}

#' @rdname as.time_period
#' @export
as.time_period.numeric = function(
  x,
  unit = NULL,
  start_date = NULL,
  ...
) {
  if (is.time_period(unit)) {
    start_date = .get_meta(unit)$start_date
    unit = .get_meta(unit)$unit
  } else {
    start_date = .get_start_date(
      x,
      start_date = start_date
    )
    unit = .get_unit(x, unit)
  }
  times = as.numeric(x)

  tmp = new_time_period(
    times,
    start_date = start_date,
    unit = unit
  )
  names(tmp) = names(x)
  return(tmp)
}

#' @rdname as.time_period
#' @export
as.time_period.grates_epiweek = function(x, ...) {
  as.time_period(as.numeric(x), start_date = "1970-01-04", unit = "1 week")
}

#' @rdname as.time_period
#' @export
as.time_period.grates_isoweek = function(x, ...) {
  as.time_period(as.numeric(x), start_date = "1969-12-29", unit = "1 week")
}

#' @export
as.time_period.grates_yearweek_monday = function(x, ...) {
  as.time_period(as.numeric(x), start_date = "1969-12-29", unit = "1 week")
}

#' @export
as.time_period.grates_yearweek_tuesday = function(x, ...) {
  as.time_period(as.numeric(x), start_date = "1969-12-30", unit = "1 week")
}

#' @export
as.time_period.grates_yearweek_wednesday = function(x, ...) {
  as.time_period(as.numeric(x), start_date = "1969-12-31", unit = "1 week")
}

#' @export
as.time_period.grates_yearweek_thursday = function(x, ...) {
  as.time_period(as.numeric(x), start_date = "1970-01-01", unit = "1 week")
}

#' @export
as.time_period.grates_yearweek_friday = function(x, ...) {
  as.time_period(as.numeric(x), start_date = "1970-01-02", unit = "1 week")
}

#' @export
as.time_period.grates_yearweek_saturday = function(x, ...) {
  as.time_period(as.numeric(x), start_date = "1970-01-03", unit = "1 week")
}

#' @export
as.time_period.grates_yearweek_sunday = function(x, ...) {
  as.time_period(as.numeric(x), start_date = "1970-01-04", unit = "1 week")
}

#' @rdname as.time_period
#' @export
as.time_period.grates_period = function(x, ...) {
  unit = .make_unit(attr(x, "n"))
  start_date = min_date(x)
  as.time_period(
    as.Date(x) + attr(x, "offset"),
    start_date = start_date,
    unit = unit
  )
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


#' @describeIn as.time_period Create a sequence using `time_period`s
#' @inheritParams base::seq
#' @export
seq.time_period = function(
  from,
  to = from,
  ...
) {
  to = vec_cast(to, from)
  orig_unit = attributes(from)$unit
  orig_start_date = attributes(from)$start_date
  x = seq.default(from = as.numeric(from), to = as.numeric(to), ...)
  return(new_time_period(
    as.numeric(x),
    start_date = orig_start_date,
    unit = orig_unit
  ))
}

#
# .convert_units = function(x, original) {
#   orig_unit = attributes(original)$unit
#   orig_start_date = attributes(original)$start_date
#   as.time_period.time_period(x, orig_unit, orig_start_date)
# }

#' @describeIn as.time_period Check is a `time_period`
#' @export
is.time_period = function(x) {
  return(inherits(x, "time_period"))
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
#' eg = as.time_period(Sys.Date()+0:10*7, unit="1 week", anchor="start")
#'
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

#' @describeIn as.time_period Convert a set of dates to numeric timepoints
#'
#' @importMethodsFrom lubridate /
#' @importMethodsFrom lubridate *
#' @importMethodsFrom lubridate +
#' @importMethodsFrom lubridate -
#'
#' @return a vector of class `time_period`
#' @export
#'
#' @concept time_period
date_to_time = function(
  dates,
  unit = NULL,
  start_date = NULL
) {
  if (is.time_period(unit)) {
    start_date = .get_meta(unit)$start_date
    unit = .get_meta(unit)$unit
  } else {
    unit = .get_unit(dates, unit)
    start_date = .get_start_date(dates, start_date = start_date)
  }

  ival = lubridate::interval(start_date, dates) / unit
  return(new_time_period(
    as.numeric(ival),
    start_date = start_date,
    unit = unit
  ))
}

#' @describeIn as.time_period Convert a set of time points to dates
#' @export
#' @concept time_period
#'
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
  if (rlang::is_integerish(timepoints)) {
    return(start_date + unit * round(timepoints))
  }

  # Interpolate fractional time periods.
  x = sort(unique(c(floor(timepoints), ceiling(timepoints))))
  y = start_date + unit * as.numeric(x)
  # as.Date(floor(stats::approx(x,y,xout=timepoints)$y),"1970-01-01")
  as.Date(stats::approx(x, y, xout = timepoints)$y, "1970-01-01")
}

# # Suppose the metadata has been stripped off a time_period. Here we can reconstruct it
# # from a numeric vector of times and a date vector.
# .infer_units = function(times, dates) {
#   if (is.time_period(times)) {
#     return(times)
#   }
#   dates = unique(sort(dates))
#   times = unique(sort(times))
#   interval = as.numeric(stats::na.omit(dates - dplyr::lag(dates))) /
#     as.numeric(stats::na.omit(times - dplyr::lag(times)))
#   if (all(interval %% 7 == 0)) {
#     n = .gcd(interval %/% 7)
#     unit = lubridate::as.period(n, unit = "week")
#   } else if (round(min(interval, na.rm = TRUE)) >= 365) {
#     n = .gcd(interval %/% 365)
#     unit = lubridate::as.period(n, unit = "year")
#   } else if (round(min(interval, na.rm = TRUE)) >= 28) {
#     n = .gcd(round(interval / 30))
#     unit = lubridate::as.period(sprintf("%d month", n))
#   } else {
#     n = .gcd(interval)
#     unit = lubridate::as.period(n, unit = "day")
#   }
#
#   ends = dates - floor(times) * unit
#   starts = dates - ceiling(times) * unit
#   possible_start_dates = stats::na.omit(
#     starts + round(as.numeric(ends - starts) * (1 + floor(times) - times))
#   )
#
#   if (length(unique(possible_start_dates)) != 1) {
#     stop(
#       "Could not infer start date from dates and times, having units of: ",
#       unit,
#       ", possibilities are ",
#       paste0(sort(unique(possible_start_dates)), collapse = ", ")
#     )
#   }
#   return(as.time_period.numeric(
#     times,
#     start_date = unique(possible_start_dates),
#     unit = unit
#   ))
# }

## Date cutting functions ----

# create a new time_period on the same scale as the old one but
# as an ordered sequence of one day intervals. this is used during conversions
# of irregular line lists to ordered time series, but mostly as an output for
# incidence / growth rate estimates.
.daily_times = function(times, extend_start = 0, extend_end = 0) {
  start_time = min(times, na.rm = TRUE)
  end_time = max(times, na.rm = TRUE)
  start_date = attributes(times)$start_date
  orig_unit = attributes(times)$unit
  dates = as.Date(
    seq(
      time_to_date(start_time, orig_unit, start_date) - extend_start,
      time_to_date(end_time, orig_unit, start_date) + extend_end,
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
#' This is useful if you want to fill in missing values that should have been
#' observed but weren't. For example, date_seq(c(1, 2, 4, 6), 1) will return
#' 1:6.
#'
#' @param x a numeric or date vector
#' @param period Gap between each observation. The existing data will be checked
#'   to ensure that it is actually of this periodicity.
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
  start_date = .get_start_date(dates, anchor = anchor)
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

  times = x
  start_date = attributes(times)$start_date
  orig_unit = attributes(times)$unit

  period = .make_unit(period)

  if (period == attributes(x)$unit) {
    # no change of unit.
    new_times = date_seq.numeric(as.numeric(x), .step(as.numeric(x)))
    return(new_time_period(new_times, start_date = start_date, unit = period))
  }

  dates = date_seq.Date(
    time_to_date(range(x, na.rm = TRUE)),
    period = period,
    anchor = start_date,
    complete = complete,
    ...
  )
  date_to_time(dates, unit = orig_unit, start_date = start_date)
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
#' fs = date_seq(dates, "2 days")
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
  start_date = .get_start_date(dates, anchor = anchor)

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
  na.value = "Unknown",
  ...
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
    label = rep(ifmt, length(start_label))
    label = .str_replace(label, "{start}", start_label)
    label = .str_replace(label, "{end}", end_label)
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
