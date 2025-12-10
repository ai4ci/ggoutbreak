# Linelist coercion ----

#' Coerce an object to a `ggoutbreak` compatible case linelist.
#'
#' @param x An object to coerce
#' @inheritDotParams as.time_period unit start_date anchor
#'
#' @returns minimally a time stamped linelist dataframe `r i_timestamped`
#' @export
#' @concept models
#'
#' @examples
#' (Sys.Date()+stats::runif(100)*7) %>% linelist()
linelist = function(x, ...) {
  UseMethod("linelist", x)
}

#' @export
linelist.default = function(x, ...) {
  x = try(linelist(as.Date(x), ...), silent = TRUE)
  if (inherits(x, "try-error")) {
    stop("cannot coerce an object of class, `", class(x)[1], "` to a linelist")
  }
  return(x)
}

#' @export
linelist.Date = function(x, ...) {
  return(dplyr::tibble(time = as.time_period(x, ...)))
}

#' @export
linelist.data.frame = function(x, ..., date = NULL) {
  # Check is already conformant:
  if (interfacer::itest(x, iface = i_timestamped)) {
    return(x)
  }

  # grps = x %>% dplyr::groups()

  if ("time" %in% colnames(x) && is.time_period(x$time)) {} else {
    # Locate and convert a date column:
    date_col = rlang::enexpr(date) %||%
      .existing_col(x, date, is.Date) %||%
      .detect_dates(x)
    x = x %>%
      dplyr::mutate(time = as.time_period(!!date_col, ...)) # %>%
  }
  return(x)
}

# Timeseries coercion ----

#' Coerce an object to a `ggoutbreak` compatible time series dataframe
#'
#' @param x An object to coerce e.g. a data frame
#' @param date R expression defining a date column (as a `Date` - optional)
#' @param count R expression defining a count column (as an `int` - optional)
#' @param denom R expression defining a denominator column (as an `int` - optional)
#' @param population R expression defining a population column (as an `numeric` - optional)
#' @param class R expression defining a class column (as an `factor` - optional)
#' @param declutter logical flag. If `TRUE` then unnecessary original columns will be dropped
#'
#' @inheritDotParams as.time_period unit start_date anchor
#'
#' @returns minimally a case count dataframe `r i_incidence_data`
#' @export
#' @concept models
#'
#' @examples
#'
#' utils::data("mers_2014_15", package="EpiEstim")
#'
#' set_default_unit("1 day")
#'
#' # Use an expression to generate a case count:
#' # N.B. complex column names need to be surrounded with bcakticks like this:
#'
#' tmp = mers_2014_15$incidence %>%
#'   timeseries(
#'     date = `mers$dates`,
#'     count = local+imported
#'   ) %>%
#'   dplyr::glimpse()
#'
#' if (interactive()) {
#'   ip = make_fixed_ip(mean = mers_2014_15$si$mean_si, sd = mers_2014_15$si$std_si)
#'   tmp %>% poisson_locfit_model(ip=ip,window=14) %>% plot_rt()
#' }
#'
#'
#' # remove columns not needed by `ggoutbreak`
#' outbreaks::dengue_fais_2011 %>%
#'   timeseries(date = onset_date, count = value, declutter = TRUE) %>%
#'   dplyr::glimpse()
#'
#'
#' # date column already exists,  could use `onset` or `death` as count
#' outbreaks::ebola_kikwit_1995 %>%
#'   timeseries(count = onset) %>%
#'   dplyr::glimpse()
#'
#' # This data set needs grouping to make it a unique timeseries:
#' ukc19::ltla_cases %>%
#'   timeseries(anchor="start", unit="1 day") %>%
#'   dplyr::glimpse()
#'
#' # from an incidence object:
#' if (requireNamespace("incidence",quietly = TRUE)) {
#'
#'   onset = outbreaks::ebola_sim$linelist$date_of_onset
#'   sex = outbreaks::ebola_sim$linelist$gender
#'   inc.week.gender = incidence::incidence(onset, interval = 7, groups = sex, standard = FALSE)
#'
#'   inc.week.gender %>% timeseries() %>% dplyr::glimpse()
#'
#'   d = Sys.Date() + sample(-3:10, 10, replace = TRUE)
#'   di = incidence::incidence(d, interval = "week", first_date = Sys.Date() - 10, standard = TRUE)
#'   di %>% timeseries(declutter=TRUE) %>% dplyr::glimpse()
#' }
#'
#' if (requireNamespace("incidence2",quietly = TRUE)) {
#'   dat = outbreaks::ebola_sim_clean$linelist
#'   x = incidence2::incidence(dat, "date_of_onset", groups = c("gender", "hospital"), interval="week")
#'   x %>% timeseries() %>% dplyr::glimpse()
#' }
#'
timeseries = function(x, ...) {
  UseMethod("timeseries", x)
}

#' @rdname timeseries
#' @export
timeseries.default = function(x, ...) {
  stop(
    "Don't know how to convert an object of class ",
    class(x)[1],
    " to a timeseries."
  )
}

#' @rdname timeseries
#' @export
timeseries.incidence2 = function(x, ...) {
  date_index = as.symbol(attr(x, "date_index"))
  count_variable = as.symbol(attr(x, "count_variable"))
  count_value = as.symbol(attr(x, "count_value"))
  grps = sapply(attr(x, "groups"), as.symbol)
  df = x %>%
    dplyr::transmute(
      !!!grps,
      time = as.time_period(!!date_index),
      statistic = forcats::as_factor(!!count_variable),
      count = !!count_value
    ) %>%
    dplyr::group_by(
      statistic,
      !!!grps
    )
  return(df)
}


#' @rdname timeseries
#' @export
timeseries.data.frame = function(
  x,
  ...,
  date = NULL,
  count = NULL,
  denom = NULL,
  population = NULL,
  class = NULL,
  declutter = FALSE
) {
  # Check is already conformant:
  if (interfacer::itest(x, iface = i_incidence_data)) {
    return(x)
  }

  grps = x %>% dplyr::groups()

  if ("time" %in% colnames(x) && is.time_period(x$time)) {} else {
    # Locate and convert a date column:
    date_col = rlang::enexpr(date) %||%
      .existing_col(x, date, is.Date) %||%
      .detect_dates(x)
    x = x %>%
      # dplyr::ungroup() %>%
      dplyr::mutate(time = as.time_period(!!date_col, ...)) # %>%
    # dplyr::group_by(!!!grps)
  }

  if (isFALSE(try(count, silent = TRUE))) {
    count_col = NULL
  } else {
    count_col = rlang::enexpr(count) %||%
      .existing_col(x, count, rlang::is_integerish) %||%
      .detect_counts(x)
  }
  denom_col = rlang::enexpr(denom) %||%
    .existing_col(x, denom, rlang::is_integerish)
  population_col = rlang::enexpr(population) %||%
    .existing_col(x, population, is.numeric)
  class_col = rlang::enexpr(class) %||%
    .existing_col(x, class, ~ is.character(.x) || is.factor(.x))

  x = x %>%
    dplyr::mutate(
      ..count = !!count_col,
      ..denom = !!denom_col,
      ..population = !!population_col,
      ..class = !!class_col %>% .ifnotnull(forcats::as_factor)
    )

  retain = setdiff(
    grps,
    c(count_col, denom_col, population_col, class_col)
  )

  if (declutter) {
    x = x %>%
      dplyr::ungroup() %>%
      dplyr::select(
        !!!retain,
        time,
        dplyr::starts_with("..")
      )
  } else {
    x = x %>%
      dplyr::ungroup() %>%
      dplyr::select(
        -dplyr::any_of(c("count", "denom", "population", "class"))
      )
  }
  x = x %>%
    dplyr::rename_with(~ gsub("..", "", .x, fixed = TRUE)) %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(!!!retain)

  if ("class" %in% colnames(x)) {
    x = x %>%
      dplyr::select(!!!retain, class, time, dplyr::everything()) %>%
      dplyr::group_by(class, .add = TRUE)
  } else {
    x = x %>%
      dplyr::select(!!!retain, time, dplyr::everything())
  }

  x = .infer_grouping(x, time)

  return(x)
}

.existing_col = function(x, nm, fn) {
  fn = rlang::as_function(fn)
  nms = rlang::ensym(nm)
  nm = format(nms)
  if (!(nm %in% colnames(x))) {
    return(NULL)
  }
  tst = x[[nm]]
  if (is.list(tst)) {
    if (all(sapply(tst, fn))) return(nms)
  } else {
    if (all(fn(tst))) return(nms)
  }
  return(NULL)
}

#' Finds the first col with groupwise unique dates in it.
#'
#' @param x dataframe
#'
#' @returns a symbol of the column name
#' @noRd
#' @unit
#' x = dplyr::tibble(d = as.Date("2025-01-01")+1:10) %>%
#'   tidyr::crossing(g = c("A","B","C")) %>%
#'   dplyr::group_by(g)
#'
#' testthat::expect_equal(.detect_dates(x), as.symbol("d"))
#'
#' testthat::expect_error(
#'   {
#'     .detect_dates(x, except = "d")
#'   },
#'   "No date column found.",
#'   fixed = TRUE
#' )
#'
.detect_dates = function(x, except = list()) {
  except = sapply(except, format)
  found_date = FALSE
  for (nm in setdiff(colnames(x), except)) {
    nm = as.symbol(nm)
    if (is.Date(x %>% dplyr::pull(!!nm))) {
      found_date = TRUE
      uq = x %>%
        dplyr::summarise(
          grp_uq = !anyDuplicated(x)
        ) %>%
        dplyr::pull(grp_uq) %>%
        all()
      if (uq) {
        message("Using candidate date column: ", format(nm))
        return(nm)
      }
    }
  }
  if (found_date) {
    stop("Non unique date column found. Is grouping correct?", call. = FALSE)
  } else {
    stop("No date column found.", call. = FALSE)
  }
}

#' Finds the first col with intergerish numbers.
#'
#' @param x dataframe
#'
#' @returns a symbol of the column name
#' @noRd
#' @unit
#' x = dplyr::tibble(
#'    d = as.Date("2025-01-01")+1:10,
#'    c = seq(1.0,10.0,1.0),
#'    denom = seq(10,100,10)
#'  ) %>%
#'  tidyr::crossing(g = c("A","B","C")) %>%
#'  dplyr::group_by(g)
#'
#' testthat::expect_equal(.detect_counts(x), as.symbol("c"))
#' testthat::expect_equal(.detect_counts(x,"c"), as.symbol("denom"))
#'
.detect_counts = function(x, except = list()) {
  except = sapply(except, format)
  for (nm in setdiff(colnames(x), except)) {
    nm = as.symbol(nm)
    if (
      is.numeric(x %>% dplyr::pull(!!nm)) &&
        rlang::is_integerish(x %>% dplyr::pull(!!nm))
    ) {
      message("Using candidate count column: ", format(nm))
      return(nm)
    }
  }
  stop("No candidate count column found.", call. = FALSE)
}

#' @rdname timeseries
#' @param declutter logical flag. If `TRUE` then unnecessary original columns will be dropped
#' @export
timeseries.incidence = function(x, ..., declutter = FALSE) {
  # reconstruct a long dataframe
  unit = x$interval
  df = x %>%
    as.data.frame()
  if (dim(x$counts)[2] > 1) {
    df = df %>%
      tidyr::pivot_longer(-dates, names_to = "class", values_to = "count") %>%
      dplyr::mutate(class = forcats::as_factor(class)) %>%
      dplyr::group_by(class)
  } else {
    df = df %>% dplyr::rename(count = counts)
  }
  df = df %>% dplyr::mutate(time = as.time_period(dates, unit = unit))
  if (declutter) {
    df = df %>% dplyr::select(dplyr::any_of(c("class", "time", "count")))
  }
  return(df)
}

# preprocess_data = function(df, multinom, ...,  date_col = "date") {
#
#   grps = df %>% dplyr::groups()
#
#   # Is this a dated line list? i.e. a datafram with a date, or a time, but no count:
#   if (!.has_cols(df, "count")) {
#
#     if (.has_time(df) && !.has_cols(df, "date")) {
#       # need a date column to summarise
#       df = df %>% dplyr::mutate(date = as.Date(time))
#     } else {
#       dateVar = rlang::ensym(date_col)
#       df = df %>% dplyr::rename(date = !!dateVar)
#     }
#
#     # this does everything:
#     df = df %>% time_summarise(...)
#
#   } else {
#
#     # this is a time series already with a count column
#     # could check here it is complete and fill it?
#
#     # make sure there is a time column.
#     if (!.has_time(df)) {
#       dateVar = rlang::ensym(date_col)
#       df = df %>% dplyr::mutate(time = as.time_period(!!dateVar, ...))
#     }
#
#     if (.has_cols(df, "class") && !.has_cols(df, "denom")) {
#       df = df %>%
#         dplyr::group_by(!!!grps, time) %>%
#         dplyr::mutate(denom = sum(count)) %>%
#         dplyr::group_by(!!!grps)
#     }
#
#   }
#
#   if (!multinom) {df = df %>% dplyr::group_by(class, .add = TRUE)}
#
#   return(df)
# }
