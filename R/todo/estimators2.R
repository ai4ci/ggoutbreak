## Date utility ----

# guess the intervals between dates
.day_interval = function(dates) {
  dates = sort(unique(dates))
  if (length(dates) < 4) return(1)
  interval = .gcd(stats::na.omit(dates-dplyr::lag(dates)))
  return(interval)
}

# greatest common denominator
.gcd2 = function(a, b) {
  if (b == 0) a else Recall(b, a %% b)
}

.gcd <- function(...) {
  Reduce(.gcd2, c(...))
}

# convert a date column to a numeric count of intervals from day_zero
# the default value for day zero is a sunday at the start of the covid outbreak.
.date_to_time = function(dates, interval=1) {
  day_zero = as.Date(getOption("day_zero","2019-12-29"))
  out = floor(as.numeric(dates-day_zero)+interval/100)/interval
  attr(out,"interval")=interval
  return(out)
}

# convert a time column of intervals from day_zero to a date
.time_to_date = function(times, interval=NA) {
  day_zero = as.Date(getOption("day_zero","2019-12-29"))
  if(is.na(interval)) interval=attr(times,"interval")
  return(floor(times*interval+interval/100)+day_zero)
}

.date_seq_times = function(times, interval=NA) {
  orig_interval=attr(times,"interval")
  if(is.null(orig_interval)) stop("the original timeseries has lost its metadata")
  if (is.na(interval)) interval=orig_interval
  out = seq(min(times),max(times),by = interval/orig_interval)
  attr(out,"interval")=orig_interval
  return(out)
}

# turn a random set of dates into an evenly spaced set separated by an interval that makes sense given the data
# intervals will be inside the data
.date_seq_dates = function(dates, interval=.day_interval(dates), truncate_partials = FALSE) {
  times = .date_to_time(dates,interval)
  # the +1/interval - 1 here ensures we have a full final interval as defined by the
  # dates. if interval is one this resolves to the max period
  # otherwise it depends on the day of the week of the largest date - a larges date on a Saturday completes a week beginning on a Sunday.
  if (truncate_partials) time_seq = seq(ceiling(min(times)),floor(max(times)+1/interval)-1,1)
  else time_seq = seq(ceiling(min(times-0.01,na.rm = TRUE)),floor(max(times+0.01,na.rm = TRUE)),1)
  date_seq = .time_to_date(time_seq, interval)
  return(date_seq)
}

# TODO: test this a bit.
# full seq dates should be start of periods. interval is length of period.
# checks to see if a date or dates is within a range of dates where the dates define the start of a period
# of size defined by interval parameter as an integer.
.within_sequence = function(dates, date_seq_dates, interval = .day_interval(date_seq_dates)) {
  times = .date_to_time(dates,interval)
  test = .date_to_time(date_seq_dates, interval)
  return(times >= min(test) & times < max(test)+1)
}

# floor to intervals from with zero_day as reference point
# makes a set of dates line up to the lower end of a period
.floor_sequence = function(dates, interval) {
  times = .date_to_time(dates,interval)
  return(.time_to_date(floor(times+1/100)))
}

#.date_seq_dates(c(Sys.Date(),Sys.Date()-7,Sys.Date()-21,Sys.Date()-28))
#.day_interval(Sys.Date()+c(4,8,24))
#.date_seq_dates(Sys.Date()+c(4,8,24))

## .specification_from_formula( formula = ~ date + age + gender + region )
## .specification_from_formula( formula = ~ date + reported(report_date) + age + gender + region )
## .specification_from_formula( formula = class(variant) ~ date + reported(report_date) + age + gender + region )

## .specification_from_formula( formula = n ~ specimen_date + age + gender + region )
## .specification_from_formula( formula = n ~ non_date(something) + specimen_date + age + gender + region )
## .specification_from_formula( formula = count() ~ date() + age + gender + region )
## .specification_from_formula( formula = class(variant) + count(n) ~ date() + age + gender + region )
## .specification_from_formula( formula = class(variant) + count(n) ~ date(specimen) + age + gender + region )
## .specification_from_formula( formula = growth.rate() + Rt() + class(variant) + count(n) ~ date(specimen) + age + gender + region )
## .specification_from_formula(class(pneumo.urine_antigen_test) + count(count) ~ pneumosim_date)
## parse$observations = named_list_lhs
## parse$predictors = named_list_rhs
## parse$groups = unnamed_list_rhs

## Formula utility ----

# rhs of a formula as a string or a one sided formula
.rhs = function(formula, as_formula=FALSE) {
  tmp = as.character(formula)
  if(length(tmp) == 2) tmp = tmp[[2]]
  else tmp = tmp[[3]]
  if (as_formula) {
    return(stats::as.formula(paste0("~",tmp)))
  } else {
    return(tmp)
  }
}

# lhs of a formula as a string or a one sided formula
.lhs = function(formula, as_formula=FALSE) {
  tmp = as.character(formula)
  if(length(tmp) == 2) return(NULL)
  else tmp = tmp[[2]]
  if (as_formula) {
    return(stats::as.formula(paste0("~",tmp)))
  } else {
    return(tmp)
  }
}

# combine a rhs and a lhs into a single formula
.join_sides = function(lhs,rhs) {
  if (rlang::is_formula(rhs)) rhs = .rhs(rhs)
  if (is.null(lhs)) {
    return(stats::as.formula(paste0("~",rhs)))
  } else {
    # if lhs is a formula then it is a right sided formula
    if (rlang::is_formula(lhs)) lhs = .rhs(lhs)
    return(stats::as.formula(sprintf("%s ~ %s", lhs,rhs)))
  }
}

# Update a formula using a new one treating rhs and lhs in the same way
.update = function(x, ...) {
  UseMethod(".update",x)
}

.update.formula = function(x, new) {
  # switch rhs to lhs
  x_rhs = .rhs(x,as_formula=TRUE)
  new_rhs = .rhs(new,as_formula=TRUE)
  update_rhs = stats::update(x_rhs,new_rhs)

  if(is.null(.lhs(x))) {
    # If the original lhs is empty
    if(is.null(.lhs(new))) {
      # the new lhs is also empty
      update_lhs = NULL
    } else {
      # the updated_lh will be the new_lhs but in formulae like
      # new = x + . ~ y
      # the + . term is problematic as it does not get removed
      new_lhs = .lhs(new,as_formula=TRUE)
      if(any(all.vars(new_lhs)==".")) {
        tmp = .lhs(new,as_formula=FALSE) # this gets the LHS as a character.
        tmp = stringr::str_remove(tmp[[1]],"(\\s\\+\\s)?\\.(\\s\\+\\s)?")
        new_lhs = stats::as.formula(paste0("~",tmp))
      }
      update_lhs = new_lhs
    }
  } else {
    # the original lhs is not empty
    x_lhs = .lhs(x,as_formula=TRUE)
    new_lhs = .lhs(new,as_formula=TRUE)
    if (is.null(new_lhs)) {
      update_lhs = NULL
    } else {
      update_lhs = stats::update(x_lhs,new_lhs)
    }
  }
  browser()
  return(.join_sides(update_lhs,update_rhs))
  # as both updates are of the RHS
}

.update.epimetadata = function(x, new) {
  formula = x$formula
  as.epimetadata(.update.formula(formula, new), type=x$type, interval = x$interval)
}

.update.epi_ts = function(x, new) {
  epi = x %>% get_meta()
  return(x %>% set_meta(.update.epimetadata(epi,new)))
}

.update.epi_ll = function(x, new) {
  epi = x %>% get_meta()
  return(x %>% set_meta(.update.epimetadata(epi,new)))
}

## Metadata utility ----

# parse a formula into a specification
.specification_from_formula = function(formula) {

  if (is.null(formula)) return(NULL)
  # vars = all.vars(formula)
  form_chr = as.character(formula)[-1]
  if (length(form_chr) == 1) form_chr = c("",form_chr)
  names(form_chr) = c("lhs","rhs")

  form_chr = form_chr %>%
    purrr::map(stringr::str_split,fixed("+"),n=Inf) %>%
    purrr::flatten() %>%
    purrr::map(stringr::str_trim)
  form_df = form_chr %>% tibble::enframe() %>% tidyr::unnest(c(value)) %>% dplyr::mutate(
    mapping = value %>% stringr::str_extract("(.*)\\(.*\\)") %>% stringr::str_remove("\\(.*\\)"),
    mapped = value %>% stringr::str_remove("(.*)\\(") %>% stringr::str_remove("\\)") %>% stringr::str_remove_all("`"),
    value = ifelse(mapped == "", mapping, mapped)
  ) %>%
    dplyr::select(-mapped) %>%
    dplyr::rename(side = name) %>%
    dplyr::mutate(value = lapply(value,as.symbol))



  if (!any(form_df$mapping=="date",na.rm = TRUE)) {
    first_na = form_df %>% dplyr::filter(is.na(mapping) & side == "rhs") %>% dplyr::pull(value) %>% first()
    form_df = form_df %>% dplyr::mutate(mapping = ifelse(value==first_na,"date",mapping))
    if (!any(form_df$mapping=="date",na.rm = TRUE)) stop("No date column identified. Either date must be first term on the rhs or specifically named date(...)")
  }

  # This will pick up a value only if there is at least one term on the lhs (and it is not already named)
  if (!any(form_df$mapping=="count",na.rm = TRUE)) {
    first_na = form_df %>% dplyr::filter(is.na(mapping) & side == "lhs") %>% dplyr::pull(value) %>% first()
    form_df = form_df %>% dplyr::mutate(mapping = ifelse(value==first_na,"count",mapping))
  }

  if (any(duplicated(stats::na.omit(form_df$mapping)))) stop("duplicate mappings specified in formula: "+formula)
  class(form_df) = c("specification",class(form_df))
  return(form_df)
}

# convert a specification back into a formula
.formula_from_specification = function(specification) {
  specification %>% dplyr::mutate(
    label = sapply(value,as_label),
    term = dplyr::case_when(
      is.na(mapping) ~ label,
      mapping == label ~ paste0(mapping,"()"),
      TRUE ~ paste0(sprintf("%s(%s)",mapping,label)))
  ) %>% dplyr::group_by(side) %>%
    dplyr::summarise(term = paste0(term,collapse=" + ")) %>%
    dplyr::summarise(formula = paste0(term,collapse=" ~ ")) %>%
    dplyr::pull(formula) %>% stats::as.formula()
}

# construst a list of utility functions from a specification object.
.mapper = function(x,...) {
  v = x
  return(list(
    grps = v %>% dplyr::filter(side=="rhs" & is.na(mapping)) %>% dplyr::pull(value),
    date = v %>% dplyr::filter(side=="rhs" & mapping=="date") %>% dplyr::pull(value) %>% `[[`(1),
    incidentals = v %>% dplyr::filter(side=="lhs" & is.na(mapping)) %>% dplyr::pull(value),
    get = function(type) {
      tmp = v %>% dplyr::filter(mapping==type) %>% dplyr::pull(value)
      if (length(tmp) == 0) return(NULL)
      tmp[[1]]
    },
    predictor = function(type) {
      tmp = v %>% dplyr::filter(side=="rhs" & mapping==type) %>% dplyr::pull(value)
      if (length(tmp) == 0) return(NULL)
      tmp[[1]]
    },
    observation = function(type="count") {
      tmp = v %>% dplyr::filter(side=="lhs" & mapping==type) %>% dplyr::pull(value)
      # can subst NULL using !! and it behaves as expected in ggplot and tidyselect
      if (length(tmp) == 0) return(NULL)
      tmp[[1]]
    },
    has_observation = function(type="count") {
      nrow(v %>% dplyr::filter(side=="lhs" & mapping==type)) > 0
    },
    has_predictor = function(type) {
      nrow(v %>% dplyr::filter(side=="rhs" & mapping==type)) > 0
    }
  ))
}


# .var_from_rhs = function(formula, match="date") {
#   if (is.null(formula)) return(NULL)
#   v = .specification_from_formula(formula)
#   sym = v %>% dplyr::filter(side=="rhs" & mapping == match) %>% dplyr::pull(value)
#   if (length(sym)==0) return(NULL)
#   return(sym)
# }
#
# .vars_from_rhs = function(formula) {
#   if (is.null(formula)) return(NULL)
#   v = .specification_from_formula(formula)
#   sym = v %>% dplyr::filter(side=="rhs" & !is.na(mapping)) %>% dplyr::select(mapping,value) %>% tibble::deframe()
#   return(sym)
# }
#
# .grps_from_rhs = function(formula) {
#   if (is.null(formula)) return(NULL)
#   v = .specification_from_formula(formula)
#   sym = v %>% dplyr::filter(side=="rhs" & is.na(mapping)) %>% dplyr::pull(value)
#   return(sym)
# }
#
# .value_from_lhs = function(formula) {
#   if (is.null(formula)) return(NULL)
#   # formula = n ~ date + type(cls) + report::report(spec) + age+gender+region+code
#   v = all.vars(rlang::f_lhs(formula))
#   if (length(v) > 1) stop("Only zero or one variable on lhs allowed, defining the value (e.g. case count)")
#   if (length(v) == 0) return(NULL)
#   return(as.symbol(v))
# }



# .rdeframe = function(form_df, ...) {
#   vars = rlang::ensyms(...)
#   if (length(vars) == 1) {
#     return(form_df %>% dplyr::pull(!!vars[[1]]) %>% unlist())
#   }
#   form_df %>%
#     dplyr::rename(.name = !!vars[[1]]) %>%
#     dplyr::mutate(.name = ifelse(is.na(.name),"na",as.character(.name))) %>%
#     dplyr::group_by(.name) %>% dplyr::group_modify(function(d,g,...) {
#
#       tibble::tibble(.value = list(.rdeframe(d,!!!vars[-1])))
#   }) %>% tibble::deframe()
# }
#
# .map
#
# tmp = .rdeframe(form_df, side, mapping, value)



## metadata object ----

as.epimetadata = function(x, ...) {
  UseMethod("as.epimetadata", x)
}

as.epimetadata.formula = function(x, type, interval = 1, ...) {
  specification = .specification_from_formula(x)
  .make_metadata(x, specification, type, interval)
}

as.epimetadata.specification = function(x, type, interval = 1, ...) {
  formula = .formula_from_specification(x)
  .make_metadata(formula, x, type, interval)
}

.make_metadata = function(
    formula,
    specification,
    type,
    interval
) {
  return(structure(
    list(
      specification = specification,
      formula = formula,
      interval = interval,
      type = type,
      m = .mapper(specification)
    ),
    class = "epimetadata"
  ))
}

.set_interval = function(meta, interval) {
  meta$interval = interval
  return(meta)
}

## generic epi data functions ----

.make_epidata = function(x, meta, cls) {
  .check_conformant(x,meta)
  x = x %>% dplyr::ungroup() %>% dplyr::mutate(.id=dplyr::row_number())
  x = x %>% set_meta(meta)
  class(x) = c(cls, class(x))
  return(x)
}

.guess_type = function(x,meta) {
  if (any(c("epi_ts","epi_ll") %in% class(x))) return(x)
  date = meta$m$date
  grps = meta$m$grps
  cls = meta$m$observation("class")
  if (meta$m$has_observation("count")) {
    # can infer type from metadata
    return(as.epi_ts.data.frame(x, meta$formula, interval = meta$interval))
  }
  if (meta$type=="ts") {
    return(as.epi_ts.data.frame(x, meta$formula, interval = meta$interval))
  }

  grpwise_count_R2 = x %>% dplyr::group_by(!!!grps,!!cls,!!date) %>% dplyr::count() %>% dplyr::pull(n) %>% magrittr::subtract(1) %>% magrittr::raise_to_power(2) %>% mean()
  full = .date_seq_dates(x %>% dplyr::pull(!!date),interval)
  incomplete_ts = x %>% dplyr::group_by(!!!grps,!!cls) %>% dplyr::summarise(matched = sum(!!date %in% full)) %>% dplyr::mutate(missing = length(full)-matched, total=length(full)) %>% dplyr::ungroup() %>% dplyr::summarise(prop = sum(missing)/sum(total)) %>% dplyr::pull(prop)
  if (incomplete_ts < 0.05 & grpwise_count_R2 < 0.01) {
    browser()
    as.epi_ts.data.frame(x, meta$formula, interval = meta$interval)
  } else {
    as.epi_ll.data.frame(x, meta$formula)
  }
}

.check_conformant = function(x, meta) {
  if (!is.data.frame(x)) stop("epi data must be a data frame")
  ok = TRUE
  for (col in meta$specification$value) {
    if (!(rlang::as_label(col) %in% colnames(x))) {
      message("No column found: ",rlang::as_label(col))
      ok = FALSE
    }
  }
  if (!ok) stop("Input is not conformant to specification provided: ",meta$formula)
}

get_meta = function(x) {
  return(attr(x,"epi"))
}

set_meta = function(x, metadata) {
  .check_conformant(x,metadata)
  attr(x,"epi")=metadata
  return(x)
}

is.epi_ll = function(x,...) {
  return(any(c("epi_ll","std_ll") %in% class(x)))
}

is.epi_ts = function(x,...) {
  return(any(c("epi_ts","std_ts") %in% class(x)))
}

## Line list object ----

as.epi_ll = function(x, ...) {
  UseMethod("as.epi_ll",x)
}

as.epi_ll.default = function(x, ...) {
  stop("Can't make a epi line list out of a ",paste0(class(x),collapse=", "))
}

as.epi_ll.data.frame = function(x, formula) {
  # interval in line list defaults to 1.
  meta = as.epimetadata(formula, type="ll")

  m = meta$m
  # date = m$date
  cls = m$observation("class")
  multinom = !rlang::is_null(cls)
  # grps = m$grps
  out_class = "epi_ll"
  if (multinom) out_class = c("epi_multi",out_class)

  if (m$has_observation("count")) stop("Attempting to make a line list out of a object with a 'count' column. Did you mean to use as.epi_ts()?")

  return(.make_epidata(x,meta,out_class))
}

as.epi_ll.epi_ts = function(x, jitter=FALSE) {
  meta = x %>% get_meta()
  m = meta$m
  interval = meta$interval
  cls = m$observation("class")
  grps = m$grps
  date = m$date
  count = m$observation("count")
  multinom = !rlang::is_null(cls)
  # grps = m$grps
  out_class = "epi_ll"
  if (multinom) out_class = c("epi_multi",out_class)

  if(is.null(count)) stop("count column must be present")
  y = x %>% dplyr::group_by(!!!grps,!!date,!!cls) %>% dplyr::group_modify(function(d,g,..) {
    join = unlist(dplyr::map2(d %>% dplyr::pull(.id), d %>% dplyr::pull(!!count), ~ rep(.x,.y)))
    return(d %>% dplyr::select(-count) %>% dplyr::inner_join(tibble::tibble(.id=join), by=".id") %>% dplyr::select(-.id))
  })
  y = y %>% dplyr::ungroup() %>% dplyr::mutate(.id=dplyr::row_number())
  if (jitter & interval > 1) {
    y = y %>% dplyr::mutate(!!date := !!date+floor(stats::runif(nrow(y),max=interval)))
  }
  specification = meta$specification %>% dplyr::filter(!(side == "lhs" & mapping %in% c("count")))
  meta = as.epimetadata(specification, type="ll")
  return(.make_epidata(y,meta,out_class))
}

summary.epi_ll = function(x,...) {
  epi = x %>% get_meta()
  m = epi$m
  cat(sprintf("linelist: %1.0f line list(s), %1.0f entries", length(m$grps)+1, nrow(x)),"\n")
  print(epi$formula, showEnv = FALSE)
}

print.epi_ll = function(x,...) {
  summary(x,...)
  NextMethod(x,...)
}

glimpse.epi_ll = function(x,...) {
  summary(x,...)
  NextMethod(x,...)
}

## Incidence ts object ----

as.epi_ts = function(x, ...) {
  UseMethod("as.epi_ts", x)
}

as.epi_ts.default = function(x, ...) {
  stop("Can't make a epi time series out of a ",paste0(class(x),collapse=", "))
}

as.epi_ts.Date = function(x, count, class = NULL, ...) {

  x = tibble::tibble(date = x, count = count, class = class)
  formula = dplyr::count() ~ date()
  if(!is.null(class)) formula = .update(formula, class() + . ~ .)
  as.epidata.data.frame(x, formula, ...)
}

.null_na = function(x) {ifelse(suppressWarnings(is.na(x)),NULL,x)}

as.epi_ts.data.frame = function(x, formula, interval = NA, rectangular = FALSE, dates = NULL) {
  meta = as.epimetadata(formula, type="ts", interval=interval)
  date = meta$m$date
  # Determine the frequency of the time series
  # either asserted, or by reduction from the data
  if (is.na(meta$interval)) meta$interval = .day_interval(x %>% dplyr::pull(!!date))
  .convert_dataframe(x, meta, rectangular = FALSE, verbose = TRUE, dates = dates)
}

as.epi_ts.epi_ll = function(x, formula = dplyr::count() + . ~ ., interval = 1, dates = NULL) {
  meta = x %>% get_meta()
  new_meta = .update(meta, formula)
  m = new_meta$m
  new_count = m$observation("count")

  # what dates are we looking at?
  orig_dates = x %>% dplyr::pull(!!m$date)
  if (is.null(dates)) dates = orig_dates
  dates = .date_seq_dates(dates,interval, truncate_partials = TRUE)

  y = .convert_dataframe(x %>% dplyr::mutate(!!new_count == 1), new_meta, rectangular = TRUE, verbose = FALSE, dates = dates)
  return(y %>% set_meta(new_meta))
}

.convert_dataframe = function(x, meta, rectangular = FALSE, verbose=FALSE, dates = NULL) {
  if (nrow(x)<2) stop("need multiple time points for a timeseries")
  interval = meta$interval
  m = meta$m
  date = m$date
  cls = m$observation("class")
  value = m$observation("count")
  grps = m$grps

  out_class = c("epi_ts")
  multinom = !rlang::is_null(cls)
  if (multinom) out_class = c("epi_multi",out_class)

  dates_given = !is.null(dates)
  orig_dates = x %>% dplyr::pull(!!date)
  if(!dates_given) dates = .date_seq_dates(orig_dates,interval)

  # make sure data dates are within the range of the desired interval dates

  if (interval > 1) {
    # this is good for linelist type data where we want to make sure we have whole intervals
    # not so good for data already in time series which may "finish" on the first date of an interval.
    x = x %>% dplyr::filter(.within_sequence(!!date, dates, interval))
    x = x %>% dplyr::mutate(!!date := .floor_sequence(!!date, interval))
  }

  # check count values are unique for each combination of date, grouping, and multinom class
  tmp = x %>% dplyr::group_by(!!!grps, !!cls, !!date) %>% dplyr::count()

  if (any(tmp$n > 1)) {
    browser()
    # TODO have to reconsider this as count is a very optional column of time series but others must be
    if (verbose) message("Input dataframe has more than one row per date (and class combination), which may be intentional. Combining (class) counts in multiple rows by summation, any other observations will be lost.")
    if (!is.null(value)) {
      if(any(is.na(tmp %>% dplyr::pull(!!value)))) warning("Count column contains some NA values. The combined count will be NA for these rows.")
      x = x %>% dplyr::group_by(!!!grps, !!cls, !!date) %>% dplyr::summarise(!!value := sum(!!value))
    }
    # since group by summarise steps will remove all other observations we need to make sure that the metadata is updated with the lhs including only class and count.
    specification = meta$specification %>% dplyr::filter(!(side == "lhs" & mapping %in% c("class","count")))
    meta = as.epimetadata(specification, type=meta$type, interval=meta$interval)
  }

  # ensure completeness of dates and (optionally) class on a per group basis
  # step 1 setup the complete combination of dates and classes (if present)
  if (multinom) {
    # ensure counts are complete for each of the outcome classes also as well as for each date.
    clsses = tibble::tibble(x) %>% dplyr::pull(!!cls) %>% unique() %>% sort()
    join_cols = c(rlang::as_label(date),rlang::as_label(cls))
  } else {
    clsses = NULL
    join_cols = rlang::as_label(date)
  }
  # step 2 generate a crossing dataframe of all combination of dates and optionally classes
  # this is the version for rectangular time series, where a single source of data contains
  # the full range of time points for all sources - i.e. there is known to be no differential
  # reporting delay.
  lhs = .dates_and_classes(date,dates,cls,clsses)
  # step 3 left join crossing dataframe with data and fill missing counts with zero.
  # ensuring that the result is valid
  x = tibble::tibble(x) %>%
    dplyr::group_by(!!!grps) %>%
    dplyr::group_modify(function(d,g,...) {
      # do a groupwise join. the lhs is either all dates or all dates and class levels
      # or if we are not using rectangular time series then calculate a group-wise lhs
      # including the range present in the data.
      if (!rectangular & !dates_given) {
        tmp = d %>% dplyr::pull(!!date)
        dates = .date_seq_dates(tmp,interval)
        lhs = .dates_and_classes(date,dates,cls,clsses)
      }
      # do the fill for missing counts.
      d = lhs %>%
        dplyr::left_join(d, by = join_cols)
      if (!is.null(value)) {
        # TODO: what about other observations?
        d = d %>% dplyr::mutate(!!value := ifelse(is.na(!!value),0,!!value))
      }
      return(d)

    }) %>% dplyr::ungroup()

  if (!".id" %in% colnames(x)) {
    # add an .id column only if there is not one already.
    x = x %>% dplyr::mutate(.id=dplyr::row_number())
  }

  return(.make_epidata(
    tibble::as_tibble(x),
    meta,
    out_class))
}

.dates_and_classes = function(date, dates, cls, clsses) {
  if (!is.null(clsses)) {
    lhs = tidyr::crossing(!!date := dates, !!cls := clsses)
  } else {
    lhs = tibble::tibble(!!date := dates)
  }
}

summary.epi_ts = function(x, ...) {
  epi = x %>% get_meta()
  m = epi$m
  dates = x %>% dplyr::pull(!!(m$date)) %>% range()
  grpCount = x %>% dplyr::select(!!!m$grps) %>% dplyr::distinct() %>% nrow()
  cat(sprintf("%1.0f timeseries, with interval %s day(s), from %s up to (but not including) %s, %1.0f total records", grpCount, epi$interval, dates[[1]], dates[[2]]+1+epi$interval, nrow(x)),"\n")
  print(epi$formula, showEnv = FALSE)
}

print.epi_ts = function(x,...) {
  summary(x,...)
  NextMethod(x,...)
}


glimpse.epi_ts = function(x,...) {
  summary(x,...)
  NextMethod(x,...)
}

## standardised formats ----

# force the rename of all observations and predictors, add in .time and .grpId (and optionally .subGrpId if multinomial) columns
.normalise = function(x, interval = NA, ...) {
  meta = x %>% get_meta()
  m = meta$m
  date = m$date
  cls = m$observation("class")
  multinom = !is.null(cls)
  grps = m$grps
  y = x
  for (map in meta$specification$mapping) {
    # rename the columns to their canonical names defined in the specification.
    if (!is.na(map)) {
      value = m$get(map)
      y = y %>% dplyr::mutate(!!map := !!value)
    }
  }
  all = meta$specification$value
  if (is.na(interval)) interval = .day_interval(y$date)

  y = y %>%
    dplyr::select(all_of(stats::na.omit(meta$specification$mapping)), !!!grps, .id)

  y = y %>% dplyr::group_by(!!!grps) %>% dplyr::mutate(
    .grpId = dplyr::cur_group_id(),
    .time = .date_to_time(date, interval),
  ) %>% dplyr::group_by(!!!grps, .grpId)
  if (multinom) {
    y = y %>% dplyr::group_by(!!!grps, class) %>% dplyr::mutate(.subGrpId = dplyr::cur_group_id()) %>% dplyr::group_by(!!!grps, .grpId)
    class(y) = c("epi_multi",class(y))
  } else {
    class(y) = c("epi_simple",class(y))
  }
  return(y)
}

.denormalise = function(x, meta) {
  y=x
  if (".time" %in% colnames(y)) {
    if (!("date" %in% colnames(y))) {
      y = y %>% dplyr::mutate(date = .time_to_date(.time))
    } else if (any(is.na(y$date))) {
      y = y %>% dplyr::mutate(date = .time_to_date(.time))
    }
  }
  y = y %>% dplyr::select(-.time)
  for (map in meta$specification$mapping) {
    # rename the columns to their denormalised names defined in the specification.
    # group columns will probably not have changed name
    if (!is.na(map)) {
      value = m$get(map)
      if (map %in% colnames(y)) {
        y = y %>% dplyr::rename(!!value := !!map)
      }
    }
  }

  old_cols = sapply(meta$specification$value,as_label, USE.NAMES = FALSE)
  new_cols = colnames(y)[!colnames(y) %in% old_cols]
  new_obs = new_cols %>% magrittr::extract(!stringr::str_starts(.,stringr::fixed(".")))
  # all new cols are added as new observations onto the lhs
  new_cols_df = tibble::tibble(
    side = "lhs",
    value = sapply(new_obs, as.symbol,USE.NAMES = FALSE),
    mapping = new_obs
  )

  new_spec = dplyr::bind_rows(
    meta$specification %>% dplyr::filter(sapply(value, as_label, USE.NAMES = FALSE) %in% colnames(y)),
    new_cols_df)
  new_meta = as.epimetadata.specification(new_spec, type=meta$type, interval = meta$interval)

  y = y %>% dplyr::ungroup() %>% dplyr::select(any_of(old_cols),all_of(new_obs),any_of(".id"))
  # y = y %>% dplyr::ungroup() %>% dplyr::select(c(!testthat::starts_with("."),.id)) %>% dplyr::glimpse()
  return(y %>% .guess_type(new_meta))
}

## Execute a simple timeseries processing step on a standard data format ---


execute_epifunction = function(x, .f, ...) {
  # x =tmp
  # .f=estimate_proportion
  # check input is valid for the function
  # this is defined in the function
  meta = x %>% get_meta()
  require_str = formals(.f)[[".require"]]
  input = .normalise(x)
  browser()
  if (!is.null(require_str)) {
    requires = eval(require_str)
    if (!all(requires %in% meta$specification$mapping)) {
      warning("Input data frame does not have all the required columns:")
      warning(meta$formula)
      stop("must contain column mappings for: ",paste0(requires,collapse = ","))
    }
  }
  output = input %>% dplyr::group_modify(function(d,g,...) {
    if ("epi_ll" %in% class(x)) {
      class(d) = c("std_ll",class(d))
      return(.f(d, g=g, ...))
    } else {
      class(d) = c("std_ts",class(d))
      # TODO: informative error messages
      return(.f(d, g=g, ..., interval = meta$interval))
    }
    # execute the epifunction call

  })
  if (!"date" %in% colnames(output)) {
    #TODO: consider autoconverting .time to date
    stop("the result of an epifunction must include a date column")
  }
  return(.denormalise(output, meta))
}

## Simple timeseries functions ----

# a set of estimators for the simple single time series situations
# the estimates target a range of outputs such as poisson rate, proportion model, growth rate, etc.
# these are aimed to be tidy but assume (and enforce) column naming conventions are adhered to
# these do not work on grouped data. they assume the input has been sanitised before hand, although should tolerate NA values.

# make sure a given column exists and create it with the orElse function if not.
ensure_exists = function(df, column, or_else = function(df) {stop("Missing column: ",column)},...) {
  or_else = purrr::as_mapper(or_else)
  out = df
  if(!(all(column %in% colnames(df)))) {
    out = or_else(df,...)
  }
  out
}

# or else NA
.opt = function(expr) tryCatch(expr,error=function(e) NA_real_)

# format a transformed normally distributed variable into quantiles
.format_result = function(df, fit, se.fit, t, estimate, modelName,link, error=NA_character_) {
  est = #purrr::map2(fit,se.fit,.f = function(fit,se.fit) {
    tibble::tibble(
      !!(paste0(link,"(x)")) := fit,
      !!(paste0("SE.",link,"(x)")) := se.fit,
      Quantile.0.025 = .opt(t(stats::qnorm(0.025,fit,se.fit))),
      Quantile.0.05 = .opt(t(stats::qnorm(0.05,fit,se.fit))),
      Quantile.0.25 = .opt(t(stats::qnorm(0.25,fit,se.fit))),
      Quantile.0.5 = t(fit),
      Quantile.0.75 = .opt(t(stats::qnorm(0.75,fit,se.fit))),
      Quantile.0.95 = .opt(t(stats::qnorm(0.95,fit,se.fit))),
      Quantile.0.975 = .opt(t(stats::qnorm(0.975,fit,se.fit))),
      model = modelName,
      error = error)
  #})
  df %>% dplyr::mutate(!!estimate := est)
}

# extract the locfit result from the locfit model and format it
.fixed_result = function(df, estimate, modelName, link, mean = NA_real_, se = NA_real_, error = "unknown error") {
  df %>% .format_result(fit = mean, se.fit= se, t=.inv[[link]], estimate, modelName, link, error)
}

.empty_result = function(df, estimate) {
  df %>% dplyr::mutate(!!estimate := tibble::tibble())
}

.inv = list(
  value = function(x) x,
  log = function(x) {ifelse(x==-Inf, 0, exp(x))},
  logit = function(x) {dplyr::case_when(x==-Inf ~ 0,x==Inf ~ 1, TRUE ~ 1/(1+exp(-x)))}
)

# This is just to format locfit results given a locfit model.
# extract the locfit result from the locfit model and format it
# ... could be where="fitp", or newdata=.time points....
.locfit_extract_result = function(df, model, estimate, modelName, link = "value") {

  tryCatch({
    points = stats::preplot(model,se.fit = TRUE,band="local", newdata = df)

    t = points$trans
    fit = points$fit
    se.fit = tryCatch({
      as.vector(forecast::na.interp(points$se.fit))
    }, error = function(e) {
      rep(NA,length(fit))
    })

    df %>% .format_result(fit,se.fit,t,estimate,modelName,link)

  }, error = function(e) {

    df %>% .fixed_result(estimate,modelName,link,error = e$message)

  })
}

## Proportion estimation ----

# Generate the formula for a locfit model based on things I understand
.locfit_formula = function(valueVar, nrowDf, window, polynomialDegree, nearestNeighbours = TRUE, ...) {
  valueVar=rlang::ensym(valueVar)
  tmp_alpha = min(window/nrowDf,1)
  tmp_alpha_2 = min((window*2+1)/nrowDf,1)
  lpParams = list(
    nn = if( nearestNeighbours ) tmp_alpha_2 else tmp_alpha, # this is given in fraction of total observations
    h = if( !nearestNeighbours ) window else 0, # this is given in units of X
    deg = polynomialDegree
  )
  lpParamsText = paste(names(lpParams),lpParams,sep="=",collapse=", ")
  lpFormula = stats::as.formula(paste0(rlang::as_label(valueVar), " ~ locfit::lp(.time, ",lpParamsText,")"))
  return(lpFormula)
}


.has_count = function(x, ...) {
  "count" %in% colnames(x)
}

# takes a line list or incidence count of patient admissions with a multinomial class label, and fits
# a quasi-binomial model with logit link using local regression. This expects a dataframe
# with an .time column and a class column. Multinomial class is either treated as a
# set of 1 versus others binomials, if the class is unordered (or not a factor) or as a set of less than or equal versus more
# than binomials (if the multinomial class is ordered)
estimate_proportion = function(d, ..., interval = .day_interval(d$date), window = 28, degree=2, quick=NA) {

  d = d %>% ensure_exists("date")
  d = d %>% ensure_exists("class")
  # convert dates to times
  d = d %>% ensure_exists(".time", or_else = ~ dplyr::mutate(., .time = .date_to_time(date, interval)))
  if (is.na(quick)) {
    quick = !((.has_count(d) & sum(d$count) < 10000) | (!.has_count(d) & nrow(d) < 10000))
  }

  is_ts = .has_count(d)
  time_span = (max(d$.time)-min(d$.time))*interval
  # get the output as fractional weeks - and convert to days.
  data_times = seq(min(d$.time),max(d$.time), 1)
  predict_times = seq(min(d$.time),max(d$.time), 1/interval)

  cumulative = is.ordered(d$class)
  model_name = sprintf("locfit:probability:%s:%s:%1.0f*%1.0f:%1.0f",if(cumulative) "cumulative" else "binomial",if(quick) "counts" else "linelist", window, interval,degree)

  out = tibble::tibble()
  # repeat once for each class level. This is a binomial comparison (x vs magrittr::not(x)) or cumulative (<=x) vs (>x)
  for (level in sort(unique(d$class))) {

    if (cumulative) {
      tmpdf = d %>% dplyr::mutate(class_bool = class <= level)
      est_name = "probability.cumulative"
    } else {
      tmpdf = d %>% dplyr::mutate(class_bool = class == level)
      est_name = "probability"
    }
    if (is_ts) {
      # summarise the counts
      tmpdf_quick = tmpdf %>% dplyr::group_by(.time,class_bool) %>% dplyr::summarise(count = sum(count),.groups="drop")
      if(!quick) tmpdf_slow = tmpdf_quick %>% dplyr::group_by(.time,class_bool) %>% dplyr::group_modify(function(d,g,..) {return(tibble::tibble(count = rep(1,d$count)))})
    } %>% {
      tmpdf_slow = tmpdf
      if(quick) tmpdf_quick = tmpdf %>% dplyr::group_by(.time,class_bool) %>% dplyr::summarise(count = dplyr::n(),.groups="drop") %>% tidyr::complete(.time = data_times, class_bool, fill=list(count=0) )
    }

    result = tibble::tibble(.time=predict_times, class=level)

    if (nrow(tmpdf) == 0) {
      # empty estimate
      out = out %>% dplyr::bind_rows(result %>% .fixed_result(est_name,model_name,link = "logit",mean = NA,se = NA, error = "no data"))
    } else if (sum(tmpdf$class_bool) < degree) {
      # zero estimate
      out = out %>% dplyr::bind_rows(result %>% .fixed_result(est_name,model_name,link = "logit",mean = -Inf,se = 10000, error = "all zeros"))
    } else if (sum(!tmpdf$class_bool) < degree) {
      # one estimate
      out = out %>% dplyr::bind_rows(result %>% .fixed_result(est_name,model_name,link = "logit",mean = Inf,se = 10000, error = "all ones"))
    } else {

      tryCatch({
        if (quick) {
          # This is how I expect it to work:
          # lf_form = .locfit_formula(class_bool, time_span, window = window, polynomialDegree = degree, nearestNeighbours = !is_ts)
          # fit = locfit::locfit(lf_form,
          #    data = tmpdf,
          #    weights = count,
          #    family="qbinomial", link="logit")

          # This is what seems to work but does not include any sample size in weighting.
          tmpdf_quick = tmpdf_quick %>% dplyr::group_by(.time) %>% dplyr::mutate(total = sum(count), p=count/total) %>%
            dplyr::filter(class_bool) # %>%
          # this bit does not work either
          # dplyr::mutate(inv_var = 1/(total*p*(1-p))) %>% dplyr::mutate(inv_var = ifelse(is.finite(inv_var),inv_var,1))

          lf_form = .locfit_formula(p, time_span, window = window, polynomialDegree = degree, nearestNeighbours = FALSE)
          # timeseries model when there are counts
          fit = locfit::locfit(lf_form,
                               data = tmpdf_quick,
                               # weights = total, weights =1/total
                               # weights = inv_var,
                               family="qbinomial", link="logit", maxit = 5000, maxk=5000)
          # browser()

        } else {
          # this is the line list version.

          lf_form = .locfit_formula(class_bool, time_span, window = window, polynomialDegree = degree, nearestNeighbours = TRUE)
          # line list model when there are no counts
          fit = locfit::locfit(lf_form,
                               data = tmpdf_slow,
                               family="qbinomial", link="logit", maxit = 5000, maxk=5000)
        }
        tmp = result %>% .locfit_extract_result(model = fit, estimate = est_name, modelName = model_name, link = "logit")

        out = out %>% dplyr::bind_rows(tmp)

      }, error = function(e) {
        browser()
        out = out %>% dplyr::bind_rows(result %>% .fixed_result(est_name,model_name,link="logit",error = e$message))

      })

    }

  }

  # convert times back to dates
  out = out %>% dplyr::mutate(date = .time_to_date(.time))

  # swap factor levels back in
  if (is.factor(d$class)) out = out %>% dplyr::mutate(class = factor(class, levels(d$class), ordered = is.ordered(d$class)))
  return(out)

}



