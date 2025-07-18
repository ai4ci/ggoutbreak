## Custom scales ----

#' A scales breaks generator for log1p scales
#'
#' @param n the number of breaks
#' @param base the base for the breaks
#'
#' @return a function for ggplot scale breaks
#' @export
#' @concept vis
#'
#' @examples
#' ggplot2::ggplot(ggplot2::diamonds, ggplot2::aes(x=price))+
#'   ggplot2::geom_density()+
#'   ggplot2::scale_x_continuous(trans="log1p", breaks=breaks_log1p())
breaks_log1p = function(n = 5, base = 10) {
  #scales::force_all(n, base)
  n_default = n
  function(x, n = n_default) {
    tmp = scales::breaks_log(n_default, base)(x + 1, n)
    return(c(0, tmp[-1]))
  }
}

#' logit scale
#'
#' Perform logit scaling with correct axis formatting.
#' To not be used directly but with ggplot (e.g. ggplot2::scale_y_continuous(trans = "logit") )
#'
#' @param n the number of breaks
#' @param ... not used, for compatibility
#' @return A scales object
#' @concept vis
#'
#' @examples
#'
#' library(ggplot2)
#' library(tibble)
#'
#' tibble::tibble(pvalue = c(0.001, 0.05, 0.1), fold_change = 1:3) %>%
#'  ggplot2::ggplot(aes(fold_change , pvalue)) +
#'  ggplot2::geom_point() +
#'  ggplot2::scale_y_continuous(trans = "logit")
#'
#' @export
logit_trans = function(n = 5, ...) {
  trans = stats::qlogis
  inv = stats::plogis
  n_default = n
  tmp_fn = scales::extended_breaks(n = n)
  breaks_fn = function(x, n = n_default) {
    x %>% trans() %>% tmp_fn(n = n) %>% inv()
  }

  scales::trans_new(
    "logit",
    transform = trans,
    inverse = inv,
    breaks = breaks_fn,
    format = scales::label_scientific(digits = 2)
  )
}


#' A log1p y scale
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param dp decimal points
#' @param base the base for the logarithm
#' @param n  the number of major breaks
#' @concept vis
#'
#' @return a ggplot scale
#' @export
scale_y_log1p = function(..., n = 5, base = 10, dp = 0) {
  return(ggplot2::scale_y_continuous(
    trans = "log1p",
    breaks = breaks_log1p(n, base),
    labels = ~ sprintf("%.*f", dp, .x),
    ...
  ))
}

#' A logit y scale
#'
#' @inheritParams ggplot2::scale_y_continuous
#'
#' @return a ggplot scale
#' @concept vis
#' @export
scale_y_logit = function(...) {
  return(ggplot2::scale_y_continuous(trans = "logit", ...))
}


# `%under%` <- function(plot, layer) {
#   if (missing(layer)) {
#     stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
#   }
#   if (!is.ggplot(plot)) {
#     stop('Need a plot on the left side')
#   }
#   plot$layers = c(head(plot$layers,-1), layer, tail(plot$layers,1))
#   plot
# }

#' Insert a layer at the bottom of a `ggplot`
#'
#' @param plot the plot to add the layer to
#' @param layer the layer to insert underneath the plot
#' @concept vis
#'
#' @return a `ggplot`
#' @export
`%above%` = function(plot, layer) {
  if (missing(layer)) {
    stop(
      "Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?"
    )
  }
  if (!ggplot2::is.ggplot(plot)) {
    stop('Need a plot on the left side')
  }
  plot$layers = c(layer, plot$layers)
  plot
}

#' Strictly integer breaks for continuous scale
#'
#' @inheritParams base::pretty
#' @param n number of breaks
#' @concept vis
#'
#' @return a ggplot breaks function
#' @export
integer_breaks = function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# make a colour aesthetic apply to fill
.fill_col = function(mapping) {
  if (is.null(mapping$fill)) {
    mapping$fill = mapping$colour
    mapping$colour = NULL
  }
  return(mapping)
}

# the subset of ... params that apply to a geom
.flt = function(geom, dots, .default = list()) {
  dots = dots[names(dots) %in% geom$aesthetics()]
  dots = c(dots, .default[!names(.default) %in% names(dots)])
  return(dots)
}

# internal function: allow a ggplot to be constructed more dynamically
.layer = function(
  geom,
  data = NULL,
  mapping,
  ...,
  .default = list(),
  .switch_fill = inherits(geom, "GeomRibbon")
) {
  dots = rlang::list2(...)
  if (.switch_fill) {
    mapping = .fill_col(mapping)
    dots$fill = dots$colour
    dots$colour = NULL
  }
  return(
    ggplot2::layer(
      geom = geom,
      stat = ggplot2::StatIdentity,
      data = data,
      mapping = .check_in_data(data, mapping),
      position = dots$position %||% "identity",
      show.legend = dots$show.legend %||% TRUE,
      inherit.aes = dots$inherit.aes %||% FALSE,
      check.aes = dots$check.aes %||% TRUE,
      check.param = dots$check.param %||% TRUE,
      param = .flt(geom, dots, .default = .default)
    )
  )
}

.check_in_data = function(df, mapping) {
  if (is.null(df)) {
    return(mapping)
  }
  for (k in names(mapping)) {
    tmp = try(rlang::eval_tidy(mapping[[k]], df), silent = TRUE)
    if (inherits(tmp, "try-error") || is.language(tmp) || is.function(tmp)) {
      mapping[[k]] = NULL
    }
  }
  return(mapping)
}

# for use as a default parameter in a function.
# checks a ggplot::aes is not given in the dots, before
.check_for_aes = function(df, ..., class_aes = c("colour", "fill")) {
  class_aes = match.arg(class_aes)
  dots = rlang::list2(...)
  if (length(dots) > 0 && any(sapply(dots, class) == "uneval")) {
    stop(
      "Unnamed `ggplot2::aes` mapping provided. Ggplot aesthetic parameters must be named `mapping=aes(...)`",
      call. = FALSE
    )
  }
  if (interfacer::is_col_present(df, class)) {
    if (class_aes == "fill") {
      return(ggplot2::aes(fill = class))
    }
    return(ggplot2::aes(colour = class))
  } else {
    return(ggplot2::aes())
  }
}

# defaults

.growth_scale_limits = function() {
  return(getOption("ggoutbreak.growth_scale_limit", default = c(-0.15, 0.15)))
}

.r_number_limits = function() {
  return(getOption("ggoutbreak.growth_scale_limit", default = c(0.5, 3.0)))
}

#' Switch UTF-8 into plain text when using the pdf device
#'
#' The plain `pdf()` device is the default when running examples via CRAN R CMD
#' check. It throws warnings and sometimes errors that other devices do not when
#' encountering UTF-8 characters in plot objects (due to a warning from `grid`).
#' The resulting behaviour is R version dependent. It is basically impossible to
#' get round these in your function examples and you either decide not to run
#' them or only run them with non UTF-8 content. This will make that decision at
#' runtime and provide a transliterated alternative for the `pdf` device in the
#' context of a function example in a CRAN check.
#'
#' @param label a UTF8 label
#' @param alt alternative for the pdf device
#'
#' @return the same string or a non UTF alternative if currently using the
#'   legacy pdf device
#' @noRd
#'
#' @examples
#' .pdf_safe("test")
#' .pdf_safe("\u00B1\u221E")
#' ggplot2::ggplot()+ggplot2::xlab(.pdf_safe("\u00B1\u221E"))
.pdf_safe = function(label, alt = label) {
  alt = iconv(alt, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  if (names(grDevices::dev.cur()) == "pdf") {
    return(alt)
  }
  return(label)
}
