## Custom scales ----

#' A scales breaks generator for log1p scales
#'
#' @param n the number of breaks
#' @param base the base for the breaks
#'
#' @return a function for ggplot scale breaks
#' @export
#'
#' @examples
#' ggplot2::ggplot(ggplot2::diamonds, ggplot2::aes(x=price))+
#'   ggplot2::geom_density()+
#'   ggplot2::scale_x_continuous(trans="log1p", breaks=breaks_log1p())
breaks_log1p = function(n=5,base=10) {
  #scales::force_all(n, base)
  n_default = n
  function(x, n = n_default) {
    tmp = scales::breaks_log(n_default,base)(x+1,n)
    return(c(0,tmp[-1]))
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
  tmp_fn = scales::extended_breaks(n=n)
  breaks_fn = function(x, n=n_default) {
    x %>% trans() %>% tmp_fn(n=n) %>% inv()
  }

  scales::trans_new("logit",
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
#'
#' @return a ggplot scale
#' @export
scale_y_log1p = function(..., n=5, base=10, dp=0) {
  return(ggplot2::scale_y_continuous(trans="log1p", breaks = breaks_log1p(n,base), labels = ~ sprintf("%.*f",dp,.x), ...))
}

#' A logit y scale
#'
#' @inheritParams ggplot2::scale_y_continuous
#'
#' @return a ggplot scale
#' @export
scale_y_logit = function(...) {
  return(ggplot2::scale_y_continuous(trans="logit", ...))
}

# make a colour aesthetic apply to fill
.fill_col = function(mapping) {
  if (is.null(mapping$fill)) {
    mapping$fill = mapping$colour
    mapping$colour=NULL
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
.layer = function(geom, data = NULL, mapping, ..., .default = list(), .switch_fill = inherits(geom,"GeomRibbon")) {
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
      mapping = mapping,
      position = dots$position %||% "identity",
      show.legend = dots$show.legend %||% TRUE,
      inherit.aes = dots$inherit.aes %||% FALSE,
      check.aes = dots$check.aes %||% TRUE,
      check.param = dots$check.param %||% TRUE,
      param = .flt(geom, dots, .default = .default)
    )
  )
}

# defaults

.growth_scale_limits = function() {
  return(getOption("growthrates.growth_scale_limit",default = c(-0.15,0.15)))
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
.pdf_safe = function(label, alt=label) {
  alt = iconv(alt, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  if (names(grDevices::dev.cur())=="pdf") {
    return(alt)
  }
  return(label)
}

