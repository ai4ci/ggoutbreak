


#' Reband any discrete distribution
#'
#' e.g. age banded population, or a discrete probability distribution e.g.
#' a serial interval distribution. This method fits a monotonically increasing
#' spline to the cumulative distribution (including the upper and lower limits)
#' and interpolating using that spline to the new cut points.
#'
#' @param x a set of upper limits of bands, e.g. for age: 0-14;15-64;65-79;80+ is 15,65,80,NA
#' @param y a set of quantities for each band e.g. population figures
#' @param xout a set of new upper limits
#' @param xlim Upper and lower limits for x. if the last band is e.g 80+ in the input and we want to know the 85+
#'   band in the output some kind of maximum upper limit is needed to interpolate
#'   to.
#' @param ytotal upper and lower limits for y. If the interpolation values fall outside of
#'   x then the min and max limits of y are given by this. This would be `c(0,1)`
#'   for a probability distribution, for example.
#' @param digits if the `xout` value is continuous then how many significant figures
#'   to put in the labels
#' @param labelling are the `xout` values interpretable as an `inclusive` upper limit, or
#'   an `exclusive` upper limit, or as an upper limit of an `positive_integer`` quantity
#' @param sep seperator for names e.g. `18-24` or `18 to 24`
#'
#' @return a rebanded set of discrete values, guaranteed to sum to the same as `y`
#' @export
#'
#' @examples
#' ul = stringr::str_extract(england_demographics$class, "_([0-9]+)",group = 1) %>%
#'   as.numeric()
#'
#' tmp = reband_discrete(
#'   ul, england_demographics$population,
#'   c(5,10,15,40,80), xlim=c(0,120))
#'
#' tmp
#'
#' sum(tmp)
#' sum(england_demographics$population)
reband_discrete = function(x, y, xout, xlim=c(0,NA), ytotal=c(0,sum(y)), digits=0, labelling = c("positive_integer","inclusive","exclusive"), sep="-") {

  min_x = xlim[[1]]
  max_x = xlim[[2]]
  min_y = ytotal[[1]]
  max_y = ytotal[[2]]

  labelling = match.arg(labelling)
  if (any(is.na(x))) {
    if (!is.null(max_x)) {
      x[is.na(x)] = max_x
    } else {
      if (max(xout,na.rm = TRUE)<max(x,na.rm = TRUE)) {
        x[is.na(x)] = Inf
      } else {
        x[is.na(x)] = max(xout)
        warning("When rebanding missing `x` values are assumed to be less than the maximum of `xout`.\nIf the NA represents a different upper limit than `xout`, the `max_x` parameter must be supplied.",call. = FALSE)
      }
    }
  }

  if(min(xout,na.rm = TRUE) < min(x,na.rm=TRUE) & min(x,na.rm=TRUE) > min_x) {
    x = c(min_x,x)
    y = c(min_y,y)
    # we do not have to assume y0 = 0
  }

  if (any(duplicated(x))) stop("x values must be unique and represent the high end of each y value.")
  y = y[order(x)]
  x = x[order(x)]
  xout = sort(xout)
  # if (max(xout)<max(x)) xout=c(xout,max(x))

  total = sum(y)
  y = y/total
  min_y = ytotal[[1]]/total
  max_y = ytotal[[2]]/total

  cum_y = cumsum(y)
  pred = stats::splinefun(x, cum_y, method = "monoH.FC")
  py = pred(xout)
  if (max(xout,na.rm = TRUE)<max(x,na.rm = TRUE)) {
    xout = c(xout,Inf)
    py = c(py,1)
  }
  pred_y = (py - dplyr::lag(py,default = 0)) * total
  if (labelling == "positive_integer") {
    nms = dplyr::case_when(
      !is.finite(xout) ~ sprintf("%.0f+",dplyr::lag(xout,default = 0)),
      dplyr::lag(xout,default = 0)+1 == xout ~ sprintf("%.0f",xout),
      is.na(dplyr::lag(xout)) ~ sprintf("%.0f%s%.0f",min_x,sep,xout-1),
      TRUE ~ sprintf("%.0f%s%.0f",dplyr::lag(xout,default = 0),sep,xout-1)
    )
  } else if (labelling == "inclusive") {
    nms = dplyr::case_when(
      !is.finite(xout) ~ sprintf(">%.*f",digits, dplyr::lag(xout,default = 0)),
      is.na(dplyr::lag(xout)) ~ sprintf("\u2264%.*f",digits, xout),
      TRUE ~ sprintf("%.*f< & \u2264%.*f",digits,dplyr::lag(xout),digits,xout)
    )
  } else (
    nms = dplyr::case_when(
      !is.finite(xout) ~ sprintf("\u2265%.*f",digits, dplyr::lag(xout,default = 0)),
      is.na(dplyr::lag(xout)) ~ sprintf("<%.*f",digits, xout),
      TRUE ~ sprintf("%.*f\u2264 & <%.*f",digits,dplyr::lag(xout),digits,xout)
    )
  )
  names(pred_y) = nms
  return(pred_y)
}
