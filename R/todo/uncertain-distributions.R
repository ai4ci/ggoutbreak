#' General distribution fitting algorithms
#' @export
DistributionFit = R6::R6Class("DistributionFit", inherit=PassthroughFilesystemCache, public = list(

  models = NULL,
  censored = NULL,
  groupedDf = NULL,
  grps = NULL,
  fitData = NULL,
  fittedModels = NULL,
  bootstraps = NULL,
  samples = NULL,
  shifted = 0,
  distFits = list(),

  #' @description New distribution fitter
  #' @param distributions - the distributions to fit
  #' @param shifted - a shift to apply to all data
  #' @param ... for compatibility
  #' @return the fitter
  initialize = function(distributions = c("weibull","gamma","lnorm","exp","norm","tnorm"), shifted = 0, ...) {
    super$methods::initialize(...)
    self$setModels(distributions, shifted)
  },

  setModels = function(distributions = c("weibull","gamma","lnorm","exp","norm","tnorm"), shifted = 0) {
    tmp = DistributionFit$standardDistributions
    self$models = tmp[names(tmp) %in% distributions]
    self$shifted = shifted
    invisible(self)
  },

  fromUncensoredData = function(groupedDf, valueExpr = value, truncate = TRUE, bootstraps=100,seed=101,...) {
    dots = rlang::list2(...)
    self$censored = FALSE
    self$groupedDf = groupedDf
    self$grps = grps = groupedDf %>% dplyr::groups()
    valueExpr=rlang::enexpr(valueExpr)
    errors = NULL
    self$fitData = groupedDf %>% dplyr::mutate(value = !!valueExpr) %>% dplyr::mutate(value = value+self$shifted) %>% dplyr::select(!!!grps,value) %>% dplyr::group_by(!!!grps)

    #TODO: cache this?
    out = self$fitData %>% dplyr::group_modify( function(d,g,...) {
      if(any(d$value == 0)) warning("Input contains zero values after shift.")
      if(any(d$value < 0)) warning("Input contains negative values after shift.")
      tmp = data.frame()
      for (m in self$models) {
        #browser()
        values = d %>% dplyr::pull(value)
        if (truncate) {
          if (any(values>m$support[2]|values<m$support[1])) message("Truncating data for ",m$name)
          values = values[values<=m$support[2] & values>=m$support[1]]
        }

        if(m$discrete) {
          values = round(values)
        }

        arg = dots
        arg$data = values
        arg$distr = m$name
        arg$start = m$start
        arg$lower=unlist(m$lower)
        #browser()
        if (length(m$fix.arg)>0) {arg$fix.arg=m$fix.arg}

        dist = suppressWarnings({
          tryCatch(do.call(fitdistrplus::fitdist, args=arg), error = function(e) {errors <<- c(errors,e$message);NULL})
        })

        if (is.null(dist)) {
          message("No fit for ",m$name)
        } else {
          res = self$extractFitted(dist, isCensored = FALSE, bootstraps = bootstraps,seed = seed)
          tmp = tmp %>% dplyr::bind_rows(res$fittedModels)
          # nasty hack coming up....
          bstr = res$bootstraps %>% dplyr::full_join(g, by = character())
          self$bootstraps = self$bootstraps %>% dplyr::bind_rows(bstr)
          self$distFits = c(self$distFits,list(dist))
        }
      }
      return(tmp)
    })
    self$fittedModels = distributionsDefinition(out)
    self$samples = NULL
    warning(unique(errors))
    invisible(self)
  },

  fromCensoredData = function(groupedDf, lowerValueExpr, upperValueExpr, truncate = TRUE, bootstraps = 100, seed=101,...) {
    lowerValueExpr=rlang::enexpr(lowerValueExpr)
    upperValueExpr=rlang::enexpr(upperValueExpr)
    dots = rlang::list2(...)
    errors=NULL

    self$censored = TRUE
    self$groupedDf = groupedDf
    self$grps = grps = groupedDf %>% dplyr::groups()
    self$fitData = groupedDf %>% dplyr::mutate(left = !!lowerValueExpr, right = !!upperValueExpr)  %>% dplyr::mutate(left = left+self$shifted, right=right+self$shifted) %>% dplyr::select(!!!grps,left,right) %>% dplyr::group_by(!!!grps)
    #TODO: cache this?
    out = self$fitData %>% dplyr::group_modify( function(d,g,...) {
      if(any(d$left == 0,na.rm=TRUE)) warning("Input contains zero values after shift.")
      if(any(d$left < 0,na.rm=TRUE)) warning("Input contains negative values after shift.")
      tmp = data.frame()
      for (m in self$models) {
        #browser()
        values = d %>% dplyr::select(left,right)
        oldLen = nrow(values)
        # browser()
        if (truncate) {

          values = values %>%
            dplyr::filter(is.na(left) | (left<=m$support[2] & left>=m$support[1])) %>%
            dplyr::filter(is.na(right) | (right<=m$support[2] & right>=m$support[1]))
          if (oldLen > nrow(values)) message("Truncating data for ",m$name)
        }

        if(m$discrete) {
          values = values %>% dplyr::mutate(
            left = round(left),
            right = round(right),
          )
        }
        arg = dots
        arg$censdata = as.data.frame(values)
        arg$distr = m$name
        arg$start = m$start
        arg$lower=unlist(m$lower)
        #browser()
        if (length(m$fix.arg)>0) {arg$fix.arg=m$fix.arg}

        dist = suppressWarnings({
          tryCatch(do.call(fitdistrplus::fitdistcens, args=arg), error = function(e) {errors <<- c(errors,e$message);NULL})
        })

        #browser()
        if (is.null(dist)) {
          message("No fit for ",m$name)
        } else {
          res = self$extractFitted(dist, isCensored = TRUE, bootstraps = bootstraps, seed=seed)
          tmp = tmp %>% dplyr::bind_rows(res$fittedModels)
          # nasty hack coming up.... change properties of object from inside the group_modify
          bstr = res$bootstraps %>% dplyr::full_join(g, by = character())
          self$bootstraps = self$bootstraps %>% dplyr::bind_rows(bstr)
          self$distFits = c(self$distFits,list(dist))
        }
      }
      return(tmp)
    })

    self$fittedModels = distributionsDefinition(out)
    self$samples = NULL
    warning(unique(errors))
    invisible(self)
  },

  # fromFittedDistributions = function(fittedModelsDf) {
  #   self$fittedModels = distributionsDefinition(fittedModelsDf)
  #   browser()
  #   cols = colnames(self$fittedModels)
  #   cols = cols[!(cols %in% c("dist","param","mean","sd","lower","upper"))]
  #   self$grps = grps = sapply(cols,as.symbol)
  #   #if(identical(self$grps,NULL)) self$grps = list()
  #   self$fittedModels = self$fittedModels %>% dplyr::group_by(!!!grps)
  #   self$createBootstraps(100)
  #   self$generateSamples(100)
  #   self$fitData = self$samples
  #   invisible(self)
  # },

  withSingleDistribution = function(dist, paramDf, bootstraps = 1000, epiestimMode = FALSE, ...) {
    dots = rlang::list2(...)
    currentBS = max(c(self$bootstraps$bootstrapNumber,0))
    distParams = names(DistributionFit$conversionFrom[[dist]])
    # is the distribution fully specified in terms of the native parameters for that distribution?
    if (all(distParams %in% paramDf$param) & !any(is.na(c(paramDf$mean,paramDf$sd)))) {
      # With a fully natively specified distribution we can create a range of samples
      # of the distribution under a set of assumptions about the sampling distributions of those parameters
      # e.g. This would be the sampling distribution of the shape and sampling distribution of the
      # rate of a gamma distribution. The assumption made here is they all follow a truncated normal and are not
      # correlated this is probably not a good assumption in the general case.
      #TODO: in theory with the results from O'Neill (2014) https://doi.org/10.1080/00031305.2014.966589
      # it would be possible if there is information about the mean and sd to
      # estimate the correlation between shape and rate. It may also be possible that sampling from mean and sd
      # actually give you a better answer that sampling from native parameters
      tmp_bootstraps = paramDf %>% dplyr::group_by(param) %>% dplyr::group_modify(function(d,g,..) {
        if(nrow(d) > 1) stop(paste0("param ",g$param," is not unique"))
        print(list(mean = d$mean, sd = d$sd, lower = d$lower, upper = d$upper))
        if (!is.null(d$lower) & !is.null(d$upper) & is.finite(d$lower) & is.finite(d$upper)) {
          return(tibble::tibble(
            bootstrapNumber = (1:bootstraps)+currentBS,
            value = msm::rtnorm(n=bootstraps, mean = d$mean, sd = d$sd, lower = d$lower, upper = d$upper)
          ))
        } else {
          return(tibble::tibble(
            bootstrapNumber = (1:bootstraps)+currentBS,
            value = stats::rnorm(n=bootstraps, mean = d$mean, sd = d$sd)
          ))
        }
      })
      self$bootstraps = self$bootstraps %>% dplyr::bind_rows(tmp_bootstraps %>% dplyr::mutate(dist = dist) %>% dplyr::mutate(...))
      self$fittedModels = self$fittedModels %>% dplyr::bind_rows(paramDf %>% dplyr::mutate(dist = dist) %>% dplyr::mutate(...))
    } else if (all(c("mean","sd") %in% paramDf$param)) {
      # In this case we are defining a distribution in terms of its moments.
      # Creating a set of bootstraps here is done by use moments to infer sampling distribution of mean and
      # standard deviation and sampling in the mean / sd space.
      converted = DistributionFit$convertParameters(dist = dist, paramDf = paramDf, bootstraps = bootstraps, epiestimMode = epiestimMode, ...)
      converted$bootstraps = converted$bootstraps %>% dplyr::mutate(bootstrapNumber = bootstrapNumber+currentBS)
      self$bootstraps = self$bootstraps %>% dplyr::bind_rows(converted$bootstraps %>% dplyr::mutate(...))
      self$fittedModels = self$fittedModels %>% dplyr::bind_rows(converted$fittedModels %>% dplyr::mutate(...))
    } else {
      stop("paramDf does not define mean and sd, or other native distribution parameters")
    }
    if (identical(self$grps,NULL))  self$grps = sapply(names(dots),as.symbol)
    self$censored = FALSE
    invisible(self)

  },

  fromBootstrappedDistributions = function(fittedDistributions, confint=c(0.025,0.975), ...) {
    self$setModels(unique(fittedDistributions$dist))
    cols = colnames(fittedDistributions)
    self$grps = unname(sapply(cols[!(cols %in% c("bootstrapNumber","dist","param","value"))], as.symbol))
    self$bootstraps = fittedDistributions
    self$fittedModels = fittedDistributions %>% dplyr::group_by(!!!self$grps,dist,param) %>% dplyr::summarise(mean=mean(value),sd=stats::sd(value),lower=stats::quantile(value,confint[1]),upper=stats::quantile(value,confint[2])) %>% dplyr::ungroup() %>% dplyr::group_by(!!!self$grps)
    self$censored = FALSE
  },

  fromBootstrappedData = function(bootstrappedDf, valueExpr = value, truncate = TRUE,...) {
    dots = rlang::list2(...)
    valueExpr = rlang::ensym(valueExpr)
    errors = NULL
    grps = bootstrappedDf %>% dplyr::groups()
    self$grps = grps
    self$groupedDf = bootstrappedDf
    #browser()
    self$bootstraps = bootstrappedDf %>% ensurer::ensure_that("bootstrapNumber" %in% colnames(.) ~ "bootstrapDf must have a bootstrapNumber column") %>%
      dplyr::group_by(!!!grps, bootstrapNumber) %>%
      dplyr::group_modify(function(d,g,...) {
        tmpBootstrap = tibble::tibble(dist=NA_character_,param=NA_character_,value=NA_real_) %>% dplyr::filter(!is.na(dist))
        for (m in self$models) {
          values = d %>% dplyr::mutate(x = !!valueExpr) %>% dplyr::pull(x)
          values = values+self$shifted
          if (truncate) {
            #if (any(values>m$support[2]|values<m$support[1])) message("Truncating data for ",m$name)
            # browser()
            values = values[values<=m$support[2] & values>=m$support[1]]

          }
          # browser(expr = m$dist == "lnorm")
          if(m$discrete) {
            values = round(values)
          }

          #control=list(trace=1, REPORT=1)
          arg = dots
          arg$data = values
          arg$distr = m$name
          arg$start = m$start
          arg$lower=unlist(m$lower)
          # browser()
          if (length(m$fix.arg)>0) {arg$fix.arg=m$fix.arg}

          dist2 = suppressWarnings({
            tryCatch(do.call(fitdistrplus::fitdist, args=arg), error = function(e) {errors <<- c(errors,e$message);NULL})
          })

          if (!is.null(dist2)) {
            # build a bootstraps entry
            #browser()
            tmpBootstrap = tmpBootstrap %>% dplyr::bind_rows(
              tibble::tibble(
                dist = dist2$distname,
                param = names(dist2$estimate),
                value = dist2$estimate,
                n = dist2$n,
                loglik = dist2$loglik,
                aic = dist2$aic,
                bic = dist2$bic
              )
            )
            if(length(m$fix.arg)>0) {
              tmpBootstrap = tmpBootstrap %>% dplyr::bind_rows(
                tibble::tibble(
                  dist = dist2$distname,
                  param = names(m$fix.arg),
                  value = unlist(m$fix.arg),
                  n = dist2$n,
                  loglik = dist2$loglik,
                  aic = dist2$aic,
                  bic = dist2$bic
                )
              )
            }
          }
        }
        return(tmpBootstrap)
      }) %>% dplyr::group_by(!!!grps)

    self$fittedModels = self$bootstraps %>% dplyr::group_by(!!!grps,dist,param) %>%
      dplyr::summarise(
        mean = mean(value,na.rm = TRUE),
        sd = stats::sd(value,na.rm = TRUE),
        lower = stats::quantile(value, 0.025),
        upper = stats::quantile(value, 0.975),
        n = sum(n),
        loglik = mean(loglik),
        aic = mean(aic),
        bic = mean(bic)
      ) %>% dplyr::group_by(!!!grps)

    self$bootstraps = self$bootstraps %>% dplyr::select(!!!grps,bootstrapNumber,dist,param,value)

    # self$groupedDf = bootstrappedDf %>% dplyr::group_by(!!!grps) %>% dplyr::summarise(
    #   !!paste0("Mean.",rlang::as_label(valueExpr) := mean(!!valueExpr)))
    self$fitData =  bootstrappedDf %>% dplyr::mutate(
      value = !!valueExpr+self$shifted) %>%
      dplyr::select(!!!grps,value)

    self$samples = bootstrappedDf %>% dplyr::mutate(value = !!valueExpr) %>%
      dplyr::select(!!!grps, bootstrapNumber, value) %>% dplyr::group_by(!!!grps, bootstrapNumber) %>% dplyr::mutate(sampleNumber = dplyr::row_number(), dist="empirical")
    self$censored = FALSE
    if (!identical(errors,NULL)) warning(unique(errors))
    invisible(self)
  },

  fromBootstrappedCensoredData = function(bootstrappedDf, lowerValueExpr=left, upperValueExpr=right, truncate = TRUE,...) {
    dots = rlang::list2(...)
    lowerValueExpr=rlang::enexpr(lowerValueExpr)
    upperValueExpr=rlang::enexpr(upperValueExpr)
    errors = NULL
    grps = bootstrappedDf %>% dplyr::groups()
    self$grps = grps
    self$groupedDf = bootstrappedDf
    #browser()

    self$bootstraps = bootstrappedDf %>% ensurer::ensure_that("bootstrapNumber" %in% colnames(.) ~ "bootstrapDf must have a bootstrapNumber column") %>%
      dplyr::group_by(!!!grps, bootstrapNumber) %>%
      dplyr::group_modify(function(d,g,...) {
        d = d %>%
          dplyr::mutate(left = !!lowerValueExpr, right = !!upperValueExpr) %>%
          dplyr::mutate(left = left+self$shifted, right=right+self$shifted) %>%
          dplyr::select(left,right)

        # if(any(d$left == 0,na.rm=TRUE)) warning("Input contains zero values after shift.")
        # if(any(d$left < 0,na.rm=TRUE)) warning("Input contains negative values after shift.")

        tmpBootstrap = tibble::tibble(dist=character(),param=character(),value=numeric())
        for (m in self$models) {
          values = d %>% dplyr::select(left,right)
          oldLen = nrow(values)
          # browser()
          if (truncate) {

            values = values %>%
              # right side is larger that the lowest bound of support
              dplyr::filter(is.na(right) | (right>=m$support[1])) %>%
              # left side is smaller than the upper bound of support
              dplyr::filter(is.na(left) | (left<=m$support[2])) #%>%
            # truncate left and right to the bounds if smaller / larger
            # dplyr::mutate(
            #   left = left %>% pmax(m$support[1],na.rm = TRUE),
            #   right = right %>% pmin(m$support[2],na.rm = TRUE)
            # )
            # dplyr::filter(is.na(left) | (left<=m$support[2] & left>=m$support[1])) %>%
            # dplyr::filter(is.na(right) | (right<=m$support[2] & right>=m$support[1]))
            # if (oldLen > nrow(values)) message("Truncating data for ",m$name)
          }


          if(m$discrete) {
            values = values %>% dplyr::mutate(
              left = round(left),
              right = round(right),
            )
          }

          arg = dots
          arg$censdata = as.data.frame(values)
          arg$distr = m$name
          arg$start = m$start
          arg$lower=unlist(m$lower)
          # if(m$name == "norm") browser()
          if (length(m$fix.arg)>0) {arg$fix.arg=m$fix.arg}

          dist2 = suppressWarnings({
            tryCatch(do.call(fitdistrplus::fitdistcens, args=arg), error = function(e) {errors <<- c(errors,e$message);NULL})
          })

          if (!is.null(dist2)) {
            # build a bootstraps entry
            #browser()
            tmpBootstrap = tmpBootstrap %>% dplyr::bind_rows(
              tibble::tibble(
                dist = dist2$distname,
                param = names(dist2$estimate),
                value = dist2$estimate,
                n = dist2$n,
                loglik = dist2$loglik,
                aic = dist2$aic,
                bic = dist2$bic
              )
            )
            if(length(m$fix.arg)>0) {
              tmpBootstrap = tmpBootstrap %>% dplyr::bind_rows(
                tibble::tibble(
                  dist = dist2$distname,
                  param = names(m$fix.arg),
                  value = unlist(m$fix.arg),
                  n = dist2$n,
                  loglik = dist2$loglik,
                  aic = dist2$aic,
                  bic = dist2$bic
                )
              )
            }
          }
        }
        return(tmpBootstrap)
      }) %>% dplyr::group_by(!!!grps)

    self$fittedModels = self$bootstraps %>% dplyr::group_by(!!!grps,dist,param) %>%
      dplyr::summarise(
        mean = mean(value,na.rm = TRUE),
        sd = stats::sd(value,na.rm = TRUE),
        lower = stats::quantile(value, 0.025),
        upper = stats::quantile(value, 0.975),
        n = sum(n),
        loglik = mean(loglik),
        aic = mean(aic),
        bic = mean(bic)
      ) %>% dplyr::group_by(!!!grps)

    self$bootstraps = self$bootstraps %>% dplyr::select(!!!grps,bootstrapNumber,dist,param,value)

    # self$groupedDf = bootstrappedDf %>% dplyr::group_by(!!!grps) %>% dplyr::summarise(
    #   !!paste0("Mean.",rlang::as_label(valueExpr) := mean(!!valueExpr)))
    self$fitData =  bootstrappedDf %>% dplyr::mutate(
      left = !!lowerValueExpr+self$shifted,
      right = !!upperValueExpr+self$shifted) %>%
      dplyr::select(!!!grps,left,right)

    self$samples = bootstrappedDf %>% dplyr::mutate(value = stats::runif(1,min=!!lowerValueExpr,max=!!upperValueExpr)) %>%
      dplyr::select(!!!grps, bootstrapNumber, value) %>% dplyr::group_by(!!!grps, bootstrapNumber) %>% dplyr::mutate(sampleNumber = dplyr::row_number(), dist="empirical")
    self$censored = TRUE
    if (!identical(errors,NULL)) warning(unique(errors))
    invisible(self)
  },

  plot = function(xlim, binwidth = 1, summary=FALSE, pts=8, facet2d = TRUE) {

    #TODO: attribute fractions of survivalDf to different times based on left and right censoring.
    # This needs a review of structure fo whole thing
    # c(min(self$fitData$value),max(self$fitData$value))

    #browser()
    support = seq(xlim[1],xlim[2],length.out = 201)
    pdfs = self$calculateDensities(support+self$shifted)

    grps = self$grps




    p1 = ggplot2::ggplot()
    if (!identical(self$fitData,NULL)) {
      if(!self$censored) {
        p1 = p1 + ggplot2::geom_histogram(data=self$fitData, ggplot2::aes(x=value-self$shifted, y=..density..),fill=NA,colour = "grey50",binwidth = binwidth)
      } else {
        p1 = p1 +
          ggplot2::geom_histogram(data=self$fitData, ggplot2::aes(x=left-self$shifted, y=..density..),fill="grey70",colour = NA,alpha=0.5,binwidth = binwidth) +
          ggplot2::geom_histogram(data=self$fitData, ggplot2::aes(x=right-self$shifted, y=..density..),fill="grey70",colour = NA,alpha=0.5,binwidth = binwidth)
      }
    }

    disc = unlist(purrr::map(DistributionFit$standardDistributions, ~ .$discrete))

    cont = pdfs %>% dplyr::filter(dist %in% names(disc)[!disc])
    if(nrow(cont) > 0) {
      p1 = p1 +  ggplot2::geom_line(data = cont, ggplot2::aes(x=value-self$shifted,y=Mean.density,colour = dist), size = 1) +
        ggplot2::geom_ribbon(data = cont, ggplot2::aes(x=value-self$shifted, ymin=Quantile.0.025.density,ymax=Quantile.0.975.density,fill = dist),colour=NA,alpha=0.1)+
        ggplot2::geom_ribbon(data = cont, ggplot2::aes(x=value-self$shifted, ymin=Quantile.0.25.density,ymax=Quantile.0.75.density,fill = dist),colour=NA,alpha=0.15)
    }

    if(summary) {
      labels = self$printDistributionSummary() %>% dplyr::group_by(!!!self$grps) %>% dplyr::summarise(label = paste0(dist," ",param,": ",`Mean (95% CI)`,collapse = "\n"))
      p1 =p1 + ggplot2::geom_label(data=labels, ggplot2::aes(x = Inf, y = Inf, label = label), fill=NA, label.size=0, label.padding = unit(2, "lines"), hjust="inward", vjust="inward", size=pts/ggplot2::.pt/(96/72))
    }

    discont = pdfs %>% dplyr::filter(dist %in% names(disc)[disc])
    if(nrow(discont) > 0) {
      # p1 = p1 +  ggplot2::geom_point(data = discont, ggplot2::aes(x=value-self$shifted,y=Mean.density,colour = dist), size = 1,show.legend = FALSE) +
      #   ggplot2::geom_errorbar(data = discont, ggplot2::aes(x=value-self$shifted, ymin=Quantile.0.025.density,ymax=Quantile.0.975.density,colour = dist,fill=dist))+
      #   ggplot2::geom_errorbar(data = discont, ggplot2::aes(x=value-self$shifted, ymin=Quantile.0.25.density,ymax=Quantile.0.75.density,colour = dist))
      # }

      p1 = p1 +  ggplot2::geom_boxplot(data = discont, ggplot2::aes(x=value-self$shifted,
                                                  y=Mean.density,
                                                  ymin=Quantile.0.025.density,
                                                  lower=Quantile.0.25.density,
                                                  middle=Quantile.0.5.density,
                                                  upper=Quantile.0.75.density,
                                                  ymax=Quantile.0.975.density,
                                                  colour = dist, fill=dist, group=(paste0(dist,round(value-self$shifted)))),stat="identity",width=0.5, alpha = 0.25, show.legend = FALSE)
    }

    p1 = p1 + ggplot2::coord_cartesian(xlim = xlim)

    # https://stackoverflow.com/questions/45908120/add-shaded-standard-error-curves-to-geom-density-in-ggplot2

    grps = self$grps
    if (length(grps) == 2 & facet2d) {
      p1 = p1 + ggplot2::facet_grid(rows = dplyr::vars(!!grps[[1]]), cols = dplyr::vars(!!grps[[2]]), scales="free", shrink = TRUE)
    } else if(length(grps) != 0) {
      p1 = p1 + ggplot2::facet_wrap(facets = grps, scales="free", shrink = TRUE)
    }
    return(p1)
  },

  extractFitted = function(distFit, isCensored, bootstraps = 100, seed=101) {
    if (!is.null(distFit)) {
      set.seed(seed)
      if (isCensored) {
        dist2 = suppressWarnings(fitdistrplus::bootdistcens(distFit,silent = TRUE,niter = bootstraps,ncpus = 3))
        #browser()
      } else {
        dist2 = suppressWarnings(fitdistrplus::bootdist(distFit,silent = TRUE,niter = bootstraps,ncpus = 3))
      }
    }
    #browser()
    m=DistributionFit$standardDistributions[[dist2$fitpart$distname]]
    fittedModels=tibble::tibble(
      n=distFit$n,
      aic=distFit$aic,
      bic=distFit$bic,
      loglik=distFit$loglik,
      dist = dist2$fitpart$distname,
      param = names(distFit$estimate),
      mean = unname(sapply(dist2$estim,mean, na.rm=TRUE)), #dist$estimate,
      sd = unname(sapply(dist2$estim,sd, na.rm=TRUE)), #dist$sd,
      lower = matrix(dist2$CI,ncol=3)[,2],
      upper = matrix(dist2$CI,ncol=3)[,3]
    )
    if(length(m$fix.arg)>0) {
      fittedModels = fittedModels %>% dplyr::bind_rows(
        tibble::tibble(
          n=distFit$n,
          aic=distFit$aic,
          bic=distFit$bic,
          loglik=distFit$loglik,
          dist = dist2$fitpart$distname,
          param = names(m$fix.arg),
          mean = unlist(m$fix.arg),
          sd = 0,
          lower = unlist(m$fix.arg),
          upper = unlist(m$fix.arg)
        )
      )
    }

    return(
      list(
        fittedModels=distributionsDefinition(fittedModels),
        #TODO: a goodness of fit measure here would be reassuring, but how this would integrate with the rest of the
        # code is a bit of a mystery. Problem is that some bootstraps may be a terrible fit but they are treated the same
        # as those that are a good fit. Could I suppose dispose of a certain fraction of bootstraps that are a poor fit...
        bootstraps = dist2$estim %>%
          dplyr::mutate(bootstrapNumber = dplyr::row_number(), dist = dist2$fitpart$distname) %>%
          tidyr::pivot_longer(names_to = "param", values_to = "value", cols=c(-bootstrapNumber,-dist))
      )
    )
  },

  filterModels = function(...) {
    clone = self$clone()
    clone$fittedModels = clone$fittedModels %>% dplyr::group_by(!!!clone$grps) %>% dplyr::filter(...)
    clone$bootstraps = clone$bootstraps %>% dplyr::semi_join(clone$fittedModels, by=unlist(c(sapply(clone$grps,as_label),"dist")))
    if (!identical(clone$samples,NULL))
      clone$samples = clone$samples %>% dplyr::semi_join(clone$fittedModels, by=unlist(c(sapply(clone$grps,as_label),"dist")))
    invisible(clone)
  },

  #' #' @description calculates a set of bootstrap parameter values
  #' #' @param bootstraps - number of bootstrap iterations
  #' #' @return a list of randomly selected bootstraps from the fitted models.
  #' createBootstraps = function(bootstraps=100) {
  #'   grps = self$grps
  #'   distributionsDf = self$fittedModels %>% dplyr::group_by(!!!grps,dist) %>% dplyr::group_modify(function(d,g,...) {
  #'     #if(nrow(d) != 1) stop("A single covolution distribution must be defined for each group")
  #'     #func = paste0("p",d$distribution)
  #'     out = tibble::tibble(
  #'       #!!distributionVar = g %>% dplyr::pull(distributionVar),
  #'       bootstrapNumber = 1:bootstraps
  #'     )
  #'     params = d %>% dplyr::pull(param) %>% unique()
  #'     for (paramName in params) {
  #'       paramMean = d %>% dplyr::filter(param == paramName) %>% dplyr::pull(mean)
  #'       paramSd = d %>% dplyr::filter(param == paramName) %>% dplyr::pull(sd)
  #'       paramMin = d %>% dplyr::filter(param == paramName) %>% dplyr::pull(lower)
  #'       paramMax = d %>% dplyr::filter(param == paramName) %>% dplyr::pull(upper)
  #'       if(is.na(paramSd)) {
  #'         paramBoot = rep(paramMean, bootstraps)
  #'       } else {
  #'         stop("This is not working. It assumes parameter distributions are normal which they aren't")
  #'         # What I'll have to do is create bootstrap sample for all distributions when fitting
  #'         # this will retain the distributions
  #'         # the fittedModels will then be the
  #'         paramBoot = msm::rtnorm(bootstraps, paramMean, paramSd, paramMin, paramMax)
  #'
  #'       }
  #'       out = out %>% tibble::add_column(!!paramName := paramBoot)
  #'     }
  #'     return(out %>% tidyr::pivot_longer(cols = dplyr::all_of(params), names_to = "param", values_to = "value"))
  #'   })
  #'   self$bootstraps = distributionsDf %>% dplyr::group_by(!!!grps, bootstrapNumber)
  #'   invisible(self)
  #' },

  generateSamples = function(sampleExpr = 1000, seed = 101) {
    sampleExpr = rlang::enexpr(sampleExpr)
    if(identical(self$bootstraps, NULL)) self$bootstraps = self$fittedModels %>% dplyr::mutate(value = mean, bootstrapNumber=0)
    grps = self$grps
    set.seed(seed)
    bootstrapSamples = self$bootstraps %>%
      dplyr::mutate(tmp_samples = !!sampleExpr) %>%
      dplyr::group_by(!!!grps,dist,bootstrapNumber,tmp_samples) %>%
      dplyr::group_modify(function(d,g,...) {
        samples = g$tmp_samples
        func = paste0("r",g %>% dplyr::pull(dist))
        params2 = d %>% dplyr::select(param,value) %>% tibble::deframe()
        sampledDist = tibble::tibble(
          value = suppressWarnings(do.call(func, c(list(n=samples),params2))),
          sampleNumber = 1:samples
        )
        return(sampledDist %>% dplyr::filter(!is.na(value)))
      })
    #browser()
    self$samples = bootstrapSamples %>% dplyr::ungroup() %>% dplyr::select(-tmp_samples) %>% dplyr::group_by(!!!grps) %>% dplyr::mutate(value = value-self$shifted)
    invisible(self)
  },

  #' @description calculates a set of pdfs from bootstrapped distributions
  #' @param support - the range of values of X for which to calculate the density.
  #' @return a list of randomly selected bootstraps from the fitted models.
  calculateDensities = function(x, summarise=TRUE) {
    grps = self$grps
    tmp = self$bootstraps %>% dplyr::group_by(!!!grps,dist,bootstrapNumber) %>% dplyr::group_modify(function(d,g,...) {
      func = paste0("d",g %>% dplyr::pull(dist))
      if (DistributionFit$standardDistributions[[g$dist]]$discrete) {
        x = round(min(x)):round(max(x))
      }
      params2 = d %>% dplyr::select(param,value) %>% tibble::deframe()
      pdfs = tibble::tibble(
        density = suppressWarnings(do.call(func, c(list(x=x),params2))),
        value = x
      )
      return(pdfs)
    })
    if (summarise) return(tmp %>% self$sevenNumbers(density))
    else return(tmp)
  },

  #' @description calculates a set of pdfs from bootstrapped distributions
  #' @param support - the range of values of X for which to calculate the density.
  #' @return a list of randomly selected bootstraps from the fitted models.
  calculateCumulativeDistributions = function(q, summarise=TRUE) {
    grps = self$grps
    tmp = self$bootstraps %>% dplyr::group_by(!!!grps,dist,bootstrapNumber) %>% dplyr::group_modify(function(d,g,...) {
      func = paste0("p",g %>% dplyr::pull(dist))
      if (DistributionFit$standardDistributions[[g$dist]]$discrete) {
        q = round(q)
      }
      params2 = d %>% dplyr::select(param,value) %>% tibble::deframe()
      pdfs = tibble::tibble(
        cumulative = suppressWarnings(do.call(func, c(list(q=q),params2))),
        value = q
      )
      return(pdfs)
    })
    if (summarise) return(tmp %>% self$sevenNumbers(cumulative))
    else return(tmp)
  },

  #' @description calculates a set of quantiles from bootstrapped distributions
  #' @param support - the range of values of X for which to calculate the density.
  #' @return a list of randomly selected bootstraps from the fitted models.
  calculateQuantiles = function(p, summarise=TRUE) {
    grps = self$grps
    tmp = self$bootstraps %>% dplyr::group_by(!!!grps,dist,bootstrapNumber) %>% dplyr::group_modify(function(d,g,...) {
      func = paste0("q",g %>% dplyr::pull(dist))
      params2 = d %>% dplyr::select(param,value) %>% tibble::deframe()
      pdfs = tibble::tibble(
        quantile = suppressWarnings(do.call(func, c(list(p=p),params2))),
        probability = p
      )
      return(pdfs)
    })
    if (summarise) return(tmp %>% self$sevenNumbers(quantile))
    else return(tmp)
  },

  discreteProbabilities = function(q, summarise=TRUE, truncate=FALSE) {
    tmp = self$calculateCumulativeDistributions(q, summarise=FALSE) %>% dplyr::ungroup() %>%
      dplyr::group_by(across(c(-value,-cumulative))) %>%
      dplyr::arrange(value) %>%
      dplyr::mutate(discreteProbability = cumulative-dplyr::lag(cumulative, default = 0)) %>%
      dplyr::select(-cumulative)
    if (truncate) {
      tmp = tmp %>%
        dplyr::group_by(across(c(-value,-discreteProbability))) %>%
        dplyr::mutate(discreteProbability = discreteProbability/sum(discreteProbability))
    }
    if (summarise) return(tmp %>% self$sevenNumbers(discreteProbability))
    else return(tmp)
  },

  discreteSurvival = function(q, summarise=TRUE) {
    tmp = self$calculateCumulativeDistributions(q, summarise=FALSE) %>% dplyr::ungroup() %>%
      dplyr::group_by(across(c(-value,-cumulative))) %>%
      dplyr::arrange(value) %>%
      dplyr::mutate(survival = 1-cumulative) %>%
      dplyr::select(-cumulative)
    if (summarise) return(tmp %>% self$sevenNumbers(survival))
    else return(tmp)
  },

  sampledProbabilities = function(q, summarise=TRUE) {
    #TODO: offset? shift?
    if(identical(self$samples, NULL)) self$generateSamples()
    self$samples %>% dplyr::mutate(
      band = cut(value,breaks = c(-Inf,q),labels = q,ordered_result = TRUE)
    ) %>% dplyr::mutate(
      value = q[as.integer(band)]
    ) %>% dplyr::group_by(!!!self$grps,value) %>% dplyr::summarise(
      sampledProbability = dplyr::n()/nrow(self$samples)
    ) %>% tidyr::complete(value,sampledProbabilities)
  },

  sevenNumbers = function(groupedDf, colVar) {
    grps = self$grps
    colVar = rlang::ensym(colVar)
    name = function(pref) paste0(pref,".",rlang::as_label(colVar))
    groupedDf %>% dplyr::ungroup() %>% dplyr::group_by(across(c(-bootstrapNumber,-(!!colVar)))) %>% dplyr::summarise(
      !!name("Mean") := mean(!!colVar ,na.rm=TRUE),
      !!name("Sd") := stats::sd(!!colVar ,na.rm=TRUE),
      !!name("Quantile.0.025") := stats::quantile(!!colVar, probs = 0.025, na.rm=TRUE),
      !!name("Quantile.0.25") := stats::quantile(!!colVar, probs = 0.25, na.rm=TRUE),
      !!name("Quantile.0.5") := stats::quantile(!!colVar, probs = 0.5, na.rm=TRUE),
      !!name("Quantile.0.75") := stats::quantile(!!colVar, probs = 0.75, na.rm=TRUE),
      !!name("Quantile.0.975") := stats::quantile(!!colVar, probs = 0.975, na.rm=TRUE),
    )
    #TODO: could make this configurable with purrr nested list
    #levels = c(0.025,0.25,0.5,0.75,0.975)
    #quan = stats::quantile(sample, levels) %>% tibble::enframe() %>% dplyr::mutate(name = paste0("Quantile."),levels) %>% tidyr::pivot_wider(names_from="name", values_from="value")
    #followed by an unnest or flatten
  },

  printDistributionSummary = function(confint = c(0.025,0.975)) {
    grps = self$grps
    tmp = DistributionFit$unconvertParameters(bootstrapsDf = self$bootstraps %>% dplyr::group_by(!!!grps), confint=confint)
    ci = floor((confint[2]-confint[1])*100)
    #TODO: we don't technically know if the self$fittedModels parameters have the same confidence intervals.
    # could recalculate them from self$bootstraps
    tmp = tmp %>% dplyr::mutate( #self$fittedModels %>% dplyr::bind_rows(tmp) %>% dplyr::mutate(
      `Distribution` = purrr::map_chr(dist, ~ self$models[[.x]][["print"]]),
      shift = self$shifted,
      #!!(sprintf("Mean \U00B1 SD (%1d%% CI)",ci)) := sprintf("%1.2f \U00B1 %1.2f (%1.2f; %1.2f)",mean,sd,lower,upper)
      !!(sprintf("Mean (%1d%% CI)",ci)) := sprintf("%1.2f (%1.2f; %1.2f)",mean,lower,upper)
    ) %>% dplyr::mutate(
      `Distribution` = ifelse(shift>0, paste0(`Distribution`, "(shifted ",shift,")"), `Distribution`)
    )
    return(tmp %>% dplyr::group_by(!!!grps))
  },

  printDistributionDetail = function(confint = c(0.025,0.975)) {
    grps = self$grps
    tmp = DistributionFit$unconvertParameters(bootstrapsDf = self$bootstraps %>% dplyr::group_by(!!!grps), confint=confint)
    ci = floor((confint[2]-confint[1])*100)
    #TODO: we don't technically know if the self$fittedModels parameters have the same confidence intervals.
    # could recalculate them from self$bootstraps
    tmp = self$fittedModels %>% dplyr::bind_rows(tmp) %>% dplyr::mutate(
      `Distribution` = purrr::map_chr(dist, ~ self$models[[.x]][["print"]]),
      shift = self$shifted,
      !!(sprintf("Mean \U00B1 SD (%1d%% CI)",ci)) := sprintf("%1.2f \U00B1 %1.2f (%1.2f; %1.2f)",mean,sd,lower,upper)
    ) %>% dplyr::mutate(
      `Distribution` = ifelse(shift>0, paste0(`Distribution`, "(shifted ",shift,")"), `Distribution`)
    ) %>% dplyr::group_by(dist,.add=TRUE)

    if ("aic" %in% names(tmp)) tmp = tmp %>% dplyr::mutate(aic= dplyr::first(stats::na.omit(aic)))
    if ("n" %in% names(tmp)) tmp = tmp %>% dplyr::mutate(n = dplyr::first(stats::na.omit(n)))
    if ("bic" %in% names(tmp)) tmp = tmp %>% dplyr::mutate(bic= dplyr::first(stats::na.omit(bic)))
    if ("loglik" %in% names(tmp)) tmp = tmp %>% dplyr::mutate(loglik= dplyr::first(stats::na.omit(loglik)))
    return(tmp %>% dplyr::group_by(!!!grps))
  },

  #' @description applies a set of parameterised convolution functions groupwise to input data
  #'

  #' @param groupedDf - an optionally grouped dataframe, containing at dateVar, and a valueVar to be colvolved
  #' @param distributionsDf - a dataframe containing the same grouping columns as groupedDf plus "distribution", and relevant distribution parameter columns
  #' @param dateVar -
  #' @param days -
  #' @param timepoints - the times to
  #' @param padLeft - what can we assume about the run in to the current values? default NA.
  #' @return a list of matrices

  tsParameterizedConvolution = function(groupedDf, distributionsDf, outputVar = "output", valueVar = "value", dateVar="date", distributionVar = "dist", paramNameVar = "param", paramValueVar = "paramValue", days = 30, timepoints = 0:days, padLeft = NA_real_, padRight = NA_real_) {
    grps = groupedDf %>% dplyr::groups()
    distributionVar = rlang::ensym(distributionVar)
    paramNameVar = rlang::ensym(paramNameVar)
    paramValueVar = rlang::ensym(paramValueVar)

    discreteDistDf = distributionsDf %>% dplyr::group_by(!!!grps,!!distributionVar) %>% dplyr::group_modify(function(d,g,...) {
      #if(nrow(d) != 1) stop("A single covolution distribution must be defined for each group in groupedDf")
      func = paste0("p",g %>% dplyr::pull(!!distributionVar))

      params2 = paramList = d %>% dplyr::select(!!paramNameVar,!!paramValueVar) %>% tibble::deframe()

      #funcCDF = paste0("p",d$distribution)
      #params = formals(func)
      #params2 = d %>% dplyr::select(dplyr::any_of(names(params))) %>% as.list()
      start = timepoints[1:(length(timepoints)-1)]
      end = timepoints[2:(length(timepoints))]
      discreteDist = tibble::tibble(
        start = start,
        end = end,
        prob = do.call(func, c(q=list(end),params2))-do.call(func, c(q=list(start),params2)),
        surv = 1-do.call(func, c(q=list(start),params2))
      )
      return(discreteDist)
    })

    tsDiscreteCovolution(groupedDf, discreteDistDf, {{outputVar}}, {{valueVar}},{{dateVar}},padLeft, padRight)

  },

  #' @description applies a set of parameterised convolution functions groupwise to input data
  #'

  #' @param groupedDf - an optionally grouped dataframe, containing at dateVar, and a valueVar to be colvolved
  #' @param discreteDistDf - a dataframe containing the same grouping columns as groupedDf plus "start", "end" and "prob" columns
  #' @param dateVar -
  #' @param padLeft - what can we assume about the run in to the current values? default NA.
  #' @param padRight - what can we assume about the run in to the current values? default NA.
  #' @return a list of matrices

  tsDiscreteConvolution = function(groupedDf, discreteDistDf, outputVar = "output", valueVar="value", dateVar="date", pExpr="prob", padLeft=NA_real_, padRight=NA_real_ ) {
    grps = groupedDf %>% dplyr::groups()
    valueVar = rlang::ensym(valueVar)
    dateVar = rlang::ensym(dateVar)
    outputVar = rlang::ensym(outputVar)
    pExpr = rlang::ensym(pExpr)
    joinCols = c(sapply(grps, as_label),"tmp_join")
    finalJoinCols = c(sapply(grps, as_label),rlang::as_label(dateVar))

    dateRanges = groupedDf %>% dplyr::summarise(min_date = min(!!dateVar), max_date = max(!!dateVar)) %>% dplyr::mutate(tmp_join=1)

    combinations = discreteDistDf %>% dplyr::select(!!!grps,start) %>% dplyr::mutate(tmp_join=1) %>% dplyr::inner_join(dateRanges, by=joinCols)

    padLeftDf = combinations %>% dplyr::mutate(tmp_value=padLeft, tmp_date=min_date-start-1) %>% dplyr::select(-start)
    padRightDf = combinations %>% dplyr::mutate(tmp_value=padRight, tmp_date=max_date+start+1) %>% dplyr::select(-start)

    tmp = groupedDf %>% dplyr::select(!!!grps,tmp_value=!!valueVar, tmp_date=!!dateVar) %>% dplyr::group_by(!!!grps) %>%
      dplyr::mutate(tmp_join=1, min_date = min(tmp_date), max_date = max(tmp_date))

    tmp = dplyr::bind_rows(padLeftDf,tmp,padRightDf)

    tmp2 = tmp %>%
      dplyr::inner_join(discreteDistDf %>% dplyr::mutate(tmp_join=1), by=joinCols)

    tmp3 = tmp2 %>% dplyr::mutate(eff_date = tmp_date+start, eff_value = tmp_value*!!pExpr) %>% dplyr::select(-tmp_join) %>%
      dplyr::filter(eff_date <= max_date & eff_date >= min_date)

    tmp4 = tmp3 %>% dplyr::group_by(!!!grps,eff_date) %>% dplyr::summarise(!!outputVar := sum(eff_value)) %>% dplyr::mutate(!!dateVar := as.Date(eff_date,"1970-01-01")) %>% dplyr::select(-eff_date)

    return(groupedDf %>% dplyr::inner_join(tmp4, by=finalJoinCols))
  },




  #' @description executes a convolution using bootstrapped parameterized distributions
  #'

  #' @param distributionDistDf - a grouped data frame containing the same columns as those grouped in groupedDf, plus "distribution" and  columns for "<parameter>_mean","<parameter>_sd","<parameter>_min","<parameter>_max" e.g. shape_mean, shape_sd, shape_min, shape_max, rate_mean, rate_sd, rate_min, rate_max
  #' @param bootstraps - number of bootstrap iterations
  #' @return the full bootstrap result (i.e. not summarized) which can be futher convoluted (asd long as the number of bootstrap iterations are kept the same).

  tsBootstrapConvolution = function(groupedDf, distributionDistDf, bootstraps=100, outputVar = "output", valueVar="value", dateVar="date", days = 30, timepoints = 0:days, padLeft=NA_real_, padRight=NA_real_) {
    grps = groupedDf %>% dplyr::groups()
    tmp = groupedDf %>% tidyr::crossing(tibble::tibble(bootstrapNumber = 1:bootstraps)) %>% dplyr::group_by(!!!grps,bootstrapNumber)
    distributionDistDf = distributionDistDf %>% dplyr::group_by(!!!grps)
    out = tmp %>% tsParameterizedConvolution(bootstrapDistributions(distributionDistDf, bootstraps = bootstraps),
                                             outputVar = {{outputVar}}, valueVar = {{valueVar}}, dateVar = {{dateVar}}, days = days, timepoints = timepoints, padLeft = padLeft, padRight = padRight)
    out = out %>% dplyr::ungroup() %>% dplyr::group_by(!!!grps)
    return(out)
  },

  #' @description summarise the result of a bootstrapped convolution using parameterized distributions
  #'

  #' @param distributionDistDf - a grouped data frame containing the same columns as those grouped in groupedDf, plus "distribution" and  columns for "<parameter>_mean","<parameter>_sd","<parameter>_min","<parameter>_max" e.g. shape_mean, shape_sd, shape_min, shape_max, rate_mean, rate_sd, rate_min, rate_max
  #' @param bootstraps - number of bootstrap iterations
  #' @return the full bootstrap result (i.e. not summarized) which can be futher convoluted (asd long as the number of bootstrap iterations are kept the same).

  summariseBootstrap = function(groupedDf, outputVars = dplyr::vars(output), dateVar = "date") {
    grps = groupedDf %>% dplyr::groups()
    dateVar = rlang::ensym(dateVar)
    joinCols = c(sapply(grps,as_label),rlang::as_label(dateVar))
    out = groupedDf %>% dplyr::select(!!!grps,!!dateVar) %>% dplyr::distinct()
    p <- c(0.2, 0.5, 0.8)
    for(outputVar in outputVars) {

      tmp = groupedDf %>% dplyr::group_by(!!!grps,!!dateVar) %>% dplyr::summarise(
        !!(paste0("CI.0.025(",rlang::as_label(outputVar),")")) := stats::quantile(!!outputVar,p=0.025),
        !!(paste0("CI.0.1(",rlang::as_label(outputVar),")")) := stats::quantile(!!outputVar,p=0.1),
        !!(paste0("Median(",rlang::as_label(outputVar),")")) := stats::quantile(!!outputVar,p=0.5),
        !!(paste0("CI.0.9(",rlang::as_label(outputVar),")")) := stats::quantile(!!outputVar,p=0.9),
        !!(paste0("CI.0.975(",rlang::as_label(outputVar),")")) := stats::quantile(!!outputVar,p=0.975),
      )

      out = out %>% dplyr::inner_join(tmp, by=joinCols)
    }
    return(out)
  }
))

#### Standard distributions ----

DistributionFit$standardDistributions = list(
  weibull = list(name = "weibull", print = "weibull", start = list(shape =1 , scale=10), fix.arg = list(), lower=list(shape=0,scale=0), support=c(0,Inf), discrete=FALSE),
  gamma = list(name = "gamma", print = "gamma", start = list(shape =1, scale=10), fix.arg = list(), lower=list(shape=0,scale=0), support=c(0,Inf), discrete=FALSE),
  gamma2 = list(name = "gamma", print = "gamma", start = list(shape =1, rate=0.1), fix.arg = list(), lower=list(shape=0,rate=0), support=c(0,Inf), discrete=FALSE),
  lnorm = list(name = "lnorm", print = "log-normal", start = list(meanlog=1 , sdlog=1), fix.arg = list(), lower=list(meanlog=-Inf, sdlog=-Inf), support=c(0,Inf), discrete=FALSE),
  exp = list(name = "exp", print = "exponential", start = list(rate=0.5), fix.arg = list(), lower=list(rate=.Machine$double.xmin), support=c(.Machine$double.xmin, Inf), discrete=FALSE),
  norm = list(name = "norm", print = "normal", start = list(mean = 1, sd = 0.1), fix.arg = list(), lower=list(sd=0), support=c(-Inf,Inf), discrete=FALSE),
  tnorm = list(name = "tnorm", print = "truncated normal", start = list(mean = 1, sd = 0.1), fix.arg = list(lower=0,upper=Inf), support=c(0,Inf), lower=list(mean=0,sd=0.001), discrete=FALSE),
  nbinom = list(name = "nbinom", print = "negative binomial", start = list(size = 1, prob = 0.1), fix.arg = list(), lower=list(size=.Machine$double.xmin, prob=0), support=c(0,Inf), discrete=TRUE),
  pois = list(name = "pois", print = "poisson", start = list(lambda = 1), fix.arg = list(), lower=list(lambda=0), support=c(0,Inf), discrete=TRUE)
)

DistributionFit$conversionFrom = list(
  gamma = list(
    shape = function(mean,sd) mean^2/sd^2,
    scale = function(mean,sd) sd^2/mean
  ),
  gamma2 = list(
    shape = function(mean,sd) mean^2/sd^2,
    rate = function(mean,sd) mean/sd^2
  ),
  lnorm = list(
    meanlog = function(mean,sd) log(mean/sqrt(1+(sd^2)/(mean^2))),
    sdlog = function(mean,sd) sqrt(log(1+(sd^2)/(mean^2)))
  ),
  norm = list(
    mean = function(mean,sd) mean,
    sd = function(mean,sd) sd
  ),
  tnorm = list(
    mean = function(mean,sd) mean,
    sd = function(mean,sd) sd,
    lower = function(mean,sd) 0,
    upper = function(mean,sd) Inf
  ),
  weibull = list(
    shape = function(mean,sd) (sd/mean)^-1.086,
    scale = function(mean,sd) mean/gamma(1+1/((sd/mean)^-1.086))
  ),
  exp = list(
    rate = function(mean,sd) 1/mean
  ),
  nbinom = list(
    size = function(mean,sd) (mean^2)/(sd^2-mean),
    prob = function(mean,sd) 1-mean/(sd^2)
  ),
  pois = list(
    lambda = function(mean,sd) mean
  )
)

DistributionFit$conversionTo = list(
  gamma = list(
    mean = function(shape,scale = 1/rate, rate=1/scale) shape*scale,
    sd = function(shape,scale = 1/rate, rate=1/scale) sqrt(shape*(scale^2))
  ),
  gamma2 = list(
    mean = function(shape,scale = 1/rate, rate=1/scale) shape*scale,
    sd = function(shape,scale = 1/rate, rate=1/scale) sqrt(shape*(scale^2))
  ),
  lnorm = list(
    mean = function(meanlog,sdlog) exp(meanlog+(sdlog^2)/2),
    sd = function(meanlog,sdlog) sqrt((exp(sdlog^2)-1)*exp(2*meanlog+sdlog^2))
  ),
  norm = list(
    mean = function(mean,sd) mean,
    sd = function(mean,sd) sd
  ),
  tnorm = list(
    mean = function(mean,sd,lower,upper) mean,
    sd = function(mean,sd,lower,upper) sd
  ),
  weibull = list(
    mean = function(scale, shape) scale*gamma(1+1/shape),
    sd = function(scale, shape) sqrt(scale^2 * (gamma(1+2/shape) - (gamma(1+1/shape))^2))
  ),
  exp = list(
    mean = function(rate) 1/rate,
    sd = function(rate) 1/(rate^2)
  ),
  nbinom = list(
    mean = function(size,prob) size*(1-prob)/prob,
    sd = function(size,prob) sqrt(size*(1-prob)/(prob^2))
  ),
  pois = list(
    mean = function(lambda) lambda,
    sd = function(lambda) lambda
  )
)


DistributionFit$unconvertParameters = function(
    bootstrapsDf,
    confint = c(0.025,0.975),
    ...
) {
  grps = bootstrapsDf %>% dplyr::groups()

  tmp = bootstrapsDf %>% dplyr::group_by(!!!grps,dist) %>% dplyr::group_modify(function(d,g,...) {
    paramList = d %>% dplyr::select(param,value) %>% tidyr::nest(data=c(value)) %>% tibble::deframe() %>% purrr::map(~ .$value)
    funcs = DistributionFit$conversionTo[[g$dist]]
    conv = tibble::tibble(
      param = "mean",
      value = suppressWarnings(do.call(funcs$mean, paramList)),
    ) %>% dplyr::bind_rows(tibble::tibble(
      param = "sd",
      value = suppressWarnings(do.call(funcs$sd, paramList)),
    ))
  })
  tmp = tmp %>% dplyr::group_by(!!!grps,dist, param) %>% dplyr::summarise(
    lower = stats::quantile(value, probs = confint[1],na.rm = TRUE),
    upper = stats::quantile(value, probs = confint[2],na.rm = TRUE),
    mean = mean(value,na.rm = TRUE),
    sd = stats::sd(value,na.rm = TRUE)
  )
  return(tmp)
}

DistributionFit$convertParameters = function(
    paramDf,
    N = NA_integer_,
    dist="gamma",
    confint = c(0.025,0.975),
    bootstraps = 1000,
    epiestimMode = FALSE,
    ...
) {

  meanOfMean = parameterDefinition(paramDf) %>% dplyr::filter(param=="mean") %>% dplyr::pull(mean)
  sdOfMean = parameterDefinition(paramDf) %>% dplyr::filter(param=="mean") %>% dplyr::pull(sd)
  lowerOfMean = parameterDefinition(paramDf) %>% dplyr::filter(param=="mean") %>% dplyr::pull(lower)
  upperOfMean = parameterDefinition(paramDf) %>% dplyr::filter(param=="mean") %>% dplyr::pull(upper)
  meanOfSd = parameterDefinition(paramDf) %>% dplyr::filter(param=="sd") %>% dplyr::pull(mean)
  sdOfSd = parameterDefinition(paramDf) %>% dplyr::filter(param=="sd") %>% dplyr::pull(sd)
  lowerOfSd = parameterDefinition(paramDf) %>% dplyr::filter(param=="sd") %>% dplyr::pull(lower)
  upperOfSd = parameterDefinition(paramDf) %>% dplyr::filter(param=="sd") %>% dplyr::pull(upper)

  invalid = function(x) {any(is.null(x) | any(is.na(x)))}

  if (invalid(c(meanOfMean,meanOfSd))) stop("must supply non NA values for meanOfMean and meanOfSd")
  conversion = DistributionFit$conversionFrom[[dist]]
  paramSamples = NULL

  # Mean of mean is implicitly normally distributed. This follows from central limit theorem
  # we need to make sure the sdOfMean is defined.
  if (invalid(sdOfMean)) {
    if (invalid(c(upperOfMean,lowerOfMean))) {
      warning("There is no sd of the mean, or quantiles defined. The fitted distributions will all have the same mean")
      sdOfMean = 0
    } else {
      sdOfMean = (upperOfMean-lowerOfMean)/(stats::qnorm(confint[2],0,1)-stats::qnorm(confint[1],0,1))
    }
  }

  # sdOfMean must be defined. Make sure that upper and lower of mean are also defined:
  if (invalid(lowerOfMean)) lowerOfMean = stats::qnorm(confint[[1]], meanOfMean, sdOfMean)
  if (invalid(upperOfMean)) upperOfMean = stats::qnorm(confint[[2]], meanOfMean, sdOfMean)

  # all parameters for mean are defined

  # turning attention to SD:
  if (invalid(sdOfSd) & invalid(c(lowerOfSd,upperOfSd)) & is.na(N)) {
    warning("There is no sd of the sd, or quantiles, or sample size defined. The fitted distributions will all have the same sd")
    sdOfSd = 0
  } else {

    # as per vignette on distribution sampling
    # we are using a nakagami with a fixed scale parameter
    # define the nakagami scale parameter
    sdScale = meanOfSd^2

    # The sd of a nakagmai is defined as
    # in https://en.wikipedia.org/wiki/Nakagami_distribution
    # in this m = sdShape and omega = sdScale
    nakaSd = function(shape,scale) sqrt(
      scale * (
        1-1/shape*exp( (lgamma(shape+0.5)-lgamma(shape))*2 )
      )
    )
    if (!invalid(sdOfSd)) {
      # we need to infer shape parameters for nakagmai distribution from moments
      # we actually have more information that we need to do this as scale is fixed
      # however the expression is a bit of a pig and needs to be solved numerically
      # browser()
      fn = function(tmp_shape) nakaSd(tmp_shape, sdScale)-sdOfSd
      root = stats::uniroot(f = fn, lower = 0.5, upper = 10000, extendInt="yes", check.conv=FALSE)
      sdShape = root$root
    } else {
      if (invalid(c(lowerOfSd, upperOfSd))) {
        # If we know N then it is possible to infer a bounding SD based on an underlying
        # normal distribution. This is essentially using the sampling variance of the variance
        # being a ChiSquared with N-1 degrees of freedom.
        # but generalising this to a gamma and then the SD to a nakagami.
        sdShape = (N-1)/2
      } else {
        # we are fitting here a nakagami distribution (see resampling vignette)
        # the nakagami omega (scale) parameter is the 1/meanOfSd^2
        fn = function(x,tmp_shape) nakagami::qnaka(x,shape=tmp_shape, scale=sdScale)
        fit.sd.naka = suppressWarnings(stats::nls(y ~ fn(x,tmp_shape), data = tibble::tibble( x=confint, y=c(lowerOfSd, upperOfSd) ), start=list(tmp_shape=1), lower=0.5 ))
        sdShape = summary(fit.sd.naka)$parameters[["tmp_shape",1]]
      }
      # with scale and shape derived we can define the sdOfSd
      sdOfSd = nakaSd(sdShape,sdScale)
    }

    if (invalid(lowerOfSd)) lowerOfSd = nakagami::qnaka(confint[[1]], sdShape, sdScale)
    if (invalid(upperOfSd)) upperOfSd = nakagami::qnaka(confint[[2]], sdShape, sdScale)
  }

  # all parameters for SD are defined

  # Sample thre distributions mean and sd
  #TODO: this assumes independence between the mean and SD
  # which is not true for distributions with a high degree of kurtosis
  # the O'Neill (2014) reference has some expressions for correlation of mean and sd
  # based on skewness and kurtosis (results 1 and 12) however these correlations are between mean
  # and variance which might require the whole of this to be re-written in terms of variance
  # which is limited by the fact that we do not have that information from source studies
  if ((invalid(sdOfMean) | sdOfMean == 0) & (invalid(sdOfSd) | sdOfSd == 0)) {

    # If the mean and sd are exactly specified and have no variability we can only estimate
    # one bootstrap (or at least further bootstraps will be the same.
    if (bootstraps>1) warning("more than one bootstraps requested but distribution has no variability defined. One one bootstrap will be returned.")
    bootstraps = 1
    boot_mean = meanOfMean
    boot_sd = meanOfSd

  } else {

    if (invalid(sdOfMean)) stop("either sdOfMean or upperOfMean and lowerOfMean must be specified")
    if (invalid(sdOfSd)) stop("either sdOfSd or upperOfSd and lowerOfSd or must be specified")

    # central limit - means are normally distributed
    if (sdOfMean > 0) {
      boot_mean = msm::rtnorm(bootstraps,mean = meanOfMean,sd = sdOfMean,lower = lowerOfMean, upper=upperOfMean)
    } else {
      # no variability defined
      boot_mean = rep(meanOfMean,bootstraps)
    }

    # sds are nakagmai distributed
    if(sdOfSd > 0) {
      if (epiestimMode) {
        # epiestim does something weird here officially
        # unofficially I'm not sure it even does this.
        boot_sd = msm::rtnorm(bootstraps,mean = meanOfSd,sd = sdOfSd,lower = lowerOfSd, upper=upperOfSd)
      } else {
        # the nakagami distribution for sd should be fully defined by this point
        boot_sd = nakagami::rnaka(bootstraps,shape = sdShape, scale=sdScale)
        boot_sd = boot_sd %>% scales::squish(range = c(lowerOfSd,upperOfSd))
      }
    } else {
      # no variability defined
      boot_sd = rep(meanOfSd,bootstraps)
    }
  }

  # browser()
  # having samples in the mean SD space we need to augment this with native parameters
  for(paramName in names(conversion)) {
    paramSamples = paramSamples %>% dplyr::bind_rows(tibble::tibble(
      dist = dist,
      bootstrapNumber = 1:bootstraps,
      param = paramName,
      value = conversion[[paramName]](boot_mean, boot_sd)
    ))
  }
  #TODO: N.b. maybe paramSamples is the bootstrappedDistributions as generated by createBootstraps
  paramDfNew = paramSamples %>% dplyr::group_by(dist,param) %>% dplyr::summarise(
    n = N,
    mean = mean(value, na.rm = TRUE),
    sd = stats::sd(value, na.rm = TRUE),
    lower = stats::quantile(value,confint[1], na.rm = TRUE),
    upper = stats::quantile(value,confint[2], na.rm = TRUE)
  )
  return(list(
    bootstraps = paramSamples,
    fittedModels = paramDfNew
  ))
}

#TODO: reversing thsi is a question of grabbing bootstraps and converting them back into mean and sd
# paramList = dfit$bootstraps %>% dplyr::select(param,value) %>% tidyr::nest(value) %>% tibble::deframe() %>% dplyr::map(~ .$value)
# gives you a named list of vectors
# funcs = DistributionFit$conversionTo[["dist"]]
# mean = suppressWarnings(do.call(funcs$mean, paramList)),
# sd = suppressWarnings(do.call(funcs$sd, paramList)),
# which can be used to generate mean and sd
