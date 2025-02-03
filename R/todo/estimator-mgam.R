## Manchester growth rate ----
# ## Adapted from code
# Copyright (c) 2020 Ian Hall
# See LICENCE for licensing information

# https://m-clark.github.io/generalized-additive-models/technical.html#a-detailed-example
# https://gavinsimpson.github.io/gratia/reference/derivatives.html

# # create a weekday and is.weekend column
# weekdayFromDates = function(df) {
#   checkValid(df,"date")
#   df %>% mutate(
#     weekday = ordered(lubridate::wday(date),levels=1:7, labels=c("sun","mon","tue","wed","thur","fri","sat")),
#     is.weekend = weekday %in% c("sat","sun")
#   )
# }

# Growth rate estimates for confirmed cases in Europe and for different metrics in Italy using GAM
# Figure 1 (main text) and figures S1 and S2 (electronic supplementary material) of:
#
# Pellis L, Scarabel F, Stage HB, Overton CE, Chappell LHK, Fearon E, Bennett E,
# University of Manchester COVID-19 Modelling Group, Lythgoe KA, House TA and Hall I,
# "Challenges in control of COVID-19: short doubling time and long delay to effect of interventions",
# Philosophical Transactions of the Royal Society B (2021)
#
# gamGrowthEstimate = function(simpleTimeseries, meth="GCV.Cp", FE='WD'){
#   if (simpleTimeseries %>% is.grouped_df()) stop("this does not work on grouped data. use a group_modify.")
#
#   simpleTimeseries %>% checkValid(c("date","value"))
#   simpleTimeseries = simpleTimeseries %>%
#     arrange(date) %>%
#     ensureExists("time", orElse = function(ts,...) ts %>% mutate(time = as.integer(date-max(date)))) %>%
#     mutate(.incidence = value)
#
#   #res <- data.frame(sdt=rep(0,npts),sdtup=rep(0,npts),sdtlow=rep(0,npts),doub=rep(0,npts),doubup=rep(0,npts),doublow=rep(0,npts))
#   #Tv <- timev
#
#   if(FE=='None') {
#     MGAM <- mcgv::gam(.incidence ~ mcgv::s(time), data = simpleTimeseries, family=quasipoisson, method=meth)
#   } else {
#     simpleTimeseries = simpleTimeseries %>%
#       ensureExists("weekday", orElse = function(ts,...) ts %>% weekendEffect(valueVar=value)) %>%
#       ensureExists("is.weekend", orElse = function(ts,...) ts %>% weekendEffect(valueVar=value))
#     if(FE=='WE'){
#       MGAM <- mcgv::gam(.incidence ~ mcgv::s(time)+is.weekend, data = simpleTimeseries, family=quasipoisson, method=meth)
#     } else {
#       MGAM <- mcgv::gam(.incidence ~ mcgv::s(time)+weekday, data = simpleTimeseries, family=quasipoisson, method=meth)
#     }
#   }
#
#   X0 <- predict(MGAM, simpleTimeseries %>% mutate(time=time-eps), type="lpmatrix")
#   eps <- 1e-7 ## finite difference interval
#   X1 <- predict(MGAM, simpleTimeseries %>% mutate(time=time+eps),type="lpmatrix")
#   Xp <- (X1-X0)/(2*eps) ## maps coefficients to (fd approx.) derivatives
#   # something to do with extracting the coefficients
#   off <- ifelse(FE=='None',1,ifelse(FE=='WE',2,7))
#   Xi <- Xp*0
#   Xi[,1:9+off] <- Xp[,1:9+off] ## weekend Xi%*%coef(MGAM) = smooth deriv i
#   df <- Xi%*%coef(MGAM)              ## ith smooth derivative
#   df.sd <- rowSums(Xi%*%MGAM$Vp*Xi)^.5 ## cheap diag(Xi%*%b$Vp%*%t(Xi))^.5
#   ## derivative calculation, pers comm S. N. Wood, found in mgcv:  Mixed  GAM  Computation  Vehicle  with  automatic  smoothness  estimation.  R  packageversion 1.8-31 (2019) https://CRAN.R-project.org/package=mgcv.
#
#   simpleTimeseries %>% formatResult(fit = df, se.fit = df.sd,t = function(x) x, estimate = "Growth", modelName = glue::glue("poisson:gam-{meth}:{FE}"), link = "value")
#
# }
