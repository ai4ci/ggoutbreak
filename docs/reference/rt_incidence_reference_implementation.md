# Reference implementation of the Rt from modelled incidence algorithm

This function estimates the reproduction number for a specific point in
time given a time series of log-normally distributed incidence
estimates, a set of infectivity profiles. This version of the algorithm
only works for a single time point and is not optimised for running over
a whole time series. For that please see
[`rt_incidence_timeseries_implementation()`](https://ai4ci.github.io/ggoutbreak/reference/rt_incidence_timeseries_implementation.md).

## Usage

``` r
rt_incidence_reference_implementation(
  mu_t,
  vcov_ij = diag(sigma_t^2),
  omega,
  sigma_t = NULL,
  tau_offset = 0
)
```

## Arguments

- mu_t:

  a vector of meanlog parameters of a lognormal distribution of modelled
  incidence. This is a vector of length `k`.

- vcov_ij:

  a log scale variance-covariance matrix of predictions. It is optional
  but if not given and `sigma_t` is given then this will be inferred
  assuming independence between estimates. This should be a matrix with
  dimensions `k * k`

- omega:

  a matrix (or vector) representing the infectivity profile as a
  discrete time probability distribution. This must be a `k * n` matrix
  or a vector of length `k`. Each of the `n` columns is a individual
  estimate of the infectivity profile (N.B. this is the same format as
  `EpiEstim`). In `EpiEstim` the first row of this matrix must be zero
  and represents a delay of zero. This constraint does not apply here.

- sigma_t:

  if `vcov_ij` is not given then this must be supplied as the sdlog of
  the log normal incidence estimate

- tau_offset:

  In most cases the infectivity profile has support for delays from
  `0:(k-1)` and the Rt estimate is made at time `(k-1)`. If there is a
  negative component of a serial interval being used as a proxy then the
  support will be `-tau_offset:(k-tau_offset-1)`.

## Value

a list with the following items:

- time_Rt: the time point of the Rt esitmate (usually `k-1`)

- mean_Rt_star: the mean of the Rt estimate

- var_Rt_star: the variance of the Rt estimate

- meanlog_Rt_star: log normal parameter for approximate distribution of
  Rt

- sdlog_Rt_star: log normal parameter for approximate distribution of Rt

- mu_Rt_mix: a vector of log normal parameters for more exact mixture
  distribution of Rt

- sigma_Rt_mix: a vector of log normal parameters for more exact mixture
  distribution of Rt

- quantile_Rt_fn: A log-normal mixture quantile function for Rt

Quantiles can either be obtained from the quantile function or from e.g.
`qlnorm(p, meanlog_Rt_star, sdlog_Rt_star)`

## Details

N.B. the description of this algorithm is given here:
https://ai4ci.github.io/ggoutbreak/articles/rt-from-incidence.html

## Examples

``` r
data = example_poisson_rt_smooth()

omega = omega_matrix(example_ip())
k = nrow(omega)

pred_time = 50
index = pred_time-k:1+1

# we will try and estimate the Rt at time k+10:
print(data$rt[pred_time])
#> [1] 1.690049

# first we need a set of incidence estimates
# in the simplest example we use a GLM:

newdata = dplyr::tibble(time = 1:100)



model = stats::glm(count ~ splines::bs(time,df=8), family = "poisson", data=data)
pred = stats::predict(model, newdata, se.fit = TRUE)

# I've picked a df to make this example work. In real life you would need
# to validate the incidence model is sensible and not over fitting
# before using it to estimate RT:
# ggplot2::ggplot()+
#   ggplot2::geom_point(data=data, ggplot2::aes(x=time, y=count))+
#   ggplot2::geom_line(
#     data = newdata %>% dplyr::mutate(fit = exp(pred$fit)
#     ), ggplot2::aes(x=time,y=fit))

mu_t = pred$fit[index]
sigma_t = pred$se.fit[index]

# prediction vcov is not simple from GLM models.
rt_est = rt_incidence_reference_implementation(mu_t=mu_t,sigma_t = sigma_t, omega = omega)

# quantiles from a mixture distribution:
rt_est$quantile_Rt_fn(c(0.025,0.5,0.975))
#>  q.0.025    q.0.5  q.0.975 
#> 1.424568 1.577702 1.793228 
# and from the rough estimate based on matching of moments:
stats::qlnorm(c(0.025,0.5,0.975), rt_est$meanlog_Rt_star, rt_est$sdlog_Rt_star)
#> [1] 1.408223 1.582160 1.777581

# GLM do not produce vcov matrices we can estimate them if we have access to
# the data:
vcov_glm = vcov_from_residuals(data$count[1:100], pred$fit, pred$se.fit)$vcov_matrix
vcov_glm_ij = vcov_glm[index, index]

# Using an estimated vcov we get very similar answers:
rt_est_vcov = rt_incidence_reference_implementation(mu_t=mu_t,vcov_ij = vcov_glm_ij, omega = omega)
rt_est_vcov$quantile_Rt_fn(c(0.025,0.5,0.975))
#>  q.0.025    q.0.5  q.0.975 
#> 1.423874 1.577717 1.794345 
# In theory assuming independence leads to excess uncertainty and possible
# underestimation bias. In most situations though this appears low risk.

# Lets do the same with a GAM:
model2 = mgcv::gam(count ~ s(time), family = "poisson", data=data)
pred2 = stats::predict(model2, newdata, se.fit = TRUE)

# we can get prediction level vcov from GAMs easily
Xp = stats::predict(model2, newdata, type = "lpmatrix")
pred_vcov = Xp %*% stats::vcov(model2) %*% t(Xp)
vcov_ij = pred_vcov[index, index]
mu_t2 = pred2$fit[index]

rt_est2 = rt_incidence_reference_implementation(mu_t=mu_t2, vcov_ij=vcov_ij, omega = omega)
rt_est2$quantile_Rt_fn(c(0.025,0.5,0.975))
#>  q.0.025    q.0.5  q.0.975 
#> 1.442381 1.592879 1.826743 

# How does this compare to EpiEstim:
# N.B. setting seed to make deterministic
withr::with_seed(100, {
  epi = EpiEstim::estimate_R(
    data$count,
    method = "si_from_sample",
    si_sample = omega,
    config = EpiEstim::make_config(
      method = "si_from_sample",
      t_start = pred_time-7,
      t_end = pred_time,
      n2 = 100)
  )
})

epi$R %>%
  dplyr::select(`Quantile.0.025(R)`, `Median(R)`, `Quantile.0.975(R)`)
#>   Quantile.0.025(R) Median(R) Quantile.0.975(R)
#> 1          1.655946   1.91267           2.25184

```
