# Package index

## Incidence, Growth Rate and Reproduction Number estimators

- [`doubling_time()`](https://ai4ci.github.io/ggoutbreak/reference/doubling_time.md)
  : Doubling time from growth rate

- [`gam_delayed_reporting()`](https://ai4ci.github.io/ggoutbreak/reference/gam_delayed_reporting.md)
  : Delayed GAM reporting model function generator

- [`gam_knots()`](https://ai4ci.github.io/ggoutbreak/reference/gam_knots.md)
  : Derive a set of knot points for a GAM from data

- [`gam_nb_model_fn()`](https://ai4ci.github.io/ggoutbreak/reference/gam_nb_model_fn.md)
  : Default GAM count negative binomial model.

- [`gam_poisson_model_fn()`](https://ai4ci.github.io/ggoutbreak/reference/gam_poisson_model_fn.md)
  : Default GAM count model.

- [`growth_rate_from_incidence()`](https://ai4ci.github.io/ggoutbreak/reference/growth_rate_from_incidence.md)
  : Estimate growth rate from modelled incidence

- [`growth_rate_from_prevalence()`](https://ai4ci.github.io/ggoutbreak/reference/growth_rate_from_prevalence.md)
  **\[experimental\]** : Estimate relative growth rate from estimated
  prevalence

- [`growth_rate_from_proportion()`](https://ai4ci.github.io/ggoutbreak/reference/growth_rate_from_proportion.md)
  **\[experimental\]** : Estimate relative growth rate from modelled
  proportion

- [`infer_population()`](https://ai4ci.github.io/ggoutbreak/reference/infer_population.md)
  : Infers a daily baseline population for a timeseries

- [`infer_prevalence()`](https://ai4ci.github.io/ggoutbreak/reference/infer_prevalence.md)
  **\[experimental\]** : Infer the prevalence of disease from incidence
  estimates and population size.

- [`infer_rate_ratio()`](https://ai4ci.github.io/ggoutbreak/reference/infer_rate_ratio.md)
  **\[experimental\]** : Calculate a risk ratio from incidence

- [`infer_risk_ratio()`](https://ai4ci.github.io/ggoutbreak/reference/infer_risk_ratio.md)
  **\[experimental\]** : Calculate a normalised risk ratio from
  proportions

- [`inv_wallinga_lipsitch()`](https://ai4ci.github.io/ggoutbreak/reference/inv_wallinga_lipsitch.md)
  : Calculate a growth rate from a reproduction number and an
  infectivity profile,

- [`linelist()`](https://ai4ci.github.io/ggoutbreak/reference/linelist.md)
  :

  Coerce an object to a `ggoutbreak` compatible case linelist.

- [`multinomial_nnet_model()`](https://ai4ci.github.io/ggoutbreak/reference/multinomial_nnet_model.md)
  : Multinomial time-series model.

- [`normalise_count()`](https://ai4ci.github.io/ggoutbreak/reference/normalise_count.md)
  : Calculate a normalised count per capita

- [`normalise_incidence()`](https://ai4ci.github.io/ggoutbreak/reference/normalise_incidence.md)
  : Calculate a normalised incidence rate per capita

- [`poisson_gam_model()`](https://ai4ci.github.io/ggoutbreak/reference/poisson_gam_model.md)
  : GAM poisson time-series model

- [`poisson_glm_model()`](https://ai4ci.github.io/ggoutbreak/reference/poisson_glm_model.md)
  : Poisson time-series model.

- [`poisson_locfit_model()`](https://ai4ci.github.io/ggoutbreak/reference/poisson_locfit_model.md)
  : Poisson time-series model.

- [`proportion_glm_model()`](https://ai4ci.github.io/ggoutbreak/reference/proportion_glm_model.md)
  : Binomial time-series model.

- [`proportion_locfit_model()`](https://ai4ci.github.io/ggoutbreak/reference/proportion_locfit_model.md)
  : A binomial proportion estimate and associated exponential growth
  rate

- [`rescale_model()`](https://ai4ci.github.io/ggoutbreak/reference/rescale_model.md)
  : Rescale a timeseries in the temporal dimension

- [`rt_cori()`](https://ai4ci.github.io/ggoutbreak/reference/rt_cori.md)
  : Reproduction number estimate using the Cori method

- [`rt_epiestim()`](https://ai4ci.github.io/ggoutbreak/reference/rt_epiestim.md)
  :

  `EpiEstim` reproduction number wrapper function

- [`rt_from_growth_rate()`](https://ai4ci.github.io/ggoutbreak/reference/rt_from_growth_rate.md)
  : Wallinga-Lipsitch reproduction number from growth rates

- [`rt_from_incidence()`](https://ai4ci.github.io/ggoutbreak/reference/rt_from_incidence.md)
  : Reproduction number from modelled incidence

- [`rt_from_renewal()`](https://ai4ci.github.io/ggoutbreak/reference/rt_from_renewal.md)
  : Reproduction number from renewal equation applied to modelled
  incidence using statistical re-sampling

- [`rt_incidence_reference_implementation()`](https://ai4ci.github.io/ggoutbreak/reference/rt_incidence_reference_implementation.md)
  : Reference implementation of the Rt from modelled incidence algorithm

- [`rt_incidence_timeseries_implementation()`](https://ai4ci.github.io/ggoutbreak/reference/rt_incidence_timeseries_implementation.md)
  : Time series implementation of the Rt from modelled incidence
  algorithm

- [`timeseries()`](https://ai4ci.github.io/ggoutbreak/reference/timeseries.md)
  :

  Coerce an object to a `ggoutbreak` compatible time series dataframe

- [`wallinga_lipsitch()`](https://ai4ci.github.io/ggoutbreak/reference/wallinga_lipsitch.md)
  : Calculate the reproduction number from a growth rate estimate and an
  infectivity profile

## Delay distributions and Infectivity profiles

- [`format_ip()`](https://ai4ci.github.io/ggoutbreak/reference/format_ip.md)
  : Print a summary of an infectivity profile

- [`make_empirical_ip()`](https://ai4ci.github.io/ggoutbreak/reference/make_empirical_ip.md)
  :

  Recover a long format infectivity profile from an `EpiEstim` style
  matrix

- [`make_fixed_ip()`](https://ai4ci.github.io/ggoutbreak/reference/make_fixed_ip.md)
  : Generate a simple discrete infectivity profile from a gamma
  distribution

- [`make_gamma_ip()`](https://ai4ci.github.io/ggoutbreak/reference/make_gamma_ip.md)
  : Make an infectivity profile from published data

- [`make_posterior_ip()`](https://ai4ci.github.io/ggoutbreak/reference/make_posterior_ip.md)
  : Make an infectivity profile from posterior samples

- [`make_resampled_ip()`](https://ai4ci.github.io/ggoutbreak/reference/make_resampled_ip.md)
  : Re-sample an empirical IP distribution direct from data

- [`omega_matrix()`](https://ai4ci.github.io/ggoutbreak/reference/omega_matrix.md)
  : Generate a infectivity profile matrix from a long format

- [`summarise_ip()`](https://ai4ci.github.io/ggoutbreak/reference/summarise_ip.md)
  : Generate a single infectivity profile from multiple bootstraps

## Visualisations

- [`breaks_log1p()`](https://ai4ci.github.io/ggoutbreak/reference/breaks_log1p.md)
  : A scales breaks generator for log1p scales
- [`geom_events()`](https://ai4ci.github.io/ggoutbreak/reference/geom_events.md)
  : Add time series event markers to a time series plot.
- [`integer_breaks()`](https://ai4ci.github.io/ggoutbreak/reference/integer_breaks.md)
  : Strictly integer breaks for continuous scale
- [`logit_trans()`](https://ai4ci.github.io/ggoutbreak/reference/logit_trans.md)
  : logit scale
- [`plot_cases()`](https://ai4ci.github.io/ggoutbreak/reference/plot_cases.md)
  : Plot a line-list of cases as a histogram
- [`plot_counts()`](https://ai4ci.github.io/ggoutbreak/reference/plot_counts.md)
  : Plot a raw case count timeseries
- [`plot_growth_phase()`](https://ai4ci.github.io/ggoutbreak/reference/plot_growth_phase.md)
  : Plot an incidence or proportion versus growth phase diagram
- [`plot_growth_rate()`](https://ai4ci.github.io/ggoutbreak/reference/plot_growth_rate.md)
  : Growth rate timeseries diagram
- [`plot_incidence()`](https://ai4ci.github.io/ggoutbreak/reference/plot_incidence.md)
  : Plot an incidence timeseries
- [`plot_ip()`](https://ai4ci.github.io/ggoutbreak/reference/plot_ip.md)
  : Plot an infectivity profile
- [`plot_multinomial()`](https://ai4ci.github.io/ggoutbreak/reference/plot_multinomial.md)
  : Plot a multinomial proportions model
- [`plot_prevalence()`](https://ai4ci.github.io/ggoutbreak/reference/plot_prevalence.md)
  **\[experimental\]** : Plot a timeseries of disease prevalence
- [`plot_proportion()`](https://ai4ci.github.io/ggoutbreak/reference/plot_proportion.md)
  : Plot a proportions timeseries
- [`plot_proportions_data()`](https://ai4ci.github.io/ggoutbreak/reference/plot_proportions_data.md)
  : Plot a raw case count proportion timeseries
- [`plot_rt()`](https://ai4ci.github.io/ggoutbreak/reference/plot_rt.md)
  : Reproduction number timeseries diagram
- [`scale_x_log1p()`](https://ai4ci.github.io/ggoutbreak/reference/scale_x_log1p.md)
  : A log1p x scale
- [`scale_x_logit()`](https://ai4ci.github.io/ggoutbreak/reference/scale_x_logit.md)
  : A logit x scale
- [`scale_y_log1p()`](https://ai4ci.github.io/ggoutbreak/reference/scale_y_log1p.md)
  : A log1p y scale
- [`scale_y_logit()`](https://ai4ci.github.io/ggoutbreak/reference/scale_y_logit.md)
  : A logit y scale

## Time series functions

- [`as.time_period()`](https://ai4ci.github.io/ggoutbreak/reference/as.time_period.md)
  [`seq(`*`<time_period>`*`)`](https://ai4ci.github.io/ggoutbreak/reference/as.time_period.md)
  [`is.time_period()`](https://ai4ci.github.io/ggoutbreak/reference/as.time_period.md)
  [`date_to_time()`](https://ai4ci.github.io/ggoutbreak/reference/as.time_period.md)
  [`time_to_date()`](https://ai4ci.github.io/ggoutbreak/reference/as.time_period.md)
  : Time period S3 class methods

- [`cut_date()`](https://ai4ci.github.io/ggoutbreak/reference/cut_date.md)
  : Places a set of dates within a regular time series

- [`date_seq(`*`<Date>`*`)`](https://ai4ci.github.io/ggoutbreak/reference/date_seq.Date.md)
  : Expand a date vector to the full range of possible dates

- [`date_seq()`](https://ai4ci.github.io/ggoutbreak/reference/date_seq.md)
  : Create the full sequence of values in a vector

- [`date_seq(`*`<numeric>`*`)`](https://ai4ci.github.io/ggoutbreak/reference/date_seq.numeric.md)
  : Create the full sequence of values in a vector

- [`date_seq(`*`<time_period>`*`)`](https://ai4ci.github.io/ggoutbreak/reference/date_seq.time_period.md)
  :

  Expand a `time_period` vector to the full range of possible times

- [`fdmy()`](https://ai4ci.github.io/ggoutbreak/reference/fdmy.md) :
  Format date as dmy

- [`is.Date()`](https://ai4ci.github.io/ggoutbreak/reference/is.Date.md)
  : Check whether vector is a date

- [`julian(`*`<time_period>`*`)`](https://ai4ci.github.io/ggoutbreak/reference/julian.time_period.md)
  : Extract Parts of a POSIXt or Date Object

- [`labels(`*`<time_period>`*`)`](https://ai4ci.github.io/ggoutbreak/reference/labels.time_period.md)
  : Label a time period

- [`max_date()`](https://ai4ci.github.io/ggoutbreak/reference/max_date.md)
  : The maximum of a set of dates

- [`min_date()`](https://ai4ci.github.io/ggoutbreak/reference/min_date.md)
  : The minimum of a set of dates

- [`months(`*`<time_period>`*`)`](https://ai4ci.github.io/ggoutbreak/reference/months.time_period.md)
  : Extract Parts of a POSIXt or Date Object

- [`quarters(`*`<time_period>`*`)`](https://ai4ci.github.io/ggoutbreak/reference/quarters.time_period.md)
  : Extract Parts of a POSIXt or Date Object

- [`set_defaults()`](https://ai4ci.github.io/ggoutbreak/reference/set_defaults.md)
  [`with_defaults()`](https://ai4ci.github.io/ggoutbreak/reference/set_defaults.md)
  [`set_default_start()`](https://ai4ci.github.io/ggoutbreak/reference/set_defaults.md)
  [`set_default_unit()`](https://ai4ci.github.io/ggoutbreak/reference/set_defaults.md)
  : Set or reset the default origin and unit for time periods

- [`time_aggregate()`](https://ai4ci.github.io/ggoutbreak/reference/time_aggregate.md)
  : Aggregate time series data preserving the time series

- [`time_summarise()`](https://ai4ci.github.io/ggoutbreak/reference/time_summarise.md)
  : Summarise data from a line list to a time-series of counts.

- [`weekdays(`*`<time_period>`*`)`](https://ai4ci.github.io/ggoutbreak/reference/weekdays.time_period.md)
  : Extract Parts of a POSIXt or Date Object

## Simulation and testing functions

- [`cfg_beta_prob_rng()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_beta_prob_rng.md)
  : Generate a random probability based on features of the simulation

- [`cfg_gamma_ip_fn()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_gamma_ip_fn.md)
  : Get a IP generating function from time varying mean and SD of a
  gamma function

- [`cfg_ip_sampler_rng()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_ip_sampler_rng.md)
  : Randomly sample from an empirical distribution

- [`cfg_linear_fn()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_linear_fn.md)
  : Linear function from dataframe

- [`cfg_step_fn()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_step_fn.md)
  : Step function from dataframe

- [`cfg_transition_fn()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_transition_fn.md)
  : Sample from a multinomial transition matrix

- [`cfg_weekly_gamma_rng()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_weekly_gamma_rng.md)
  : Weekly delay function with day of week effect

- [`cfg_weekly_ip_fn()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_weekly_ip_fn.md)
  : Weekly convolution distribution function

- [`cfg_weekly_proportion_rng()`](https://ai4ci.github.io/ggoutbreak/reference/cfg_weekly_proportion_rng.md)
  : Random probability function with day of week effect

- [`quantify_lag()`](https://ai4ci.github.io/ggoutbreak/reference/quantify_lag.md)
  : Identify estimate lags in a model

- [`score_estimate()`](https://ai4ci.github.io/ggoutbreak/reference/score_estimate.md)
  : Calculate scoring statistics from predictions.

- [`sim_apply_ascertainment()`](https://ai4ci.github.io/ggoutbreak/reference/sim_apply_ascertainment.md)
  : Apply a ascertainment bias to the observed case counts.

- [`sim_apply_delay()`](https://ai4ci.github.io/ggoutbreak/reference/sim_apply_delay.md)
  : Apply delay distribution to count or linelist data

- [`sim_branching_process()`](https://ai4ci.github.io/ggoutbreak/reference/sim_branching_process.md)
  : Generate a line list from a branching process model parametrised by
  reproduction number

- [`sim_convolution()`](https://ai4ci.github.io/ggoutbreak/reference/sim_convolution.md)
  : Apply a time varying probability and convolution to count data

- [`sim_delay()`](https://ai4ci.github.io/ggoutbreak/reference/sim_delay.md)
  : Apply a time-varying probability and delay function to linelist data

- [`sim_delayed_observation()`](https://ai4ci.github.io/ggoutbreak/reference/sim_delayed_observation.md)
  : Apply a right censoring to count data.

- [`sim_events()`](https://ai4ci.github.io/ggoutbreak/reference/sim_events.md)
  : Extract the events dataframe from a simulation output

- [`sim_geom_function()`](https://ai4ci.github.io/ggoutbreak/reference/sim_geom_function.md)
  :

  The principal input function to a `ggoutbreak` simulation as a
  `ggplot2` layer.

- [`sim_multinomial()`](https://ai4ci.github.io/ggoutbreak/reference/sim_multinomial.md)
  : Generate a multinomial outbreak defined by per class growth rates
  and a poisson model

- [`sim_poisson_Rt_model()`](https://ai4ci.github.io/ggoutbreak/reference/sim_poisson_Rt_model.md)
  : Generate an outbreak case count series defined by Reproduction
  number using a poisson model.

- [`sim_poisson_model()`](https://ai4ci.github.io/ggoutbreak/reference/sim_poisson_model.md)
  : Generate an outbreak case count series defined by growth rates using
  a poisson model.

- [`sim_seir_model()`](https://ai4ci.github.io/ggoutbreak/reference/sim_seir_model.md)
  : SEIR model with time-varying transmission parameter

- [`sim_summarise_linelist()`](https://ai4ci.github.io/ggoutbreak/reference/sim_summarise_linelist.md)
  : Summarise a line list

- [`sim_test_data()`](https://ai4ci.github.io/ggoutbreak/reference/sim_test_data.md)
  : Generate a simple time-series of cases based on a growth rate step
  function

## Reparameterised statistical distributions

- [`dbeta2()`](https://ai4ci.github.io/ggoutbreak/reference/dbeta2.md) :
  The Beta Distribution
- [`dcgamma()`](https://ai4ci.github.io/ggoutbreak/reference/dcgamma.md)
  : Density: gamma distribution constrained to have mean \> sd
- [`dgamma2()`](https://ai4ci.github.io/ggoutbreak/reference/dgamma2.md)
  : The Gamma Distribution
- [`dlnorm2()`](https://ai4ci.github.io/ggoutbreak/reference/dlnorm2.md)
  : The Log Normal Distribution
- [`dlogitnorm()`](https://ai4ci.github.io/ggoutbreak/reference/dlogitnorm.md)
  : Logit-normal distribution
- [`dlogitnorm2()`](https://ai4ci.github.io/ggoutbreak/reference/dlogitnorm2.md)
  : Logit-normal distribution
- [`dnbinom2()`](https://ai4ci.github.io/ggoutbreak/reference/dnbinom2.md)
  : The Negative Binomial Distribution
- [`dnull()`](https://ai4ci.github.io/ggoutbreak/reference/dnull.md) :
  Null distributions always returns NA
- [`dwedge()`](https://ai4ci.github.io/ggoutbreak/reference/dwedge.md) :
  Wedge distribution
- [`pbeta2()`](https://ai4ci.github.io/ggoutbreak/reference/pbeta2.md) :
  The Beta Distribution
- [`pcgamma()`](https://ai4ci.github.io/ggoutbreak/reference/pcgamma.md)
  : Cumulative probability: gamma distribution constrained to have mean
  \> sd
- [`pgamma2()`](https://ai4ci.github.io/ggoutbreak/reference/pgamma2.md)
  : The Gamma Distribution
- [`plnorm2()`](https://ai4ci.github.io/ggoutbreak/reference/plnorm2.md)
  : The Log Normal Distribution
- [`plogitnorm()`](https://ai4ci.github.io/ggoutbreak/reference/plogitnorm.md)
  : Logit-normal distribution
- [`plogitnorm2()`](https://ai4ci.github.io/ggoutbreak/reference/plogitnorm2.md)
  : Logit-normal distribution
- [`pnbinom2()`](https://ai4ci.github.io/ggoutbreak/reference/pnbinom2.md)
  : The Negative Binomial Distribution
- [`pnull()`](https://ai4ci.github.io/ggoutbreak/reference/pnull.md) :
  Null distributions always returns NA
- [`pwedge()`](https://ai4ci.github.io/ggoutbreak/reference/pwedge.md) :
  Wedge distribution
- [`qbeta2()`](https://ai4ci.github.io/ggoutbreak/reference/qbeta2.md) :
  The Beta Distribution
- [`qcgamma()`](https://ai4ci.github.io/ggoutbreak/reference/qcgamma.md)
  : Quantile: gamma distribution constrained to have mean \> sd
- [`qgamma2()`](https://ai4ci.github.io/ggoutbreak/reference/qgamma2.md)
  : The Gamma Distribution
- [`qlnorm2()`](https://ai4ci.github.io/ggoutbreak/reference/qlnorm2.md)
  : The Log Normal Distribution
- [`qlogitnorm()`](https://ai4ci.github.io/ggoutbreak/reference/qlogitnorm.md)
  : Logit-normal distribution
- [`qlogitnorm2()`](https://ai4ci.github.io/ggoutbreak/reference/qlogitnorm2.md)
  : Logit-normal distribution
- [`qnbinom2()`](https://ai4ci.github.io/ggoutbreak/reference/qnbinom2.md)
  : The Negative Binomial Distribution
- [`qnull()`](https://ai4ci.github.io/ggoutbreak/reference/qnull.md) :
  Null distributions always returns NA
- [`qwedge()`](https://ai4ci.github.io/ggoutbreak/reference/qwedge.md) :
  Wedge distribution
- [`rbern()`](https://ai4ci.github.io/ggoutbreak/reference/rbern.md) : A
  random Bernoulli sample as a logical value
- [`rbeta2()`](https://ai4ci.github.io/ggoutbreak/reference/rbeta2.md) :
  The Beta Distribution
- [`rcategorical()`](https://ai4ci.github.io/ggoutbreak/reference/rcategorical.md)
  : Sampling from the multinomial equivalent of the Bernoulli
  distribution
- [`rcgamma()`](https://ai4ci.github.io/ggoutbreak/reference/rcgamma.md)
  : Sampling: gamma distribution constrained to have mean \> sd
- [`rdiscgamma()`](https://ai4ci.github.io/ggoutbreak/reference/rdiscgamma.md)
  : Random count data from a discrete gamma distribution
- [`rexpgrowth()`](https://ai4ci.github.io/ggoutbreak/reference/rexpgrowth.md)
  : Randomly sample incident times in an exponentially growing process
- [`rexpgrowthI0()`](https://ai4ci.github.io/ggoutbreak/reference/rexpgrowthI0.md)
  : Randomly sample incident times in an exponentially growing process
  with initial case load
- [`rgamma2()`](https://ai4ci.github.io/ggoutbreak/reference/rgamma2.md)
  : The Gamma Distribution
- [`rlnorm2()`](https://ai4ci.github.io/ggoutbreak/reference/rlnorm2.md)
  : The Log Normal Distribution
- [`rlogitnorm()`](https://ai4ci.github.io/ggoutbreak/reference/rlogitnorm.md)
  : Logit-normal distribution
- [`rlogitnorm2()`](https://ai4ci.github.io/ggoutbreak/reference/rlogitnorm2.md)
  : Logit-normal distribution
- [`rnbinom2()`](https://ai4ci.github.io/ggoutbreak/reference/rnbinom2.md)
  : The Negative Binomial Distribution
- [`rnull()`](https://ai4ci.github.io/ggoutbreak/reference/rnull.md) :
  Null distributions always returns NA
- [`rwedge()`](https://ai4ci.github.io/ggoutbreak/reference/rwedge.md) :
  Wedge distribution
- [`wedge`](https://ai4ci.github.io/ggoutbreak/reference/wedge.md) :
  Wedge distribution

## Data Documentation

## Others

- [`reband_discrete()`](https://ai4ci.github.io/ggoutbreak/reference/reband_discrete.md)
  : Reband any discrete distribution
- [`vcov_from_residuals()`](https://ai4ci.github.io/ggoutbreak/reference/vcov_from_residuals.md)
  : Estimate Parametric VCOV Matrix from Residuals
