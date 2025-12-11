# Example generators

These are a set of internally cached functions to support examples. They
are exported as internal functions so that the examples can run
correctly and cache their output to prevent excessive repetition of the
code examples.

## Usage

``` r
example_england_covid_by_age()

example_poisson_age_stratified()

example_poisson_locfit()

example_proportion_age_stratified()

example_ganyani_ip()

example_du_serial()

example_ip()

example_bpm()

example_serial()

example_poisson_rt()

example_poisson_growth_rate()

example_poisson_rt_smooth()

example_poisson_rt_2class()

example_delayed_observation()
```

## Value

example output of the stated functions, usually a dataframe.

## Functions

- `example_england_covid_by_age()`: Example input format including age
  stratified COVID-19 data.

- `example_poisson_age_stratified()`: Example output of
  [`poisson_locfit_model()`](https://ai4ci.github.io/ggoutbreak/reference/poisson_locfit_model.md)
  run on age stratified COVID-19 data.

- `example_poisson_locfit()`: Example output of
  [`poisson_locfit_model()`](https://ai4ci.github.io/ggoutbreak/reference/poisson_locfit_model.md)
  run on un-stratified COVID-19 data.

- `example_proportion_age_stratified()`: Example output of
  [`proportion_locfit_model()`](https://ai4ci.github.io/ggoutbreak/reference/proportion_locfit_model.md)
  An England COVID-19 age stratified proportion model dataset. The
  proportion represented here is the positive tests in this age group,
  versus the positive tests in all age groups, so represents a relative
  growth of ne age group versus others.

- `example_ganyani_ip()`: Generation time estimate from
  [`make_gamma_ip()`](https://ai4ci.github.io/ggoutbreak/reference/make_gamma_ip.md).
  This estimate is truncated to make it compatible with `EpiEstim`. Take
  from Ganyani T, Kremer C, Chen D, Torneri A, Faes C, Wallinga J,
  Hens N. Estimating the generation interval for coronavirus disease
  (COVID-19) based on symptom onset data, March 2020. Euro Surveill.
  2020 Apr;25(17):2000257. doi: 10.2807/1560-7917.ES.2020.25.17.2000257.
  PMID: 32372755; PMCID: PMC7201952.

- `example_du_serial()`: An undjusted serial interval between symptoms
  [`make_resampled_ip()`](https://ai4ci.github.io/ggoutbreak/reference/make_resampled_ip.md) Z.
  Du, X. Xu, Y. Wu, L. Wang, B. J. Cowling, and L. A. Meyers, ‘Serial
  Interval of COVID-19 among Publicly Reported Confirmed Cases’, Emerg
  Infect Dis, vol. 26, no. 6, pp. 1341–1343, Jun. 2020, doi:
  10.3201/eid2606.200357.

- `example_ip()`: Example output of
  [`make_gamma_ip()`](https://ai4ci.github.io/ggoutbreak/reference/make_gamma_ip.md)
  A test infectivity profile generated from a set of discretised gamma
  distributions with parameters mean 5 (95% CI 4-6) and sd 2 (95% CI
  1.5-2.5).

- `example_bpm()`: Example output of
  [`sim_branching_process()`](https://ai4ci.github.io/ggoutbreak/reference/sim_branching_process.md)
  An example of the linelist output of the branching process model
  simulation. This is generated using the `example_ip()` infectivity
  profile and also includes a delay to symptom onset which is a random
  gamma distributed quantity with mean of 6 and standard deviation of 2

- `example_serial()`: Example output of
  [`make_resampled_ip()`](https://ai4ci.github.io/ggoutbreak/reference/make_resampled_ip.md)
  A serial interval estimated from symptom onset in simulated data
  including negative intervals. This serial interval is resampled from
  the first 1000 patients in the `example_bpm()` dataset for whom both
  infector and infectee has symptoms. These patients are generated with
  a symptom delay of mean 6 days and SD 2 from infection (discrete
  under-dispersed gamma) and an infectivity profile with mean 5 days and
  SD 2 as defined in `example_ip()` dataset. This serial interval is
  relevant to the estimation of \$R_t\$ from symptomatic case counts in
  the `example_bpm()` dataset but includes negative times, and cannot be
  used with `EpiEstim`.

- `example_poisson_rt()`: Example output of
  [`sim_poisson_Rt_model()`](https://ai4ci.github.io/ggoutbreak/reference/sim_poisson_Rt_model.md)
  An example of the linelist output of the poisson model simulation with
  defined \$R_t\$. This is generated using the `example_ip()`
  infectivity profile

- `example_poisson_growth_rate()`: Example output of
  [`sim_poisson_model()`](https://ai4ci.github.io/ggoutbreak/reference/sim_poisson_model.md)
  A simulation dataset determined by a step function of growth rates.
  This is useful for demonstrating growth rate estimators.

- `example_poisson_rt_smooth()`: Example output of
  [`sim_poisson_Rt_model()`](https://ai4ci.github.io/ggoutbreak/reference/sim_poisson_Rt_model.md)
  Output of a poisson model simulation with a smooth function for
  \$R_t\$ defined as `R(t) = e^(sin(t/80*pi)^4-0.25))`. This is a
  relatively unchallenging test data set that should not pose a problem
  for smooth estimators.

- `example_poisson_rt_2class()`: Two class example using
  [`sim_poisson_Rt_model()`](https://ai4ci.github.io/ggoutbreak/reference/sim_poisson_Rt_model.md)
  Two smooth \$R_t\$ based incidence timeseries one growing with an time
  varying Rt `exp(sin(t / 80 * pi)^4 - 0.25)` and the other offset by 10
  days: `exp(sin((t - 10) / 80 * pi)^4 - 0.25)`. This is a simple
  relative growth test

- `example_delayed_observation()`: Example output of
  [`sim_branching_process()`](https://ai4ci.github.io/ggoutbreak/reference/sim_branching_process.md)
  with
  [`sim_apply_delay()`](https://ai4ci.github.io/ggoutbreak/reference/sim_apply_delay.md)
  This simulates what might be observed in an outbreak if there was on
  average a 5 day delay on the reporting of hospital admissions. The
  configuration of the outbreak is the same as `example_bpm()`, but this
  is summary data that describes the whole history of admissions that
  were observed, when observed at any given time point. This is a
  triangular set of data where the counts are right censored by the
  observation time.

## Examples

``` r
suppressWarnings({
  example_poisson_age_stratified() %>% dplyr::glimpse()
  example_ip() %>% dplyr::glimpse()
  example_bpm() %>% dplyr::glimpse()
  example_serial() %>% dplyr::glimpse()
  example_poisson_rt() %>% dplyr::glimpse()
  example_poisson_growth_rate() %>% dplyr::glimpse()
  example_poisson_rt_smooth() %>% dplyr::glimpse()
  example_poisson_rt_2class() %>% dplyr::glimpse()
  example_ganyani_ip() %>% dplyr::glimpse()
  example_du_serial() %>% dplyr::glimpse()
  example_delayed_observation() %>% dplyr::glimpse()
})
#> incomplete fit locfit model - try decreasing `deg` or increasing `window`.
#> Rows: 26,790
#> Columns: 20
#> Groups: class [19]
#> $ class            <fct> 00_04, 00_04, 00_04, 00_04, 00_04, 00_04, 00_04, 00_0…
#> $ time             <t[day]> 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44…
#> $ incidence.fit    <dbl> -1.612905, -1.803708, -1.959326, -2.082483, -2.175902…
#> $ incidence.se.fit <dbl> 1.0533915, 0.9003108, 0.7844360, 0.7012956, 0.6463539…
#> $ incidence.0.025  <dbl> 0.02528574, 0.02820419, 0.03029440, 0.03152429, 0.031…
#> $ incidence.0.05   <dbl> 0.03523977, 0.03745604, 0.03878940, 0.03932042, 0.039…
#> $ incidence.0.25   <dbl> 0.09793934, 0.08972926, 0.08304107, 0.07765344, 0.073…
#> $ incidence.0.5    <dbl> 0.19930774, 0.16468709, 0.14095339, 0.12462043, 0.113…
#> $ incidence.0.75   <dbl> 0.40559365, 0.30226304, 0.23925342, 0.19999437, 0.175…
#> $ incidence.0.95   <dbl> 1.1272371, 0.7240980, 0.5121982, 0.3949666, 0.3286558…
#> $ incidence.0.975  <dbl> 1.5709871, 0.9616244, 0.6558260, 0.4926439, 0.4028982…
#> $ growth.fit       <dbl> -0.2093030305, -0.1855586041, -0.1593989278, -0.13168…
#> $ growth.se.fit    <dbl> 0.22418245, 0.20710820, 0.19030250, 0.17387303, 0.157…
#> $ growth.0.025     <dbl> -0.64869255, -0.59148321, -0.53238498, -0.47246827, -…
#> $ growth.0.05      <dbl> -0.57805034, -0.52622127, -0.47241869, -0.41767908, -…
#> $ growth.0.25      <dbl> -0.360511792, -0.325250959, -0.287756014, -0.24895896…
#> $ growth.0.5       <dbl> -0.2093030305, -0.1855586041, -0.1593989278, -0.13168…
#> $ growth.0.75      <dbl> -0.058094269, -0.045866249, -0.031041841, -0.01440780…
#> $ growth.0.95      <dbl> 0.15944428, 0.15510406, 0.15362083, 0.15431230, 0.156…
#> $ growth.0.975     <dbl> 0.23008649, 0.22036600, 0.21358712, 0.20910150, 0.206…
#> Rows: 1,800
#> Columns: 5
#> Groups: boot [100]
#> $ tau         <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
#> $ a0          <dbl> 0.0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.…
#> $ a1          <dbl> 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11…
#> $ probability <dbl> 0.000000e+00, 7.677533e-03, 9.291725e-02, 2.043664e-01, 2.…
#> $ boot        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2…
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> complete
#> Rows: 36,813
#> Columns: 8
#> $ time                <t[day]> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ id                  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,…
#> $ generation_interval <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ infector            <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
#> $ generation          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ symptom_onset       <lgl> FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,…
#> $ symptom_onset_delay <dbl> NA, NA, 6, 6, 5, 5, NA, 6, NA, 11, 6, NA, 5, 8, NA…
#> $ symptom_onset_time  <t[day]> NA, NA, 6, 6, 5, 5, NA, 6, NA, 11, 6, NA, 5, 8,…
#> Rows: 2,394
#> Columns: 5
#> Groups: boot [100]
#> $ tau         <dbl> -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, …
#> $ a0          <dbl> -7.5, -6.5, -5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, …
#> $ a1          <dbl> -6.5, -5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3…
#> $ probability <dbl> 9.138864e-06, 2.057870e-03, 1.033504e-03, 5.130966e-03, 7.…
#> $ boot        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> Rows: 81
#> Columns: 6
#> Groups: statistic [1]
#> $ time      <t[day]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ rt        <dbl> 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, …
#> $ imports   <dbl> 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ rate      <dbl> 30.0000000, 0.7045695, 5.7984339, 12.9399338, 17.4968560, 20…
#> $ count     <int> 27, 0, 5, 12, 16, 15, 20, 30, 32, 43, 53, 62, 88, 90, 112, 1…
#> $ statistic <chr> "infections", "infections", "infections", "infections", "inf…
#> Rows: 105
#> Columns: 6
#> Groups: statistic [1]
#> $ time      <t[day]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ growth    <dbl> 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, …
#> $ imports   <dbl> 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
#> $ rate      <dbl> 100.0000, 110.5171, 122.1403, 134.9859, 149.1825, 164.8721, …
#> $ count     <int> 94, 93, 131, 136, 153, 157, 188, 196, 223, 247, 268, 320, 32…
#> $ statistic <chr> "infections", "infections", "infections", "infections", "inf…
#> Rows: 161
#> Columns: 6
#> Groups: statistic [1]
#> $ time      <t[day]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ rt        <dbl> 0.7788008, 0.7788026, 0.7788303, 0.7789494, 0.7792673, 0.779…
#> $ imports   <dbl> 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ rate      <dbl> 30.0000000, 0.2194882, 1.8028493, 3.9734539, 5.0843677, 5.01…
#> $ count     <int> 29, 0, 0, 6, 3, 9, 5, 3, 4, 6, 10, 0, 1, 1, 2, 5, 2, 2, 1, 3…
#> $ statistic <chr> "infections", "infections", "infections", "infections", "inf…
#> Rows: 322
#> Columns: 8
#> Groups: class [2]
#> $ time      <t[day]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ rt        <dbl> 0.7788008, 0.7788026, 0.7788303, 0.7789494, 0.7792673, 0.779…
#> $ imports   <dbl> 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ rate      <dbl> 30.0000000, 0.2194882, 1.8028493, 3.9734539, 5.0843677, 5.01…
#> $ count     <int> 33, 0, 0, 1, 4, 5, 5, 3, 3, 3, 2, 7, 5, 1, 3, 4, 3, 3, 4, 4,…
#> $ statistic <chr> "infections", "infections", "infections", "infections", "inf…
#> $ class     <fct> one, one, one, one, one, one, one, one, one, one, one, one, …
#> $ denom     <int> 64, 0, 2, 9, 9, 10, 15, 5, 4, 8, 3, 14, 10, 3, 9, 5, 6, 7, 1…
#> Rows: 2,400
#> Columns: 5
#> Groups: boot [100]
#> $ tau         <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, …
#> $ a0          <dbl> 0.0, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.…
#> $ a1          <dbl> 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11…
#> $ probability <dbl> 0.000000e+00, 3.213194e-04, 2.011007e-02, 1.168491e-01, 2.…
#> $ boot        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> Rows: 2,528
#> Columns: 5
#> Groups: boot [100]
#> $ tau         <dbl> -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, …
#> $ a0          <dbl> -5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5, 4.…
#> $ a1          <dbl> -4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5…
#> $ probability <dbl> 0.007822933, 0.003372250, 0.014498957, 0.014498957, 0.0567…
#> $ boot        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> .
#> complete
#> Rows: 3,321
#> Columns: 4
#> Groups: obs_time, statistic [81]
#> $ statistic <chr> "admitted", "admitted", "admitted", "admitted", "admitted", …
#> $ obs_time  <t[day]> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,…
#> $ time      <t[day]> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
#> $ count     <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
```
