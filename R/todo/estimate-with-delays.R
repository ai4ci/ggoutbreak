library(tidyverse)

bpm = ggoutbreak::sim_branching_process() %>%
  ggoutbreak::sim_apply_delay() %>%
  glimpse()


# timeseries counts by observation day:
# All of this is observation delay (not physiological delay)
# sample results available when with result (sample date)
# admissions and deaths delayed reporting.
# test results available immediately (reporting date)
# symptoms available immediately

delayed_counts = bpm %>%
  ggoutbreak::sim_summarise_linelist(
    censoring = list(
      admitted = \(t) rgamma2(t, mean = 5),
      death = \(t) rgamma2(t, mean = 10),
      sample = \(t, result_delay) result_delay
    ),
    max_time = 0:80
  )

plot_counts(
  delayed_counts %>% filter(obs_time %% 10 == 0),
  mapping = aes(colour = labels(obs_time))
) +
  geom_line() +
  facet_wrap(~statistic, scales = "free_y")

admit_count = delayed_counts %>%
  filter(statistic == "admitted") %>%
  select(statistic, obs_time, time, count) %>%
  mutate(tau = as.numeric(obs_time - time)) %>%
  glimpse()


library(mgcv)

# future::plan(future::multisession, workers = 12)
#
# do_model = carrier::crate(
#   function(k_val) {
#     delay_factor = 1 - exp(-k_val * data$tau)
#     delay_factor <- pmax(delay_factor, 1e-10)
#     model = #try({
#       mgcv::gam(
#         count ~ s(time, bs = "cr"),
#         data = data,
#         family = "poisson",
#         offset = log(delay_factor)
#       )
#     #}, silent = TRUE)
#     if (inherits(model, "try-error")) {
#       return(NA)
#     }
#     return(model)
#   },
#   data = admit_count
# )

# do_model = carrier::crate(function(k_val) {
#   reporting_prob <- 1 - exp(-k_val * data$tau)
#   model <- mgcv::gam(count ~ s(time,k=10, bs="cr"),
#                data = data,
#                family = "poisson",
#                weights = reporting_prob)
#   if(inherits(model, "try-error")) return(NA)
#   return(model)
# }, data=admit_count %>% mutate(time= as.numeric(time)))

# model = tibble(
#     k = exp(seq(2,-6,length.out = 100))
#   ) %>% dplyr::mutate(
#     model = furrr::future_map(k, do_model,.progress = TRUE)
#     # model = purrr::map(k, do_model,.progress = TRUE)
#   )
# model = model %>% mutate(aic = purrr::map_dbl(model, ~ AIC(.x)))
# ggplot(model, aes(x=log(k),y=aic))+geom_line()
# model %>% filter(aic < min(aic)+qchisq(0.95, df = 1)) %>% glimpse()
#
# best_model = model %>% filter(aic == min(aic)) %>% pull(model) %>% `[[`(1)
# newdata = tibble(time=0:80)
# pred = predict(best_model,newdata,se.fit = TRUE)
# newdata = newdata %>% mutate(
#   time = as.time_period(time,"1 day"),
#   incidence.fit = pred$fit,
#   incidence.se.fit = pred$se.fit
# )
# newdata = newdata %>% ggoutbreak:::.result_from_fit(.,"incidence",inv = exp)
#
# plot_incidence(newdata, data %>% filter(obs_time==80))

library(mgcv)

data = admit_count %>%
  ungroup() %>%
  filter(tau > 0) %>%
  mutate(
    count1 = cbind(count, pmax(count, 2)),
    time = as.numeric(time),
    tau = pmin(tau, 40)
  )
# kts = c(1:10*7)
# kts = seq(4,78,2)
kts = c(seq(10, 70, 7), 80)
best_model =
  mgcv::gam(
    # count ~ s(time, bs = "bs", m=2, k=10) + s(log(tau), k = 5, pc = 40), #, bs="mpi"),
    count ~ s(time, bs = "cr", k = length(kts)) + s(log(tau), k = 4, pc = 40), #, bs="mpi"),
    data = data,
    knots = list(time = kts),
    method = "REML",
    # mgcv::nb()
    mgcv::cpois()
  )

# gam.check(best_model)

newdata = tibble(time = 0:80, tau = 40)
pred = predict(best_model, newdata, se.fit = TRUE)
deriv = predict_derivative(best_model, newdata)

incid = newdata %>%
  mutate(
    time = as.time_period(time, "1 day"),
    incidence.fit = pred$fit,
    incidence.se.fit = pred$se.fit
  ) %>%
  .result_from_fit(., "incidence", inv = exp) %>%
  mutate(
    time = as.time_period(time, "1 day"),
    growth.fit = deriv$fit,
    growth.se.fit = deriv$se.fit
  ) %>%
  .result_from_fit(., "growth")


plot_incidence(incid, admit_count %>% filter(obs_time == 80)) +
  scale_y_log1p()

# summary(best_model)
# coefficients(best_model)

# Delay distribution ----

newdata_tau = tibble(tau = 1:40, time = 80)
pred_tau = predict(best_model, newdata_tau, se.fit = TRUE)

test = proportion_from_incidence(
  pred_tau$fit,
  pred_tau$se.fit,
  max_time = 40
)

plot_tau = newdata_tau %>%
  mutate(
    ascertainment.fit = test$mu_logit,
    ascertainment.se.fit = test$sigma_logit
  ) %>%
  .result_from_fit("ascertainment", inv = .expit) %>%
  select(-time)

ggplot(
  plot_tau,
  aes(
    x = tau,
    y = ascertainment.0.5,
    ymin = ascertainment.0.025,
    ymax = ascertainment.0.975
  )
) +
  geom_line() +
  geom_ribbon(alpha = 0.1)


# RT ----

# cor_ij = .gam_glm_cor(best_model, newdata)
Xp <- stats::predict(best_model, newdata, type = "lpmatrix")
pred_vcov <- Xp %*% stats::vcov(best_model) %*% t(Xp)

tmp_ip = ggoutbreak::test_ip
mu = pred$fit
sigma = pred$se.fit
min_tau = min(tmp_ip$tau)
omega = tmp_ip %>% omega_matrix(epiestim_compat = FALSE)
rt = rt_incidence_timeseries_implementation(
  pred$time,
  mu = pred$fit,
  vcov = pred_vcov,
  ip = tmp_ip
)

rt_df = bind_rows(rt)
plot_rt(rt_df %>% mutate(time = as.time_period(row_number(), "1 day")))
