library(tidyverse)
tmp = tempfile(fileext = ".zip")
download.file(
  "https://pmc.ncbi.nlm.nih.gov/articles/instance/9796165/bin/jiac317_supplementary_data.zip",
  tmp
)

dir = tempdir()
unzip(tmp, "Supplementary_Data_sensitivity_summary.xlsx", exdir = dir)

modelled = readxl::read_excel(
  fs::path(dir, "Supplementary_Data_sensitivity_summary.xlsx"),
  sheet = "All tests"
)

tmp = modelled %>%
  mutate(time = floor(days_since_infection + 0.5)) %>%
  select(-days_since_infection) %>%
  nest(data = -time) %>%
  dplyr::glimpse()

tmp = tmp %>%
  mutate(
    data2 = purrr::map(
      data,
      ~ dplyr::tibble(
        x = c(0, mean(.x$lower_95), mean(.x$median), mean(.x$upper_95), 1),
        y = c(0, 0.025, 0.5, 0.975, 1)
      )
    )
  ) %>%
  mutate(
    qfun = purrr::map(
      data2,
      ~ splinefun(x = .x$y, y = .x$x, method = "monoH.FC")
    )
  ) %>%
  mutate(
    samples = purrr::map(
      qfun,
      ~ dplyr::tibble(
        probability = .x(runif(100, min = 0.01, max = 0.99)),
        boot = 1:100
      )
    )
  )

covid_test_sensitivity = tmp %>%
  select(tau = time, samples) %>%
  unnest(samples) %>%
  group_by(boot)
interfacer::itest(covid_test_sensitivity, ggoutbreak::i_discrete_ip)

if (interactive()) {
  interfacer::use_dataframe(covid_test_sensitivity)
}


# https://www.nature.com/articles/s41467-020-20568-4
# Von Kampen et al
# This is from symptom onset not infection

tmp2 = tempfile(fileext = ".xlsx")
download.file(
  "https://static-content.springer.com/esm/art%3A10.1038%2Fs41467-020-20568-4/MediaObjects/41467_2020_20568_MOESM4_ESM.xlsx",
  tmp2
)
raw = readxl::read_excel(tmp2, "Sheet1")

raw = raw %>%
  mutate(
    tau = `duration of symptoms in days`,
    positive = `virus culture result` == "POS"
  )

model = glm(positive ~ splines::bs(tau), data = raw, family = "binomial")
tmp = predict(model, newdata = dplyr::tibble(tau = 0:50), se.fit = TRUE)

df = dplyr::tibble(
  tau = 0:50,
  mu = tmp$fit,
  sigma = tmp$se.fit
)

covid_viral_shedding = df %>%
  dplyr::filter(mu > .logit(0.001)) %>%
  mutate(
    samples = purrr::map2(
      mu,
      sigma,
      ~ dplyr::tibble(
        probability = .expit(rnorm(100, mean = .x, sd = .y)),
        boot = 1:100
      )
    )
  ) %>%
  select(-mu, -sigma) %>%
  unnest(samples) %>%
  group_by(boot) %>%
  dplyr::glimpse()

if (interactive()) {
  interfacer::use_dataframe(covid_viral_shedding)
}

plot_ip(covid_viral_shedding, alpha = 0.1)
