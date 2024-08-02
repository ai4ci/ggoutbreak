## Venue check ins and pings.
tmp = readr::read_csv("https://assets.publishing.service.gov.uk/media/64662cfde140700013b6e221/covid19_app_country_specific_dataset.csv")
glimpse(tmp)

england_nhs_app = tmp %>%
  dplyr::filter(`Country (Wlad)` == "England / Lloegr") %>%
  dplyr::transmute(
    date = `Week starting (Wythnos yn dechrau)`,
    alerts = as.integer(`Contact tracing alert (Hysbysiadau olrhain cyswllt)`),
    visits = as.integer(`Check-ins (Cofrestriadau)`),
    time = ggoutbreak::as.time_period(date, unit = "1 week", anchor="start")
  )

if (interactive()) interfacer::use_dataframe(england_nhs_app)

