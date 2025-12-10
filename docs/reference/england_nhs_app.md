# NHS COVID-19 app data

check-in (social activity) and alerts (self isolation instruction) data
from the NHS COVID-19 app, aggregated to country level on a week by week
basis.

## Usage

``` r
data(england_nhs_app)
```

## Format

A dataframe containing the following columns:

- date (date) - the start date of the week

- alerts (integer) - the count of self-isolation alerts

- visits (integer) - the number of venue check-ins representing visits
  to social venues.

- time (time_period) - the time column

No mandatory groupings.

No default value.

137 rows and 4 columns
