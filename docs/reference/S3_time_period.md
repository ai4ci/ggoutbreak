# S3 time period class

Time periods are just a zero based numeric representation of dates with
a time unit baked in. This allows variable length periods (e.g. days or
weeks), and fractional days to be represented in a consistent(ish) way

## Usage

``` r
new_time_period(
  x = double(),
  start_date = as.Date(0),
  unit = lubridate::days(1)
)
```

## Arguments

- x:

  description

- start_date:

  the zero time date as something that can be coerced to a date. If the
  `x` input is already a `time_period` and this is different to its
  `start_date` then `x` will be recalibrated to use the new start date.

- unit:

  the length of one unit of time. This will be either a integer number
  of days, or a specification such as "1 week", or another
  `time_period`. If `x` is a `time_period`, and the unit is different to
  that of `x` this will return a rescaled `time_period` using the new
  units.

- ...:

  used for subtype implementations

## Value

a `time_period` class, consisting of a vector of numbers, with
attributes for time period and `start_date`

## Functions

- `new_time_period()`: Construct a new time period

## Unit tests


    default = new_time_period(as.numeric(1:10))
    shifted = new_time_period(as.numeric(1:10), start_date = as.Date("1970-01-10"))
    stretched = new_time_period(as.numeric(1:10), unit = lubridate::weeks(1))

    testthat::expect_equal(
      format(default),
      c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
    )

    testthat::expect_equal(
      vec_ptype_full(default),
      "time unit: day, origin: 1970-01-01 (a Thursday)"
    )

    testthat::expect_equal(
      vec_ptype_full(shifted),
      "time unit: day, origin: 1970-01-10 (a Saturday)"
    )

    testthat::expect_equal(
      vec_ptype_full(stretched),
      "time unit: week, origin: 1970-01-01 (a Thursday)"
    )

    testthat::expect_equal(vec_ptype_abbr(default), "t[day]")

    dshifted = vec_cast(default, shifted)
    testthat::expect_equal(
      as.Date(dshifted) == as.Date(default),
      c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
    )

    dsquashed = vec_cast(default, stretched)
    testthat::expect_equal(vec_cast(dsquashed, double()), c(
      0.142857142857143,
      0.285714285714286,
      0.428571428571429,
      0.571428571428571,
      0.714285714285714,
      0.857142857142857,
      1,
      1.14285714285714,
      1.28571428571429,
      1.42857142857143
    ))

    testthat::expect_equal(
      diff(vec_cast(stretched, Sys.Date())),
      structure(
        c(7, 7, 7, 7, 7, 7, 7, 7, 7),
        class = "difftime",
        units = "days"
      )
    )

    # Cast numeric to time period check equality
    testthat::expect_equal(all(c(shifted, vec_cast(11:20, shifted)) == 1:20), TRUE)

    # Different cadence can be matched:
    testthat::expect_equal(
      vec_match(default, stretched),
      c(NA, NA, NA, NA, NA, NA, 1L, NA, NA, NA)
    )

    # Matching is within tolerance
    testthat::expect_equal(all(default == default + 0.0000001), TRUE)

    # comparison
    testthat::expect_equal(
      shifted > rev(shifted),
      c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
    )

    # sorting
    testthat::expect_equal(
      as.numeric(sort(sample(shifted, 10))),
      c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    )

    # Arithmetic:
    tmp = withr::with_seed(100, default + runif(10))
    testthat::expect_equal(any(tmp == default), FALSE)
    testthat::expect_equal(all(floor(tmp) == default), TRUE)

    # Basic operation returns the time_period
    testthat::expect_equal(is.time_period(default - 1), TRUE)

    # modulo works:
    testthat::expect_equal(default 
    # Other operations will work but the result has to be interpreted relative to origin
    testthat::expect_equal(sin(default * pi / 2), c(
      1, 0, -1, 0, 1, 0, -1, 0, 1, 0
    ))
