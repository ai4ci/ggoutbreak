# Convert time period to dates

Convert time period to dates

## Usage

``` r
# S3 method for class 'time_period'
as.Date(x, ...)

# S3 method for class 'time_period'
as.POSIXct(x, ...)
```

## Arguments

- x:

  a time_period

- ...:

  not used

## Value

a vector of dates representing the start of each of the input
`time_period` entries

## Functions

- `as.POSIXct(time_period)`: Convert to a vector of POSIXct
