# Counts of COVID-19 variants

Data from the COG-UK and Sanger centre sequencing programme. The data
were made available through the Welcome foundation at Lower tier local
authority level, and is weekly time series of counts per variant.
Variants were assigned using the tree structure of the Pango lineage.
Different sub-lineages are aggregated to the major WHO variants of
concern.

## Usage

``` r
data(england_variants)
```

## Format

A dataframe containing the following columns:

- date (date) - the end date of the week

- time (time_period) - the time column

- class
  (enum(`Other`,`Alpha (B.1.1.7)`,`Delta (B.1.617.2)`,`Delta (AY.4)`,`Omicron (Other)`,`Omicron (BA.2)`,`Omicron (BA.4)`,`Omicron (BA.5)`,`XBB (Other)`,`Kraken (XBB.1.5)`,`Arcturus (XBB.1.16)`,`Eris (EG.5.1)`)) -
  the class column

- who_class
  (enum(`Other`,`Alpha`,`Delta`,`Omicron`,`Kraken`,`Arcturus`,`Eris`)) -
  the who_class column

- count (numeric) - the weekly count column

- denom (numeric) - the number of sequences performed in that week

Must be grouped by: class (and other groupings allowed).

No default value.

479 rows and 6 columns
