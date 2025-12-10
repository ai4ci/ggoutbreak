# The Log Normal Distribution

Density, distribution function, quantile function and random generation
for the log normal distribution whose logarithm has mean equal to
`meanlog` and standard deviation equal to `sdlog`.

## Usage

``` r
plnorm2(q, mean = 1, sd = sqrt(exp(1) - 1), lower.tail = TRUE, log.p = FALSE)
```

## Source

`dlnorm` is calculated from the definition (in ‘Details’). `[pqr]lnorm`
are based on the relationship to the normal.

Consequently, they model a single point mass at `exp(meanlog)` for the
boundary case `sdlog = 0`.

## Arguments

- q:

  vector of quantiles

- mean:

  the mean value on the true scale (vectorised)

- sd:

  the standard deviation on the true scale (vectorised)

- lower.tail:

  logical; if TRUE (default), probabilities are \\P\[X \le x\]\\,
  otherwise, \\P\[X \> x\]\\.

- log.p:

  logical; if TRUE, probabilities p are given as log(p).

## Value

`dlnorm` gives the density, `plnorm` gives the distribution function,
`qlnorm` gives the quantile function, and `rlnorm` generates random
deviates.

The length of the result is determined by `n` for `rlnorm`, and is the
maximum of the lengths of the numerical arguments for the other
functions.

The numerical arguments other than `n` are recycled to the length of the
result. Only the first elements of the logical arguments are used.

## Details

The log normal distribution has density \$\$ f(x) =
\frac{1}{\sqrt{2\pi}\sigma x} e^{-(\log(x) - \mu)^2/2 \sigma^2}% \$\$
where \\\mu\\ and \\\sigma\\ are the mean and standard deviation of the
logarithm. The mean is \\E(X) = exp(\mu + 1/2 \sigma^2)\\, the median is
\\med(X) = exp(\mu)\\, and the variance \\Var(X) = exp(2\mu +
\sigma^2)(exp(\sigma^2) - 1)\\ and hence the coefficient of variation is
\\\sqrt{exp(\sigma^2) - 1}\\ which is approximately \\\sigma\\ when that
is small (e.g., \\\sigma \< 1/2\\).

## Note

The cumulative hazard \\H(t) = - \log(1 - F(t))\\ is
`-plnorm(t, r, lower = FALSE, log = TRUE)`.

## References

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) *The New S
Language*. Wadsworth & Brooks/Cole.

Johnson, N. L., Kotz, S. and Balakrishnan, N. (1995) *Continuous
Univariate Distributions*, volume 1, chapter 14. Wiley, New York.

## See also

[Distributions](https://rdrr.io/r/stats/Distributions.html) for other
standard distributions, including
[`dnorm`](https://rdrr.io/r/stats/Normal.html) for the normal
distribution.

## Examples

``` r
plnorm2(seq(0,4,0.25), 2, 1)
#>  [1] 0.000000e+00 1.550937e-05 3.482566e-03 3.287216e-02 1.091319e-01
#>  [6] 2.239928e-01 3.546433e-01 4.814610e-01 5.933575e-01 6.863496e-01
#> [11] 7.607047e-01 8.186775e-01 8.631396e-01 8.968813e-01 9.223215e-01
#> [16] 9.414327e-01 9.557664e-01
```
