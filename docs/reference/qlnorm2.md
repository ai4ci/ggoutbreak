# The Log Normal Distribution

Density, distribution function, quantile function and random generation
for the log normal distribution whose logarithm has mean equal to
`meanlog` and standard deviation equal to `sdlog`.

## Usage

``` r
qlnorm2(p, mean = 1, sd = sqrt(exp(1) - 1), lower.tail = TRUE, log.p = FALSE)
```

## Source

`dlnorm` is calculated from the definition (in ‘Details’). `[pqr]lnorm`
are based on the relationship to the normal.

Consequently, they model a single point mass at `exp(meanlog)` for the
boundary case `sdlog = 0`.

## Arguments

- p:

  vector of probabilities.

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
qlnorm2(c(0.25,0.5,0.72), 2, 1)
#> [1] 1.300774 1.788854 2.355843
```
