# The Negative Binomial Distribution

Density, distribution function, quantile function and random generation
for the negative binomial distribution with parameters `size` and
`prob`.

## Usage

``` r
rnbinom2(n, mean, sd = sqrt(mean))
```

## Source

`dnbinom` computes via binomial probabilities, using code contributed by
Catherine Loader (see
[`dbinom`](https://rdrr.io/r/stats/Binomial.html)).

`pnbinom` uses [`pbeta`](https://rdrr.io/r/stats/Beta.html).

`qnbinom` uses the Cornish–Fisher Expansion to include a skewness
correction to a normal approximation, followed by a search.

`rnbinom` uses the derivation as a gamma mixture of Poisson
distributions, see

Devroye, L. (1986) *Non-Uniform Random Variate Generation.*
Springer-Verlag, New York. Page 480.

## Arguments

- n:

  number of observations. If `length(n) > 1`, the length is taken to be
  the number required.

- mean:

  the mean value on the true scale (vectorised)

- sd:

  the standard deviation on the true scale (vectorised)

## Value

`dnbinom` gives the density, `pnbinom` gives the distribution function,
`qnbinom` gives the quantile function, and `rnbinom` generates random
deviates.

Invalid `size` or `prob` will result in return value `NaN`, with a
warning.

The length of the result is determined by `n` for `rnbinom`, and is the
maximum of the lengths of the numerical arguments for the other
functions.

The numerical arguments other than `n` are recycled to the length of the
result. Only the first elements of the logical arguments are used.

`rnbinom` returns a vector of type
[integer](https://rdrr.io/r/base/integer.html) unless generated values
exceed the maximum representable integer when
[`double`](https://rdrr.io/r/base/double.html) values are returned.

## Details

The negative binomial distribution with `size` \\= n\\ and `prob` \\=
p\\ has density \$\$ p(x) = \frac{\Gamma(x+n)}{\Gamma(n) x!} p^n
(1-p)^x\$\$ for \\x = 0, 1, 2, \ldots\\, \\n \> 0\\ and \\0 \< p \le
1\\.

This represents the number of failures which occur in a sequence of
Bernoulli trials before a target number of successes is reached. The
mean is \\\mu = n(1-p)/p\\ and variance \\n(1-p)/p^2\\.

A negative binomial distribution can also arise as a mixture of Poisson
distributions with mean distributed as a gamma distribution (see
[`pgamma`](https://rdrr.io/r/stats/GammaDist.html)) with scale parameter
`(1 - prob)/prob` and shape parameter `size`. (This definition allows
non-integer values of `size`.)

An alternative parametrization (often used in ecology) is by the *mean*
`mu` (see above), and `size`, the *dispersion parameter*, where `prob` =
`size/(size+mu)`. The variance is `mu + mu^2/size` in this
parametrization.

If an element of `x` is not integer, the result of `dnbinom` is zero,
with a warning.

The case `size == 0` is the distribution concentrated at zero. This is
the limiting distribution for `size` approaching zero, even if `mu`
rather than `prob` is held constant. Notice though, that the mean of the
limit distribution is 0, whatever the value of `mu`.

The quantile is defined as the smallest value \\x\\ such that \\F(x) \ge
p\\, where \\F\\ is the distribution function.

## See also

[Distributions](https://rdrr.io/r/stats/Distributions.html) for standard
distributions, including
[`dbinom`](https://rdrr.io/r/stats/Binomial.html) for the binomial,
[`dpois`](https://rdrr.io/r/stats/Poisson.html) for the Poisson and
[`dgeom`](https://rdrr.io/r/stats/Geometric.html) for the geometric
distribution, which is a special case of the negative binomial.

## Examples

``` r
rnbinom2(10, 5, sqrt(5))
#>  [1] 5 6 7 4 2 6 8 7 7 6
```
