# Transformed root mean squared error (TRMSE)

Computes the TRMSE between simulated and observed values using a Box-Cox
transformation, reducing the influence of extreme values.

## Usage

``` r
trmse(sim, obs, lambda = 0.3)
```

## Arguments

- sim:

  numeric. Simulated values.

- obs:

  numeric. Observed values.

- lambda:

  numeric. Box-Cox transformation parameter. Default: 0.3.

## Value

numeric. The TRMSE value.

## Details

TRMSE applies a Box-Cox transformation to both observed and simulated
values before computing RMSE: \$\$ TRMSE = \sqrt{\frac{1}{n}
\sum\_{i=1}^{n} \bigg( \frac{(S_i + 1)^{\lambda} - 1}{\lambda} -
\frac{(O_i + 1)^{\lambda} - 1}{\lambda} \bigg)^2} \$\$ where \\S_i\\ and
\\O_i\\ are simulated and observed values, respectively. NA values are
removed before computation.

## References

van Werkhoven, K., Wagener, T., Reed, P., & Tang, Y. (2009).
Sensitivity-guided reduction of parametric dimensionality for
multi-objective calibration of watershed models.

## Examples

``` r
# Synthetic daily flow data
set.seed(123)
obs <- abs(rnorm(730, mean = 50, sd = 20))
sim <- obs * runif(730, min = 0.8, max = 1.5)

# Compute TRMSE
trmse(sim, obs)
#> [1] 0.7312501
```
