# Estimating log likelihood in Condition Stage

Function written in `C++` estimating the log likelihood of a given
parameter set during the condition stage.

## Usage

``` r
est_con_multinominal_c(
  observations,
  anchor,
  max_iter = 500000L,
  step_size = 1e-04,
  cr_rel_change = 1e-12,
  n_random_starts = 10L,
  fast = TRUE,
  trace = FALSE
)
```

## Arguments

- observations:

  `NumericVector` containing the frequency of the categories.

- anchor:

  `Integer` ranging between 1 and the number of categories. Anchor
  defines the reference category. That is the category with the highest
  probability according to the assumption of weak superiority.

- max_iter:

  `Integer` specifying the maximal number of iterations for each random
  start.

- step_size:

  `Double` for specifying the size for increasing or decreasing the
  probabilities during the estimation. This value should not be less
  than 1e-3.

- cr_rel_change:

  `Double` for defining when the estimation should stop. That is, if the
  change in log-likelihood is smaller as this value the estimation
  stops.

- n_random_starts:

  `Integer` for the number of random start.

- fast:

  `Bool` If `TRUE` a fast estimation is applied. This option ignored all
  other parameters. If `FALSE` the estimation described in Berding and
  Pargmann (2022) is used. Default is `TRUE`.

- trace:

  `Bool` `TRUE` if information about the progress of estimation should
  be printed to the console. `FALSE` if not desired.

## Value

Returns the log likelihood as a single numeric value.

## References

Berding, Florian, and Pargmann, Julia (2022).Iota Reliability Concept of
the Second Generation.Measures for Content Analysis Done by Humans or
Artificial Intelligences. Berlin: Logos. https://doi.org/10.30819/5581
