# Gradient for Log Likelihood in Condition Stage

Function written in `C++` estimating the gradient of the log likelihood
function for a given parameter set and given observations.

## Usage

``` r
grad_ll(param_values, observations)
```

## Arguments

- param_values:

  `NumericVector` containing the probabilities of a multinominal
  distribution. The length of this factor is the number of categories -
  1 since it contains only the parameters to be estimated.

- observations:

  `NumericVector` containing the number of observations for each
  category of the multinominal distribution. The length of this vector
  equals the number of categories.

## Value

Returns the gradient as a `NumericVector`.

## References

Berding, Florian, and Pargmann, Julia (2022).Iota Reliability Concept of
the Second Generation.Measures for Content Analysis Done by Humans or
Artificial Intelligences. Berlin: Logos. https://doi.org/10.30819/5581
