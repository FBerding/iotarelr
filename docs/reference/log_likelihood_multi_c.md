# Estimating log-likelihood in Condition Stage

Function written in `C++` estimating the log likelihood of a given
parameter set during the condition stage.

## Usage

``` r
log_likelihood_multi_c(probabilities, observations)
```

## Arguments

- probabilities:

  `NumericVector` containing the probabilities of a multinominal
  distribution. In the context of Iota Reliability this refers to a
  specific row of the Assignment Error Matrix.

- observations:

  `NumericVector` containing the number of observations for each
  category of the multinominal distribution.

## Value

Returns the log likelihood as a single numeric value.

## References

Berding, Florian, and Pargmann, Julia (2022).Iota Reliability Concept of
the Second Generation.Measures for Content Analysis Done by Humans or
Artificial Intelligences. Berlin: Logos. https://doi.org/10.30819/5581
