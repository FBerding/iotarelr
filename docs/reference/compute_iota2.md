# Computes Iota and its elements in version 2

Fits a model of Iota2 to the data

## Usage

``` r
compute_iota2(
  data,
  random_starts = 10,
  max_iterations = 5000,
  cr_rel_change = 1e-12,
  con_step_size = 1e-04,
  con_rel_convergence = 1e-12,
  con_max_iterations = 5000,
  con_random_starts = 5,
  b_min = 0.01,
  fast = TRUE,
  trace = TRUE,
  con_trace = FALSE
)
```

## Arguments

- data:

  Data for which the elements should be estimated. Data must be an
  object of type `data.frame` or `matrix` with cases in the rows and
  raters in the columns.

- random_starts:

  An integer for the number of random starts for the EM algorithm.

- max_iterations:

  An integer for the maximum number of iterations within the EM
  algorithm.

- cr_rel_change:

  Positive numeric value for defining the convergence of the EM
  algorithm.

- con_step_size:

  `Double` for specifying the size for increasing or decreasing the
  probabilities during the conditioning stage of estimation. This value
  should not be less than 1e-3.

- con_rel_convergence:

  `Double` for determining the convergence criterion during the
  conditioning stage. The algorithm stops if the relative change is
  smaller than this criterion.

- con_max_iterations:

  `Integer` for the maximum number of iterations during the conditioning
  stage.

- con_random_starts:

  `Integer` for the number of random starts within the conditioning
  stage.

- b_min:

  Value ranging between 0 and 1, determining the minimal size of the
  categories for checking if boundary values occurred. The algorithm
  tries to select solutions that are not considered to be boundary
  values.

- fast:

  `Bool` If `TRUE` a fast estimation is applied during the condition
  stage. This option ignores all parameters beginning with "con\_". If
  `FALSE` the estimation described in Berding and Pargmann (2022) is
  used. Default is `TRUE`.

- trace:

  `TRUE` for printing progress information on the console. `FALSE` if
  this information is not to be printed.

- con_trace:

  `TRUE` for printing progress information on the console during
  estimations in the conditioning stage. `FALSE` if this information is
  not to be printed.

## Value

Returns a `list` with the following three components: The first
component `estimates_categorical_level` comprises all elements that
describe the ratings on a categorical level. The elements are
sub-divided into raw estimates and chance-corrected estimates.

- `raw_estimates`:

  `alpha_reliability:`

  :   A vector containing the Alpha Reliabilities for each category.
      These values represent probabilities.

  `beta_reliability:`

  :   A vector containing the Beta Reliabilities for each category.
      These values represent probabilities.

  `assignment_error_matrix:`

  :   Assignment Error Matrix containing the conditional probabilities
      for assigning a unit of category i to categories 1 to n.

  `iota:`

  :   A vector containing the Iota values for each category.

  `iota_error_1:`

  :   A vector containing the Iota Error Type I values for each
      category.

  `iota_error_2:`

  :   A vector containing the Iota Error Type II values for each
      category.

- `elements_chance_corrected`:

  `alpha_reliability:`

  :   A vector containing the chance-corrected Alpha Reliabilities for
      each category.

  `beta_reliability:`

  :   A vector containing the chance-corrected Beta Reliabilities for
      each category.

## References

Florian Berding and Julia Pargmann (2022).Iota Reliability Concept of
the Second Generation. Measures for Content Analysis Done by Humans or
Artificial Intelligences. Berlin: Logos. https://doi.org/10.30819/5581
