# Check new rater

Function for estimating the reliability of codings for a new rater based
on Iota 2

## Usage

``` r
check_new_rater(
  true_values,
  assigned_values,
  con_step_size = 1e-04,
  con_random_starts = 5,
  con_max_iterations = 5000,
  con_rel_convergence = 1e-12,
  con_trace = FALSE,
  fast = TRUE,
  free_aem = FALSE
)
```

## Arguments

- true_values:

  `Vector` containing the true categories of the coding units. Vector
  must have the same length as `assigned_values`.

- assigned_values:

  `Vector` containing the assigned categories of the coding units.
  Missing values are currently not supported and have to be omitted from
  the vector. Vector must have the same length as `true_values`.

- con_step_size:

  `Double` for specifying the size for increasing or decreasing the
  probabilities during the conditioning stage of estimation. This value
  should not be less than 1e-3.

- con_random_starts:

  `Integer` for the number of random starts within the condition stage.

- con_max_iterations:

  `Integer` for the maximum number of iterations during the conditioning
  stage.

- con_rel_convergence:

  `Double` for determining the convergence criterion during the
  conditioning stage. The algorithm stops if the relative change is
  smaller than this criterion.

- con_trace:

  `TRUE` for printing progress information on the console during
  estimations in the conditioning stage. `FALSE` if you do not want to
  have this information printed.

- fast:

  `Bool` If `TRUE` a fast estimation is applied during the condition
  stage. This option ignores all parameters beginning with "con\_". If
  `FALSE` the estimation described in Berding and Pargmann (2022) is
  used. Default is `TRUE`.

- free_aem:

  `Bool` If `TRUE` the Assignment Error Matrix is estimated in a way
  ensuring conformity with the assumption of weak superiority. if
  `FALSE` the Assignment Error Matrix is freely estimated. `TRUE` is
  default.

## Value

Returns a `list` with the following three components: The first
component `estimates_categorical_level` comprises all elements that
describe the ratings on a categorical level. The elements are
sub-divided into raw estimates and chance-corrected estimates.

`raw_estimates`

- `alpha_reliability:`:

  A vector containing the Alpha Reliabilities for each category. These
  values represent probabilities.

- `beta_reliability:`:

  A vector containing the Beta Reliabilities for each category. These
  values represent probabilities.

- `assignment_error_matrix:`:

  An Assignment Error Matrix containing the conditional probabilities
  for assigning a unit of category i to categories 1 to n.

- `iota:`:

  A vector containing the Iota values for each category.

`elements_chance_corrected`

- `alpha_reliability:`:

  A vector containing the chance-corrected Alpha Reliabilities for each
  category.

- `beta_reliability:`:

  A vector containing the chance-corrected Beta Reliabilities for each
  category.

The second component `estimates_scale_level` contains elements to
describe the quality of the ratings on a scale level. It contains the
following elements:

- `iota_index:`:

  The Iota Index representing the reliability on a scale level.

- `iota_index_d4:`:

  The Static Iota Index, which is a transformation of the original Iota
  Index, in order to consider the uncertainty of estimation.

- `iota_index_dyn2:`:

  The Dynamic Iota Index, which is a transformation of the original Iota
  Index, in order to consider the uncertainty of estimation.

The third component `information` contains important information
regarding the parameter estimation. It comprises the following elements:

- `log_likelihood:`:

  Log-likelihood of the best solution.

- `convergence:`:

  If estimation converged 0, otherwise 1.

- `est_true_cat_sizes:`:

  Estimated categorical sizes. This is the estimated amount of the
  categories.

- `conformity:`:

  `0` if the solution is in line with assumptions of weak superiority. A
  number greater 0 indicates the number of violations of the assumption
  of weak superiority.

- `random_starts:`:

  Numer of random starts for the EM algorithm.

- `boundaries:`:

  `False` if the best solution does not contain boundary values. `True`
  if the best solution does contain boundary values

- `p_boundaries:`:

  Percentage of solutions with boundary values during estimation.

- `call:`:

  Name of the function that created the object.

- `n_rater:`:

  Number of raters.

- `n_cunits:`:

  Number of coding units.

## Note

The returned object contains further slots since the returned object is
of class `iotarelr_iota2`. These slots are empty because they are not
part of the estimation within this function.

Please do not use the measures on the scale level if the Assignment
Error Matrix was freely estimated since this kind of matrix is not
conceptualized for comparing the coding process with random guessing.

## References

Florian Berding and Julia Pargmann (2022). Iota Reliability Concept of
the Second Generation. Measures for Content Analysis Done by Humans or
Artificial Intelligences. Berlin:Logos. https://doi.org/10.30819/5581
