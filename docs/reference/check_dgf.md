# Check for Different Guidance Functioning (DGF)

Function for checking if the coding scheme is the same for different
sub-groups.

## Usage

``` r
check_dgf(
  data,
  splitcr,
  random_starts = 300,
  max_iterations = 5000,
  cr_rel_change = 1e-12,
  con_step_size = 1e-04,
  con_random_starts = 10,
  con_max_iterations = 5000,
  con_rel_convergence = 1e-12,
  b_min = 0.01,
  trace = FALSE,
  con_trace = FALSE,
  fast = TRUE
)
```

## Arguments

- data:

  Data for which the elements should be estimated. Data must be an
  object of type `data.frame` or `matrix` with cases in the rows and
  raters in the columns. Please note that no additional variables are
  allowed in this object.

- splitcr:

  `Vector` containing the assignments of coding units to groups. The
  vector must have the same length as the number of rows of object
  `data`.

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

- con_random_starts:

  `Integer` for the number of random starts within the condition stage.

- con_max_iterations:

  `Integer` for the maximum number of iterations during the condition
  stage.

- con_rel_convergence:

  `Double` for determining the convergence criterion during condition
  stage. The algorithm stops if the relative change is smaller than this
  criterion.

- b_min:

  Value ranging between 0 and 1 determining the minimal size of the
  categories for checking if boundary values occurred. The algorithm
  tries to select solutions that are not considered to be boundary
  values.

- trace:

  `TRUE` for printing progress information on the console. `FALSE` if
  this information is not to be printed.

- con_trace:

  `TRUE` for printing progress information on the console during
  estimations in the condition stage. `FALSE` if this information is not
  to be printed.

- fast:

  `Bool` If `TRUE` a fast estimation is applied during the condition
  stage. This option ignores all parameters beginning with "con\_". If
  `FALSE` the estimation described in Berding and Pargmann (2022) is
  used. Default is `TRUE`.

## Value

Returns an object of class `iotarelr_iota2_dif`. For each group, the
results of the estimation are saved separately. The structure within
each group is similar to the results from
[`compute_iota2()`](compute_iota2.md). Please check that documentation.

## References

Florian Berding and Julia Pargmann (2022).Iota Reliability Concept of
the Second Generation. Measures for Content Analysis Done by Humans or
Artificial Intelligences. Berlin:Logos. https://doi.org/10.30819/5581
