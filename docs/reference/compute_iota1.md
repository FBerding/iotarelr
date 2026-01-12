# Computes Iota and its elements in version 1

Computes all elements of the Iota Reliability Concept

## Usage

``` r
compute_iota1(data)
```

## Arguments

- data:

  Data for which the elements should be estimated. Data must be an
  object of type `data.frame` or `matrix` with cases in the rows and
  raters in the columns.

## Value

A list with the following components

- alpha:

  A vector containing the chance-corrected Alpha Reliabilities for every
  category.

- beta:

  A vector containing the chance-corrected Beta Reliabilities for every
  category.

- iota:

  A vector containing the Iota values for every category.

- assignment_error_matrix:

  A matrix with the conditional probabilities for every category. The
  rows refer to the true categories and the columns refer to the
  assigned categories. The elements on the diagonal represent the alpha
  errors of that category. The other elements in each row represent the
  conditioned probabilities that a coding unit is wrongly assigned to
  another category.

- average_iota:

  A numeric value ranging between 0 and 1, representing the Average Iota
  values on a categorical level. It describes the reliability of the
  whole scale.

## References

\- Berding, Florian, Elisabeth Riebenbauer, Simone Stuetz, Heike
Jahncke, Andreas Slopinski, and Karin Rebmann. 2022. Performance and
Configuration of Artificial Intelligence in Educational
Settings.Introducing a New Reliability Concept Based on Content
Analysis. Frontiers in Education.
https://doi.org/10.3389/feduc.2022.818365
