# Get Iota2 Measures

Function for calculating the elements of the Iota Concept 2

## Usage

``` r
get_iota2_measures(aem, categorical_sizes, categorical_levels)
```

## Arguments

- aem:

  Assignment Error Matrix.

- categorical_sizes:

  Probabilities for the different categories to occur.

- categorical_levels:

  `Vector` containing all possible categories of the content analysis.

## Value

Returns a `list` of all measures belonging to the Iota Concept of the
second generation. The first component `estimates_categorical_level`
comprises all elements that describe the ratings on a categorical level.
The elements are sub-divided into raw estimates and chance-corrected
estimates.

- `raw_estimates`:

  `iota:`

  :   A vector containing the Iota values for each category.

  `iota_error_1:`

  :   A vector containing the Iota Error Type I values for each
      category.

  `iota_error_2:`

  :   A vector containing the Iota Error Type II values for each
      category.

  `alpha_reliability:`

  :   A vector containing the Alpha Reliabilities for each category.
      These values represent probabilities.

  `beta_reliability:`

  :   A vector containing the Beta Reliabilities for each category.
      These values represent probabilities.

  `assignment_error_matrix:`

  :   Assignment Error Matrix containing the conditional probabilities
      for assigning a unit of category i to categories 1 to n.

- `elements_chance_corrected`:

  `alpha_reliability:`

  :   A vector containing the chance-corrected Alpha Reliabilities for
      each category.

  `beta_reliability:`

  :   A vector containing the chance-corrected Beta Reliabilities for
      each category.

The second component `estimates_scale_level` contains elements for
describing the quality of the ratings on a scale level. It comprises the
following elements:

- `iota_index:`:

  The Iota Index, representing the reliability on a scale level.

- `iota_index_d4:`:

  The Static Iota Index, which is a transformation of the original Iota
  Index, in order to consider the uncertainty of estimation.

- `iota_index_dyn2:`:

  The Dynamic Iota Index, which is a transformation of the original Iota
  Index, in order to consider the uncertainty of estimation.

## References

Florian Berding and Julia Pargmann (2022).Iota Reliability Concept of
the Second Generation. Measures for Content Analysis Done by Humans or
Artificial Intelligences. Berlin: Logos. https://doi.org/10.30819/5581
