# Estimate Expected Categories

Function for estimating the expected category of coding units.

## Usage

``` r
est_expected_categories(data, aem)
```

## Arguments

- data:

  `Matrix` which contains the codings for every coding unit. The coding
  units must be in the rows and the raters must be in the columns. At
  least two raters are necessary.

- aem:

  Assignment Error Matrix based on the second generation of the Iota
  Concept (Iota2).

## Value

Returns a `matrix` with the original data, the conditioned probability
of each true category, and the expected category for every coding unit.

## References

Florian Berding and Julia Pargmann (2022).Iota Reliability Concept of
the Second Generation. Measures for Content Analysis Done by Humans or
Artificial Intelligences. Berlin:Logos. https://doi.org/10.30819/5581
