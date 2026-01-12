# Generating randomly chosen probabilities for categorical sizes

Function written in `C++` for generating a set of randomly chosen
probabilities describing the size of the different classes. The
probabilities describe the relative frequencies of the categories in the
data.

## Usage

``` r
get_random_start_values_class_sizes(n_categories)
```

## Arguments

- n_categories:

  Integer for the number of categories in the data. Must be at least 2.

## Value

Returns a vector of randomly chosen categorical sizes.
