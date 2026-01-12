# Plot Iota2

Function for creating a plot object that can be plotted via 'ggplot2'.

## Usage

``` r
plot_iota(
  object,
  xlab = "Amount on all cases",
  ylab = "Categories",
  liota = "Assignment of the true category (Iota)",
  lcase2 = "Assignment to the false category",
  lcase3 = "Assignment from the false true category",
  lscale_quality = "Scale Quality",
  lscale_cat = c("insufficent", "minimum", "satisfactory", "good", "excellent"),
  number_size = 6,
  key_size = 0.5,
  text_size = 10,
  legend_position = "bottom",
  legend_direction = "vertical",
  scale = "none"
)
```

## Arguments

- object:

  Estimates of Iota 2 created with
  [`compute_iota2()`](compute_iota2.md), [`check_dgf()`](check_dgf.md)
  or [`check_new_rater()`](check_new_rater.md).

- xlab:

  `Character` passed to xlab() from scale_fill_manual(). Label of the
  x-axis.

- ylab:

  `Character` passed to ylab() from scale_fill_manual(). Label of the
  y-axis.

- liota:

  `Character` passed to labels() from scale_fill_manual(). Label for
  Iota.Amount of cases that are assigned to the correct category.

- lcase2:

  `Character` passed to labels() from scale_fill_manual(). Label for the
  amount of cases that are assigned to a false category.

- lcase3:

  `Character` passed to labels() from scale_fill_manual(). Label for the
  amount of cases that are assigned from a false category.

- lscale_quality:

  `character` passed to scale_fill_manual() determining the title for
  the quality of a scale. Only used in conjunction with `scale`.

- lscale_cat:

  Vector of strings with length 5. This vector contains the labels for
  each category of quality for the scale.

- number_size:

  `Double` passed to geom_text() determining the size of the numbers
  within the plot.

- key_size:

  `Double` passed to theme() determining the size of the legend keys.

- text_size:

  `Double` passed to theme() determining the size of the text within the
  legend.

- legend_position:

  `string` Position of the legend. Possible values are
  "bottom","right","left", "top" and "none".

- legend_direction:

  `string` Layout of the items in the legend. Possible values are
  "horizontal" and "vertical".

- scale:

  `String` for requesting an additional plot of reliability on the scale
  level. If `scale="dynamic_iota_index"` Dynamic Iota Index is used. If
  `scale="static_iota_index"` Static Iota Index is used. If
  `scale="none"` no additional plot is created.

## Value

Function returns an object of class `gg, ggplot` illustrating how the
data of the different categories influence each other.

## References

Florian Berding and Julia Pargmann (2022).Iota Reliability Concept of
the Second Generation. Measures for Content Analysis Done by Humans or
Artificial Intelligences. Berlin: Logos. https://doi.org/10.30819/5581
