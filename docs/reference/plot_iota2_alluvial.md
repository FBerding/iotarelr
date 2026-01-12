# Plot of the Coding Stream

Function for creating an alluvial plot that can be plotted via
'ggplot2'.

## Usage

``` r
plot_iota2_alluvial(
  object,
  label_titel = "Coding Stream from True to Assigned Categories",
  label_prefix_true = "true",
  label_prefix_assigned = "labeled as",
  label_legend_title = "True Categories",
  label_true_category = "True Category",
  label_assigned_category = "Assigned Category",
  label_y_axis = "Relative Frequencies",
  label_categories_size = 3,
  key_size = 0.5,
  text_size = 10,
  legend_position = "right",
  legend_direction = "vertical"
)
```

## Arguments

- object:

  Estimates of Iota 2 created with
  [`compute_iota2()`](compute_iota2.md),
  [`check_new_rater()`](check_new_rater.md) or with
  [`check_dgf()`](check_dgf.md). Please note that the object created by
  [`check_dgf()`](check_dgf.md) cannot be passed directly. Only the
  elements of the corresponding list are compatible.

- label_titel:

  `Character` containing the title of the plot.

- label_prefix_true:

  `Character` representing the prefix for tagging the true categories.
  Character is applied to every category.

- label_prefix_assigned:

  `Character` representing the prefix for tagging the assigned
  categories. Character is applied to every category.

- label_legend_title:

  `Character` containing the title of the legend.

- label_true_category:

  `Character` describing the stratum of true categories.

- label_assigned_category:

  `Character` describing the stratum of assigned categories.

- label_y_axis:

  `Character`. Label of the y-axis.

- label_categories_size:

  `double` determining the size of the label for each true and assigned
  category within the plot.

- key_size:

  `double` determining the size of the legend.

- text_size:

  `double` determining the size of the text within the legend.

- legend_position:

  `string` Position of the legend. Possible values are
  "bottom","right","left", "top" and "none".

- legend_direction:

  `string` Layout of the items in the legend. Possible values are
  "horizontal" and "vertical".

## Value

Returns an object of class `gg` and `ggplot` which can be shown with
[`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Note

An example for interpreting the plot can be found in the vignette [Get
started](../doc/iotarelr.md) or via
[`vignette("iotarelr", package = "iotarelr")`](../articles/iotarelr.md).
