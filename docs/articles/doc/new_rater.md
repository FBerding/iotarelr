# 5) Training New Raters

## 1 Introduction

Many scientific studies include the development of a new coding scheme
for their study which is typically the focus of introductory literature
of content analysis (Krippendorff, 2019; Kuckartz, 2018; Mayring, 2015;
Schreier, 2012). In these cases, the reliability can be estimated and
analyzed as described in the first [vignette](iotarelr.md). In contrast
to the literature for new coding schemes, less recommendations refer to
the application of existing coding schemes which are available from
published studies or other sources. In these cases, it is not necessary
to develop a new scheme, but rather to apply an existing one to new
data, often along with new raters. The application of an existing coding
scheme to new data has several reasons.

- First, studies using the same coding scheme can be directly compared,
  contributing to knowledge accumulation in a specific topic or
  discipline.
- Second, applying an existing coding schemes provides the opportunity
  to prove the results of prior studies by trying to reproduce them.
- Third, using an existing scheme saves resources, since ideally, the
  improvement cycle for developing is not necessary. This saves
  capacities which then become available for other important study
  characteristics (greater sample sizes, refining specific categories,
  considering more categories etc.).

Estimating the reliability of codings based on an existing coding scheme
differs from estimating the reliability in the development a coding
scheme. The phase of developing a coding scheme aims to provide a
theoretically and empirically sound guide for data analysis. This
includes that researchers and raters clear up their own interpretation
of categories and data and develop the same understanding of the
categories based on theory and empircal evidence. Ideally, the final
coding scheme is precise enough to document this shared understanding
and to guide users to the same interpretation of data and the same
assignments of data to categories.

In contrast, the phase of applying an existing coding scheme by new
raters does not aim to incorporate the interpretation of data and
categories of the new raters. The aim is to train new raters to achieve
the understanding of data and categories documented in the scheme. The
existing coding scheme represents an already discussed and validated
understanding, which new raters have to acquire in order to apply the
coding scheme in the same way as in its development study or any other
preliminary study. As a consequence, reliability estimation has to
consider the existence of a predefined understanding.

## 2 Estimating the quality of ratings for a new rater

Within the frame work of the *Iota Concept*, this is realized with the
function [`check_new_rater()`](../../reference/check_new_rater.md). In
order to estimate how well a new rater has developed the understanding
documented in a coding scheme, data and material from existing sources
must be used. The new rater assigns the material to categories and the
assignments are compared to the existing assignments of the material.
Based on this data, the *Assignment Error Matrix* for the new rater can
be calculated.

To illustrate this process, we continue the example from the first
[vignette](iotarelr.md). First, we simulate an “old” study by
calculating Iota2 for the example. Second, we calculate the most likely
true category for each coding unit. These assignments serve us as “true”
categories. In practice, these steps do not have to be performed in
every case. If an old source provides the final assignments of coding
units, these values can be used as “true” values.

``` r
library(iotarelr)
res_iota2<-compute_iota2(data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")],
                         random_starts = 2,
                         trace = FALSE)
expected_categories<-est_expected_categories(
  data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")],
  aem=res_iota2$categorical_level$raw_estimates$assignment_error_matrix)
true_values<-expected_categories$expected_category
```

Third, a new rater has to rate the same material as in the old source.
That is, the new rater has to rate the same exams with the same coding
scheme. His results are saved in the vector `iotarelr_new_rater`. Now,
we can request the reliability estimate for this rater by requesting the
function [`check_new_rater()`](../../reference/check_new_rater.md).

``` r
res_new_rater<-check_new_rater(
  true_values = true_values,
  assigned_values = iotarelr_new_rater)
```

The resulting object is of the class `iotarelr_iota2`. Thus, we can use
the function [`get_summary()`](../../reference/get_summary.md) to look
into the results.

``` r
get_summary(res_new_rater)
#> Summary
#> 
#> Call: check_new_rater
#> 
#> Number of Raters: 1
#> Number of Categories: 3
#> Categories: average,good,poor
#> Number of Coding Units: 318
#> 
#> Random Start: NA
#> Log-Likelihood: NA
#> 
#> 
#> Primary Parameters
#> Assignment Error Matrix
#>         average  good  poor
#> average   0.357 0.286 0.357
#> good      0.205 0.397 0.397
#> poor      0.313 0.178 0.509
#> 
#> Categorical Sizes
#> average    good    poor 
#>   0.242   0.245   0.513 
#> 
#> Categorical Level
#>           dimensions average   good   poor
#> 1              Alpha  0.3571 0.3974 0.5092
#> 2               Beta  0.4725 0.6062 0.3938
#> 3               Iota   0.191 0.2403 0.3747
#> 4  Iota Error Type I  0.3438 0.3643 0.3612
#> 5 Iota Error Type II  0.4653 0.3954 0.2641
#> 
#> Scale Level
#> Iota Index: 0.2
#> Static Iota Index: 0.004
#> Dynamic Iota Index: 0.187
```

Focusing on the *Assignment Error Matrix*, the values on the diagonal
are very high. This leads to high vales for the *Alpha Reliability*,
which is about 78.73% for average exams, about 85.90% for good exams and
about 93.24% for poor exams. Thus, the chance is very high for this new
rater to assign the correct categories. Please note that the
interpretation of these values is now slightly different from the
development phase. Here, the high values indicate that the new rater has
a high chance to assign the same categories to a coding unit as in the
old study.

This is also documented in the values for *Iota* which range between
.6735 for average exams to .8334 for the poor exams. The *Dynamic Iota
Index* is quite high with .716. Finally, we can plot the results for the
new rater.

``` r
plot_iota(res_new_rater)
```

![Figure 1: Plot of Iota for a New
Rater](new_rater_files/figure-html/unnamed-chunk-4-1.png)

Figure 1: Plot of Iota for a New Rater

The plot of *Iota* for the new rater emphasizes that he is able to
recover a large number of true categories for all kinds of exams. Only
in the case of average exams, some coding units belonging to that
category are not part of the labeled data. Here, nearly the same number
of forgotten average exams is compensated by coding units from other
categories. Thus, on average, the number of coding units in the labeled
data for the average exams equals their true number, but on the
individual level there are errors.

## 3 Conclusion

The function [`check_new_rater()`](../../reference/check_new_rater.md)
provides detailed insights into how a new rater has acquired their
understanding of an existing coding scheme. This information can be used
to individually guide the training process of new raters, since the
*Assignment Error Matrix* and *Iota Reliability* provide hints where
errors occur and where everything is fine. This can increase the quality
of codings and saves both costs and time, making the replication and
comparison of studies using content analysis more easy.

## References

- Früh, W. (2017). Inhaltsanalyse: Theorie und Praxis (9., überarbeitete
  Auflage). UTB.
- Krippendorff, K. (2019). Content Analysis: An Introduction to Its
  Methodology (4th Ed.). SAGE.
- Kuckartz, U. (2018). Qualitative Inhaltsanalyse: Methoden, Praxis,
  Computerunterstützung (4. Auflage). Grundlagentexte Methoden. Beltz.
- Mayring, P. (2015). Qualitative Inhaltsanalyse: Grundlagen und
  Techniken (12., überarbeitete Auflage). Beltz.
- Schreier, M. (2012). Qualitative Content Analysis in Practice. SAGE.
