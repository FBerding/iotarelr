# 3) Different Guidance Functioning

## 1 Introduction

In this vignette, we would like to show you how the tools of the *Iota
Concept* can be used to analyze the suitability of a coding scheme for
rating different kinds of materials. The analysis is very similar to the
analysis of different item functioning (DIF) from item response theory.
The focal point here is the question whether or not a coding scheme has
the same degree of reliability for different groups of materials. This
issue is closely connected to the question if a coding scheme reproduces
bias for specific groups.

If a coding scheme is perceived as a measurement instrument, like a test
or questionnaire, it must be reliable and valid. Reliability and
validity demand the absence of different guidance functioning (DGF),
since the existence of DGF means that the data is influenced by other
sources, as for example the phenomenon of interest.

We use the term “different guidance functioning” in order to eliminate
this kind of analysis from DIF. Furthermore, we would like to express
that a coding scheme usually only provides a guideline for raters in
assessing a phenomenon.

The analysis of DIF is important for a broad range of applications. For
example,

- ensuring that ratings of newspapers do not prefer or discourage
  specific political parties.
- ensuring that books in different languages are rated without the
  influence of the different languages.
- ensuring that social media contributions are rated similarly without
  bias for the platform-specific form of contribution.
- ensuring that material created by different groups of learners is
  assessed without bias from their socioeconomic background.
- …

In the following, we would like to illustrate this kind of analysis by
continuing the example from the first [vignette](iotarelr.md). In order
to conduct an analysis of DGF, we need additional data that contains
information on how the different coding units are grouped. Let us have a
look at the sample data set.

## 2 Example Analysis

``` r
library(iotarelr)
head(iotarelr_written_exams)
#>   Coder A Coder B Coder C    Sex
#> 1 average average    good female
#> 2 average    poor average   male
#> 3    poor average    poor female
#> 4 average average average female
#> 5    poor average    good female
#> 6    poor    poor average female
```

The data set contains the ratings of exams from three raters and an
additional column called “Sex”. This column stores the information if
the participant of the exam identifies as male or a female. Since an
exam should only measure participants’ performance and should be fair,
the *Assignment Error Matrix* in both groups should not differ. That is,
men and women should have the same chance for a good, an average or a
poor exam.

The corresponding analysis can be requested via the function
[`check_dgf()`](../../reference/check_dgf.md). This function is similar
to [`compute_iota2()`](../../reference/compute_iota2.md) but has an
additional argument, `splitcr`. This argument uses the group information
for the analysis while `data` only uses the ratings. Please note that
calling this function may take some time.

``` r
dgf_exam<-check_dgf(data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")],
                    splitcr = iotarelr_written_exams$Sex,
                     random_starts = 2)
```

The results are stored in the object `dgf_exam`. For every group
specified in the column “Sex” a model of Iota2 is fitted. The results an
be accessed in the same way as for objects created with
[`compute_iota2()`](../../reference/compute_iota2.md). The only thing to
do is to request the function
[`get_summary()`](../../reference/get_summary.md)with the results for
the specific group. Let’s start with the females.

``` r
get_summary(dgf_exam$group_female)
#> Summary
#> 
#> Call: compute_iota2
#> 
#> Number of Raters: 3
#> Number of Categories: 3
#> Categories: average,good,poor
#> Number of Coding Units: 173
#> 
#> Random Start: 2
#> Log-Likelihood: 549.189068930579
#> The best log-likelihood has been replicated.
#> 
#> Primary Parameters
#> Assignment Error Matrix
#>         average  good  poor
#> average   0.887 0.113 0.000
#> good      0.252 0.507 0.241
#> poor      0.441 0.052 0.507
#> 
#> Categorical Sizes
#> average    good    poor 
#>   0.138   0.621   0.241 
#> 
#> Categorical Level
#>           dimensions average   good   poor
#> 1              Alpha  0.8869 0.5069 0.5066
#> 2               Beta   0.382 0.7897 0.5348
#> 3               Iota  0.3061 0.4848 0.3124
#> 4  Iota Error Type I   0.039 0.4716 0.3042
#> 5 Iota Error Type II  0.6549 0.0436 0.3833
#> 
#> Scale Level
#> Iota Index: 0.378
#> Static Iota Index: 0.078
#> Dynamic Iota Index: 0.329
```

The summary for the females show the basic information of the fitted
model. The best log-likelihood has been replicated. Thus, we can be
confident that our model represents the best model for the females.

Special attention should be paid to the *Assignment Error Matrix*, since
this matrix describes how the true categories are assigned. For the
females, an average exam is assigned as indeed an average exam in about
88.7% of cases. A good exam is assigned a good exam in about 50% of
cases. The remaining 50% of cases are assigned as an average exam or as
a poor exam with the same probability of 25%. A truly poor exam is
assigned as a poor exam in about 50% of cases and nearly in all other
cases as an average exam. The error to assign a poor exam as a good exam
is only about 5%. Let us now have a look at the results for the men.

``` r
get_summary(dgf_exam$group_male)
#> Summary
#> 
#> Call: compute_iota2
#> 
#> Number of Raters: 3
#> Number of Categories: 3
#> Categories: average,good,poor
#> Number of Coding Units: 145
#> 
#> Random Start: 2
#> Log-Likelihood: 461.504035085927
#> The best log-likelihood has not been replicated. Increase the
#>       number of random stars and/or inspect the Assignment Error Matrices and
#>       categorical sizes.
#> 
#> Primary Parameters
#> Assignment Error Matrix
#>         average  good  poor
#> average   0.545 0.260 0.196
#> good      0.333 0.333 0.333
#> poor      0.089 0.159 0.752
#> 
#> Categorical Sizes
#> average    good    poor 
#>   0.578   0.238   0.184 
#> 
#> Categorical Level
#>           dimensions average   good   poor
#> 1              Alpha  0.5448 0.3333 0.7518
#> 2               Beta  0.5317  0.419  0.544
#> 3               Iota  0.4675 0.1899 0.3679
#> 4  Iota Error Type I  0.3907 0.3797 0.1215
#> 5 Iota Error Type II  0.1418 0.4304 0.5106
#> 
#> Scale Level
#> Iota Index: 0.299
#> Static Iota Index: 0.035
#> Dynamic Iota Index: 0.268
```

For the men, the result is very different. According to their
*Assignment Error Matrix*, an average exam is assigned as an average
exam only in 54.4% of the cases. If it is not recognized as an average
exam, it has a higher chance to be considered as a good exam than than a
poor one. Concentrating on the good exams, the *Assignment Error Matrix*
shows the same probability for all categories. This implies that a good
exam is assigned randomly to any of the three categories. Only the poor
exams have a high chance to be assigned to the correct category with
about 75.1%. That is, a poor exam is assigned as a poor exam in three of
four cases.

Comparing the *Assignment Error Matrices*, it becomes clear that the
coding scheme is more reliable for females than for men.

How the different degrees of reliability affect the labeled data can be
explored with the function
[`plot_iota()`](../../reference/plot_iota.md). Here we can directly pass
the object `dgf_exam` to the function.

``` r
plot_iota(dgf_exam,
          ylab = "Groups")
```

![Figure 1: Plot of Iota for different
Groups](dgf_files/figure-html/unnamed-chunk-6-1.png)

Figure 1: Plot of Iota for different Groups

Let us start with the average exams of the females. The red rectangle
for this category implies that the data labeled as “average” contains a
lot of exams which truly are either good or poor exams (*Iota Error
II*). The red rectangle is even bigger than the green (*Iota*) and
orange ones (*Iota Error I*), meaning that the number of averagely good
exams is overestimated for the females. Concentrating on the males, the
red rectangle (*Iota Error II*) is quite small but the organ one (*Iota
Error I*) is quite big. Thus, the number of averagely good exams is
underestimated.

Regarding the good exams of the females, the red rectangle is quite
small (*Iota Error II*). This means that there is only a small number of
average or poor exams that are labeled as good. However, the orange
rectangle is quite big (*Iota Error I*). It has nearly the same size as
the green rectangle (*Iota*). Thus, the data for the good exams misses
about half of the corresponding exams. This means that the number of
good exams is underestimated in the data set for the females.
Concentrating on the males, *Iota* is only about .189, which is low.
Thus, the good exams are not reliably represented in the individual
data. In contrast, 37.7% of the labeled data are missing good exams
(*Iota Error I*). Instead, the data set is made up of 43.4% of exams
from other categories (*Iota Error II*). That is, from truly average or
truly poor exams. These exams compensate the missing good exams. As a
consequence, in the *average of all male participants*, the number of
good exams is actually quite correct for the males. On the *level of
single males*, the data is not reliable.

Finally, both diagrams for the poor exams look similar for both men and
women. For the women, the number of missing poor exams is higher than
for the men (*Iota Error I*). In contrast, the data labeled as poor
exams contains more exams which truly belong to the category “good” or
“average” for the men as well as the females (*Iota Error II*). Thus, in
*average over all female participants*, the number of poor exams is
quite correct as well. For the males, the number or poor exams is
overestimated. For men as well as for women, the data is not reliable on
the individual level.

## 3 Summary

To sum up, the coding scheme treats the exams of males and females
differently. The number of averagely good exams is overestimated for
females and underestimated for men. The number of good exams is
underestimated for females but quite accurate for men, although the
number has its source in the wrong exam categories. Finally, the number
of poor exams is quite correct for females, although it sources from
quite a lot of wrong exams, while the number of poor exams for the men
tends to be overestimated. Thus, in descriptive statistics, females
appear to have lower performance than they truly have. For men, the data
shows the tendency that they show either a good or a poor performance
but nothing in between.

This example shows how the guidance of the coding scheme can be analyzed
for different groups of materials. This information can help in
developing coding schemes that are free from bias, providing a more
reliable and valid source of data for subsequent analysis and
conclusion.
