test_iota1<-compute_iota1(
  data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")])
test_iota2<-compute_iota2(
  data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")],
  random_starts = 2,
  trace = FALSE)
test_iota2_dgf<-check_dgf(
  data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")],
  splitcr = iotarelr_written_exams$Sex,
  random_starts = 1,
  trace = FALSE)
test_iota2_new_rater<-check_new_rater(
  true_values = iotarelr_written_exams$`Coder A`,
  assigned_values = iotarelr_new_rater)
#------------------------------------------------------------------------------
test_that("object classes", {
  testthat::expect_s3_class(test_iota2,
                         "iotarelr_iota2"
                        )
  testthat::expect_s3_class(test_iota2_new_rater,
                         "iotarelr_iota2"
  )
  testthat::expect_s3_class(test_iota2_dgf,
                         "iotarelr_iota2_dgf"
  )
  testthat::expect_s3_class(test_iota1,
                         "iotarelr_iota1"
  )
})
#------------------------------------------------------------------------------
test_that("plots", {
  testthat::expect_s3_class(plot_iota(test_iota2),
                         c("gg","ggplot")
  )
  testthat::expect_s3_class(plot_iota(test_iota2_new_rater),
                         c("gg","ggplot")
  )
  testthat::expect_s3_class(plot_iota(test_iota2_dgf),
                         c("gg","ggplot")
  )

  testthat::expect_s3_class(plot_iota(test_iota2,
                                      scale="dynamic_iota_index"),
                          c("gtable","gTree","grob", "gDesc")
  )
  testthat::expect_s3_class(plot_iota(test_iota2_new_rater,
                            scale="dynamic_iota_index"),
                          c("gtable","gTree","grob", "gDesc")
  )
  testthat::expect_s3_class(plot_iota(test_iota2_dgf,
                            scale="dynamic_iota_index"),
                          c("gtable","gTree","grob", "gDesc")
  )
  testthat::expect_error(plot_iota(test_iota1)
  )
  testthat::expect_s3_class(plot_iota2_alluvial(test_iota2_new_rater),
                            c("gg","ggplot")

  )
  testthat::expect_s3_class(plot_iota2_alluvial(test_iota2),
                            c("gg","ggplot")
  )
  testthat::expect_error(plot_iota2_alluvial(test_iota1))
  testthat::expect_error(plot_iota2_alluvial(test_iota2_dgf))
})
#------------------------------------------------------------------------------
test_that("summary", {
  testthat::expect_output(get_summary(test_iota2))

  testthat::expect_output(get_summary(test_iota2_new_rater))

  testthat::expect_error(get_summary(test_iota2_dgf))
  testthat::expect_output(get_summary(test_iota2_dgf$group_female))
  testthat::expect_output(get_summary(test_iota2_dgf$group_male))

  testthat::expect_error(get_summary(test_iota1))
})
#------------------------------------------------------------------------------
test_that("expected categories", {
  test_exp_cat<-est_expected_categories(data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")],
                                        aem=test_iota2$categorical_level$raw_estimates$assignment_error_matrix)
  testthat::expect_s3_class(test_exp_cat,"data.frame")
  testthat::expect_equal(rowSums(test_exp_cat[c("prob_average",
                                                 "prob_good",
                                                 "prob_poor")]),
                         rep(x=1,times=nrow(test_exp_cat)))

})
#------------------------------------------------------------------------------
test_that("consequences nominal", {
  test_con_nominal_weak_dyn<-get_consequences(
    measure_typ = "dynamic_iota_index",
    measure_1_val = 1.00,
    level = .95,
    strength = "weak",
    data_type = "nominal",
    sample_size = 500)
  testthat::expect_equal(test_con_nominal_weak_dyn["deviation","practically no effect"],
                         .933)
  testthat::expect_equal(test_con_nominal_weak_dyn["deviation","practically weak effect"],
                         .999)
  testthat::expect_equal(test_con_nominal_weak_dyn["classification rate","practically no effect"],
                         .817)
  testthat::expect_equal(test_con_nominal_weak_dyn["classification rate","practically weak effect"],
                         .958)
  testthat::expect_equal(test_con_nominal_weak_dyn["risk of Type I errors","practically no effect"],
                         .833)
  testthat::expect_equal(test_con_nominal_weak_dyn["risk of Type I errors","practically weak effect"],
                         .964)

  test_con_nominal_medium_dyn<-get_consequences(
    measure_typ = "dynamic_iota_index",
    measure_1_val = 1.00,
    level = .95,
    strength = "medium",
    data_type = "nominal",
    sample_size = 500)
  testthat::expect_equal(test_con_nominal_medium_dyn["deviation","practically no effect"],
                         .849)
  testthat::expect_equal(test_con_nominal_medium_dyn["deviation","practically weak effect"],
                         .999)
  testthat::expect_equal(test_con_nominal_medium_dyn["classification rate","practically no effect"],
                         .002)
  testthat::expect_equal(test_con_nominal_medium_dyn["classification rate","practically weak effect"],
                         .064)
  testthat::expect_equal(test_con_nominal_medium_dyn["risk of Type I errors","practically no effect"],
                         .999)
  testthat::expect_equal(test_con_nominal_medium_dyn["risk of Type I errors","practically weak effect"],
                         .999)

  test_con_nominal_strong_dyn<-get_consequences(
    measure_typ = "dynamic_iota_index",
    measure_1_val = 1.00,
    level = .95,
    strength = "strong",
    data_type = "nominal",
    sample_size = 500)
  testthat::expect_equal(test_con_nominal_strong_dyn["deviation","practically no effect"],
                         .789)
  testthat::expect_equal(test_con_nominal_strong_dyn["deviation","practically weak effect"],
                         .998)
  testthat::expect_equal(test_con_nominal_strong_dyn["classification rate","practically no effect"],
                         .999)
  testthat::expect_equal(test_con_nominal_strong_dyn["classification rate","practically weak effect"],
                         .999)
  testthat::expect_equal(test_con_nominal_strong_dyn["risk of Type I errors","practically no effect"],
                         .999)
  testthat::expect_equal(test_con_nominal_strong_dyn["risk of Type I errors","practically weak effect"],
                         .999)


})
#------------------------------------------------------------------------------
test_that("consequences ordinal", {
  test_con_ordinal_weak_dyn<-get_consequences(
    measure_typ = "dynamic_iota_index",
    measure_1_val = 1.00,
    level = .95,
    strength = "weak",
    data_type = "ordinal",
    sample_size = 500)
  testthat::expect_equal(test_con_ordinal_weak_dyn["deviation","practically no effect"],
                         .877)
  testthat::expect_equal(test_con_ordinal_weak_dyn["deviation","practically weak effect"],
                         .999)
  testthat::expect_equal(test_con_ordinal_weak_dyn["classification rate","practically no effect"],
                         .584)
  testthat::expect_equal(test_con_ordinal_weak_dyn["classification rate","practically weak effect"],
                         .874)
  testthat::expect_equal(test_con_ordinal_weak_dyn["risk of Type I errors","practically no effect"],
                         .997)
  testthat::expect_equal(test_con_ordinal_weak_dyn["risk of Type I errors","practically weak effect"],
                         .999)

  test_con_ordinal_medium_dyn<-get_consequences(
    measure_typ = "dynamic_iota_index",
    measure_1_val = 1.00,
    level = .95,
    strength = "medium",
    data_type = "ordinal",
    sample_size = 500)
  testthat::expect_equal(test_con_ordinal_medium_dyn["deviation","practically no effect"],
                         .724)
  testthat::expect_equal(test_con_ordinal_medium_dyn["deviation","practically weak effect"],
                         .999)
  testthat::expect_equal(test_con_ordinal_medium_dyn["classification rate","practically no effect"],
                         .007)
  testthat::expect_equal(test_con_ordinal_medium_dyn["classification rate","practically weak effect"],
                         .197)
  testthat::expect_equal(test_con_ordinal_medium_dyn["risk of Type I errors","practically no effect"],
                         .999)
  testthat::expect_equal(test_con_ordinal_medium_dyn["risk of Type I errors","practically weak effect"],
                         .999)

  test_con_ordinal_strong_dyn<-get_consequences(
    measure_typ = "dynamic_iota_index",
    measure_1_val = 1.00,
    level = .95,
    strength = "strong",
    data_type = "ordinal",
    sample_size = 500)
  testthat::expect_equal(test_con_ordinal_strong_dyn["deviation","practically no effect"],
                         .617)
  testthat::expect_equal(test_con_ordinal_strong_dyn["deviation","practically weak effect"],
                         .993)
  testthat::expect_equal(test_con_ordinal_strong_dyn["classification rate","practically no effect"],
                         .645)
  testthat::expect_equal(test_con_ordinal_strong_dyn["classification rate","practically weak effect"],
                         .998)
  testthat::expect_equal(test_con_ordinal_strong_dyn["risk of Type I errors","practically no effect"],
                         .999)
  testthat::expect_equal(test_con_ordinal_strong_dyn["risk of Type I errors","practically weak effect"],
                         .999)


})
##Test input------------------------------------------------------------------
test_that("Input types for check new rater", {

  #assigned values as factor
  expect_equal(
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast = FALSE),
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = as.factor(iotarelr_new_rater),
      con_random_starts = 20,
      fast = FALSE),
    tolerance=1e-1
  )
  expect_equal(
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast = TRUE),
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = as.factor(iotarelr_new_rater),
      con_random_starts = 20,
      fast = TRUE)
  )

  #assigned values as data.frame
  expect_equal(
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast = FALSE),
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = as.data.frame(iotarelr_new_rater),
      con_random_starts = 20,
      fast = FALSE),
    tolerance=1e-1
  )
  expect_equal(
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast=TRUE),
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = as.data.frame(iotarelr_new_rater),
      con_random_starts = 20,
      fast=TRUE)
  )

  #true values as factor
  expect_equal(
    check_new_rater(
      true_values = as.factor(iotarelr_written_exams$`Coder A`),
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast = FALSE),
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast = FALSE),
    tolerance=1e-1
  )

  expect_equal(
    check_new_rater(
      true_values = as.factor(iotarelr_written_exams$`Coder A`),
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast=TRUE),
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast = TRUE)
  )

  #true values as data.frame
  expect_equal(
    check_new_rater(
      true_values = as.data.frame(iotarelr_written_exams$`Coder A`),
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast = FALSE),
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast=FALSE),
    tolerance=1e-2
  )
  expect_equal(
    check_new_rater(
      true_values = as.data.frame(iotarelr_written_exams$`Coder A`),
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast=TRUE),
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast=TRUE)
  )
})
#Test fast and slow estimation--------------------------------------------------
test_that("Comparisaton of fast and slow estimation", {

  expect_equal(
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast = FALSE)$categorical_level$raw_estimates$assignment_error_matrix,
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = as.factor(iotarelr_new_rater),
      con_random_starts = 20,
      fast = TRUE)$categorical_level$raw_estimates$assignment_error_matrix,
    tolerance=1e-1
  )

  expect_equal(
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 20,
      fast = FALSE)$information$est_true_cat_sizes,
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = as.factor(iotarelr_new_rater),
      con_random_starts = 20,
      fast = TRUE)$information$est_true_cat_sizes,
    tolerance=1e-1
  )

  expect_equal(
    compute_iota2(
      data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")],
      random_starts = 50,
      trace = FALSE,
      fast = FALSE)$categorical_level$raw_estimates$assignment_error_matrix,
    compute_iota2(
      data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")],
      random_starts = 20,
      trace = FALSE,
      fast=TRUE)$categorical_level$raw_estimates$assignment_error_matrix,
    tolerance=1e-1
  )

  expect_equal(
    compute_iota2(
      data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")],
      random_starts = 50,
      trace = FALSE,
      fast = FALSE)$information$est_true_cat_sizes,
    compute_iota2(
      data=iotarelr_written_exams[c("Coder A","Coder B","Coder C")],
      random_starts = 20,
      trace = FALSE,
      fast=TRUE)$information$est_true_cat_sizes,
    tolerance=1e-1
  )

})
