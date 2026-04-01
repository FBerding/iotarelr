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



