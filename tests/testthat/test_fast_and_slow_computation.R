
skip("Comparisaton of fast and slow estimation")
test_that("Comparisaton of fast and slow estimation", {

  expect_equal(
    check_new_rater(
      true_values = iotarelr_written_exams$`Coder A`,
      assigned_values = iotarelr_new_rater,
      con_random_starts = 200,
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
      con_random_starts = 200,
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
      random_starts = 200,
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
      random_starts = 200,
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
