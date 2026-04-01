
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
