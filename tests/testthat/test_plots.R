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
  testthat::expect_s3_class(plot_iota(test_iota2,
                                      scale="static_iota_index"),
                            c("gtable","gTree","grob", "gDesc")
  )
  testthat::expect_s3_class(plot_iota(test_iota2_new_rater,
                                      scale="static_iota_index"),
                            c("gtable","gTree","grob", "gDesc")
  )
  testthat::expect_s3_class(plot_iota(test_iota2_dgf,
                                      scale="static_iota_index"),
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
