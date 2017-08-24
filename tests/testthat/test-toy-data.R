context("toy_data")

test_that("toy data returns behavr object", {
  tdt <- toy_activity_data()

  expect_true("behavr" %in% class(tdt))
  behavr:::check_conform(tdt)

  tdt <- toy_ethoscope_data()
  expect_true("behavr" %in% class(tdt))
  behavr:::check_conform(tdt)

  tdt <- toy_dam_data()
  expect_true("behavr" %in% class(tdt))
  behavr:::check_conform(tdt)
})



# dt <- data.table::data.table(t = c(hms::hms(c(1:3, 1:4))),
#            id=c(1,1,1,2,2,2,2))
#
# dt[, max(t), by=id]
