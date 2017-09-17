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

