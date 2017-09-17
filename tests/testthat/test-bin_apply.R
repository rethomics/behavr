context("bin_apply")



test_that("bin_apply works", {
  dt <- toy_activity_data(duration = days(1))

  dt[, t_round := mins(30) * floor(t/mins(30))]
  expect_identical(
    dt[,mean(moving), by=t_round]$V1,
    dt_binned_expr <- bin_apply(dt[, -"id"], moving)$moving
  )

})



test_that("bin_apply works with experssions or strings ", {
  dt <- toy_activity_data(duration = days(1))

  dt_binned_str <- bin_apply(dt[, -"id"], y = "moving")
  dt_binned_expr <- bin_apply(dt[, -"id"], moving)
  dt_binned_expr_inv <- bin_apply(dt[, -"id"], !moving)
  dt_binned_expr_inv[, moving := 1  - `!moving`]
  dt_binned_expr_inv[, `!moving` := NULL]
  str <- "moving"
  dt_binned_str_var <- bin_apply(dt[, -"id"], y = str)

  moving = NULL
  dt_binned_null <- bin_apply(dt[, -"id"], moving)

  expect_equal(dt_binned_expr_inv, dt_binned_expr)
  expect_equal(dt_binned_expr_inv, dt_binned_str)
  expect_equal(dt_binned_expr_inv, dt_binned_str_var)
  expect_equal(dt_binned_expr_inv, dt_binned_null)

  dt_binned <- bin_apply(dt[, -"id"], y = "moving")
  dt_binned_t_str <- bin_apply(dt[, -"id"], y = "moving", x="t")
  dt_binned_t_expr <- bin_apply(dt[, -"id"], y = "moving", x=t)
  dt_binned_t2 <- bin_apply(dt[, -"id"], y = "moving", x=t+mins(30))
  dt_binned_t2[, t := `t + mins(30)` - mins(30)]
  dt_binned_t2[, `t + mins(30)` := NULL]
  setcolorder(dt_binned_t2, c("t", "moving"))
  expect_identical(dt_binned, dt_binned_t2)
  expect_identical(dt_binned, dt_binned_t_str)
  expect_identical(dt_binned, dt_binned_t_expr)

})

test_that("bin_apply_all works", {
  dt <- toy_activity_data(data.frame(id=1:3), duration = days(1))
  dt_binned_str <- bin_apply_all(dt, y = "moving")
  dt_binned_expr <- bin_apply_all(dt, y = moving)
  dt_binned_expr_inv <- bin_apply_all(dt, !moving)
  dt_binned_expr_inv[, moving := 1-`!moving`]
  dt_binned_expr_inv[, `!moving` := NULL]
  expect_equal(dt_binned_expr_inv, dt_binned_expr)
  expect_equal(dt_binned_expr_inv, dt_binned_str)
})
