context("rbindlist")

test_that("all_identical utility", {
  l1 <- list(a=letters[1:2], letters[1:2], letters[1:2])
  l2 <- list(a=letters[1:2], letters[1:2], letters[1])
  l3 <- list(a=letters[1:2], letters[1:2], letters[1:3])
  l4 <- list(a=letters[1:2])
  expect_true(behavr:::all_identical(l1))
  expect_false(behavr:::all_identical(l2))
  expect_false(behavr:::all_identical(l3))
  expect_true(behavr:::all_identical(l4))
})




test_that("bind_behavr_list works", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)
  met[,id:= id+5]
  data[,id:= id+5]
  data.table::setkeyv(met, "id")
  data.table::setkeyv(data, "id")
  d2 <- behavr(data,met)

  ok_list <- list(d, d2)
  d_all <- bind_behavr_list(ok_list)
  behavr:::check_conform(d_all)
  expect_equal(nrow(d_all[meta=TRUE]), 10)


  d3 <- behavr(data,met)
  d3[, w :=2]
  not_ok_list <- list(d, d3)
  expect_error(bind_behavr_list(not_ok_list), regex="data.*same columns")
  d3[, w :=NULL]

  m <- d3[meta=TRUE]
  setmeta(d3,m[, test:="A"])
  not_ok_list <- list(d, d3)
  expect_error(bind_behavr_list(not_ok_list), regex="metadata.*same columns")

  m[, test:=NULL]

  m[, id2 := id -1]
  d3[, id2 := id -1]
  data.table::setkeyv(m,"id2")
  data.table::setkeyv(d3,"id2")
  setmeta(d3,m)
  not_ok_list <- list(d, d3)
  expect_error(bind_behavr_list(not_ok_list), regex="metadata.*same key")

  # we create an intentional duplicate
  # id=5 is in both datasets
  met[, id := id -1]
  data[, id := id -1]
  data.table::setkeyv(met,"id")
  data.table::setkeyv(data,"id")
  d3 <- behavr(data,met)

  not_ok_list <- list(d, d3)
  expect_error(bind_behavr_list(not_ok_list), regex="[Dd]uplicated key")
})


test_that("bind_behavr_list works when data is missing", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)
  met[,id:= id+5]
  data[,id:= id+5]
  data.table::setkeyv(met, "id")
  data.table::setkeyv(data, "id")
  d2 <- behavr(data,met)

  ok_list <- list(d, NULL,d2)
  d_all <- bind_behavr_list(ok_list)
  d_all2 <- bind_behavr_list(list(d, d2))

  expect_identical(d_all, d_all2)

  no_data <- bind_behavr_list(list(NULL, NULL))
  expect_true(is.null(no_data))
  no_data <- bind_behavr_list(list())
  expect_true(is.null(no_data))

})
