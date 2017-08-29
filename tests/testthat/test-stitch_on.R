context("stitch")

test_that("stitch works", {
  set.seed(1)
  met1 <- data.table::data.table(uid = 1:5,id = 1:5,
                                condition = letters[1:5],
                                sex=c("M","M","M","F", "F"),
                                key="id")

  met2 <- data.table::data.table(uid = 1:4,id = 6:9,
                                condition = letters[1:4],
                                sex=c("M","M","M","F"),
                                key="id")

  met1[, datetime := as.POSIXct("2015-01-02")]
  met2[, datetime := as.POSIXct("2015-01-03")]
  met <- rbind(met1, met2)
  data.table::setkeyv(met, "id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]

  d <- behavr(data,met)
  d2 <- stitch_on(d, on ="uid")


  expect_equal(nrow(unique(d2, by =data.table::key(d2))), 5)
  expect_identical(d2[t > 100 & id == 1, x],  d[ id == 6, x])
  expect_identical(d2[t > 100 & id == 1, t],  d[ id == 6, t] + days(1))
  expect_equal(nrow(d2[t > 100 & id == 5]), 0)

  expect_identical(meta(d2), unique(met[order(datetime)], by="uid"))


  ## now, last part of the query is BEFORE the first part

  met1[, datetime := as.POSIXct("2015-01-02")]
  met2[, datetime := as.POSIXct("2015-01-01")]
  met <- rbind(met1, met2)
  data.table::setkeyv(met, "id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]

  d <- behavr(data,met)
  d2 <- stitch_on(d, on ="uid")

  expect_equal(nrow(unique(d2, by =data.table::key(d2))), 5)
  expect_identical(d2[t > 100 & id == 6, x],  d[ id == 1, x])
  expect_identical(d2[t > 100 & id == 6, t],  d[ id == 1, t] + days(1))
  expect_identical(d2[t > 100 & id == 6, t],  d[ id == 1, t] + days(1))
  expect_equal(nrow(d2[t > 100 & id == 5]), 0)
})


test_that("stitch fails when overlap", {
  set.seed(1)
  met <- data.table::data.table(uid = 1:5,id = 1:5,
                                condition = letters[1:5],
                                sex=c("M","M","M","F", "F"),
                                key="id")

  met2 <- data.table::data.table(uid = 1:4,id = 6:9,
                                 condition = letters[1:4],
                                 sex=c("M","M","M","F"),
                                 key="id")

  met[, datetime := as.POSIXct("2015-01-02")]
  met2[, datetime := as.POSIXct("2015-01-02")]
  met <- rbind(met, met2)
  data.table::setkeyv(met, "id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]

  d <- behavr(data,met)
  expect_error(stitch_on(d, on ="uid"), "overlap")

})






