context("rejoin")

test_that("rejoin works", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)

  expect_equivalent(met[data], rejoin(d))

})

test_that("fails when not called on a behavr", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)

  expect_error(rejoin(data), "not a behavr")

  data.table::setattr(d,"metadata", NULL)
  expect_error(rejoin(d), "no metadata")
})
