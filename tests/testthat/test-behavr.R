context("behavr")

test_that("[] works", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)

  expect_identical(d[], d)
  expect_identical(attr(d[], "metadata"), met)

  expect_identical(d[t<50], behavr(data[t<50],met))
  expect_identical(attr(d[t<50], "metadata"), met)

  d[, t:=t+1]
  expect_identical(d$t, data$t +1) # behavr copy at construction, so no ref

  d2 <- d # d2 is a reference to d
  d2[, x:=x+1]
  expect_identical(d2, d)
  expect_identical(d$t,rep(t,5) + 1)
  expect_identical(attr(d, "metadata"), met)

  d[ id==1L, t:=t+1.0] # sub-assign
  expect_identical(d$t,c(t+2, rep(t,4) + 1))
  d[, x:=NULL] #remove columns
  expect_null(d$x)

  d <- behavr(data,met)
  expect_equivalent(as.data.frame(data[met]), as.data.frame(d[met]))
})


test_that("constructor is strict enough", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  data <- met[,list(t=1L:100L, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  data[, id2:=id]

  data.table::setkeyv(data, NULL)
  data.table::setkeyv(met, "id")
  expect_error(behavr(data,met), regex="x has no key")
  data.table::setkeyv(data, "id")
  data.table::setkeyv(met, NULL)
  expect_error(behavr(data,met), regex="metadata has no key")

  # the new key, id2, is not the same as "meta"
  data.table::setkeyv(data, "id2")
  data.table::setkeyv(met, "id")
  expect_error(behavr(data,met), regex="different key")

  data.table::setkeyv(data, "id")
  data.table::setkeyv(met, "id")
})




test_that("coercion to data.table when key is dropped", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  data <- met[,list(t=1L:100L, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)

  # drop class when id is  lost
  expect_identical(class(d[,.(x)]), class(data))
  expect_identical(class(d[,.(x,y)]), class(data))

  #if id is kep, no coercion
  expect_identical(class(d[,.(id,x,y)]), class(d))
  expect_identical(class(d[,.(id)]), class(d))
  expect_identical(class(d[,.(id)]), class(d))
})


test_that("Getting/setting metadata with [] works", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  data <- met[,list(t=1L:100L, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)
  expect_identical(d[id==1, meta=T], met[id==1])
  expect_error(d[, id:=1, meta=T], regex="not allowed")
  d[, lifespan:=1,meta=T]
  expect_identical(meta(d)$lifespan,rep(1, nrow(met)))
})
