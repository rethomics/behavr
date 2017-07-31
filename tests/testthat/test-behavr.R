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
  expect_identical(d$t, data$t) # we modified the reference, so data is changed too
  expect_identical(d$t,rep(t,5) + 1)
  expect_identical(attr(d, "metadata"), met)

  d[ id==1L, t:=t+1.0] # sub-assign
  expect_identical(d$t,c(t+2, rep(t,4) + 1))
  d[, x:=NULL] #remove columns
  expect_null(d$x)

  expect_identical(d[met], data[met])
})


#test_that("constructor is strict enough", {
 # set.seed(1)
 # met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
 # data <- met[,list(t=1L:100L, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
 # data[, id2:=id]

#  data.table::setkeyv(data, NULL)
#  data.table::setkeyv(met, "id")
#  expect_failure(behavr(data,met), message="data has no key")

#  data.table::setkeyv(data, "id")
#  data.table::setkeyv(met, NULL)
#  expect_failure(behavr(data,met), message="metadata has no key")

  # the new key, id2, is not the same as "meta"
 # data.table::setkeyv(data, "id2")
 # data.table::setkeyv(met, "id")
 # expect_failure(behavr(data,met), message="not the same key")

  #data.table::setkeyv(data, "id")
  #data.table::setkeyv(met, "id")


  #merge(data[1],met[1], all=T)
  #0expect_failure(behavr(data,met), message="not the same key")
  #d <- behavr(data,met)
  #meta(d[,sum(x),by=id])

#})
