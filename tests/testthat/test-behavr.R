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
  d[id==1]
  # drop class when id is  lost
  expect_identical(class(d[,.(x)]), class(data))
  expect_identical(class(d[,.(x,y)]), class(data))


  #if id is kep, no coercion
  expect_identical(class(d[,.(id,x,y)]), class(d))
  expect_identical(class(d[,.(id)]), class(d))
  expect_identical(class(d[,.(id)]), class(d))
  #library(data.table)
  d[, id := NULL]
  expect_identical(class(d), class(data))
})


test_that("Getting/setting metadata with [] works", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  data <- met[,list(t=1L:100L, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)
  expect_identical(d[meta=T], met)
  expect_identical(d[id==1, meta=T], met[id==1])
  expect_error(d[, id:=1, meta=T], regex="not allowed")
  d[, lifespan:=1,meta=T]
  expect_identical(meta(d)$lifespan,rep(1, nrow(met)))
})


test_that("[,a] returns a vector", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  data <- met[,list(t=1L:100L, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)

  expect_equal(d[,eating], met[data]$eating)
  expect_equal(d[,mean(x)], mean(met[data]$x))

  # id is now a factor
  set.seed(1)
  met <- data.table::data.table(id = as.factor(1:5), condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  data <- met[,list(t=1L:100L, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)
  d[, id]

})


test_that("is.behavr works", {
  set.seed(1)

  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  data <- met[,list(t=1L:100L, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)
  expect_false(is.behavr(met))
  expect_false(is.behavr(data))
  expect_true(is.behavr(d))

})



test_that("filtering data updates metadata", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  data <- met[,list(t=1L:100L, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  # we update time in id=1, so we can exclude by time
  data[id == 1, t:= t+100L]
  d <- behavr(data,met)

  expect_message(d_small <- d[ t <=100, verbose=T], "removing 1 individual")
  expect_equal(nrow(d_small[, meta=T]), nrow(unique(data[t <= 100,data.table::key(data), with=FALSE])))

  expect_message(d_small <- d[ t > 100, verbose=T], "removing 4 individual")
  expect_equal(nrow(d_small[, meta=T]), nrow(unique(data[t > 100,data.table::key(data), with=FALSE])))


  d_small <- d[ t > 100]
  expect_equal(nrow(d_small[, meta=T]), nrow(unique(data[t > 100,data.table::key(data), with=FALSE])))


  # nothing to do
  d_small <- d[ t > 50]
  expect_equal(nrow(d_small[, meta=T]), nrow(unique(data[t > 50,data.table::key(data), with=FALSE])))
  d_small <- d[ t > 50, verbose=T]
  expect_equal(nrow(d_small[, meta=T]), nrow(unique(data[t > 50,data.table::key(data), with=FALSE])))

})

test_that("metadata columns can be extracted without id", {
  met = data.table(id=1, treatment="a", sex="f", key="id")
  dt <- toy_dam_data(metadata=met, duration=hours(1))
  dt_bak <- data.table::copy(dt)
  expect_equal(dt[,treatment, meta=T], met[,treatment])
  expect_equal(dt[,.(sex, treatment), meta=T], met[,.(sex, treatment)])
  expect_equal(dt[,.(id, treatment), meta=T], met[,.(id, treatment)])
  expect_equal(dt[, id, meta=T], met[, id])

  dt[, treatment:=NULL, meta=T]
  expect_equal(dt[, meta=T], met[,-"treatment"])
  expect_error(dt[, id:=NULL, meta=T], "that removes its key")

  setattr(dt_bak, "metadata", NULL)
  setattr(dt, "metadata", NULL)
  expect_equal(dt_bak, dt)

})


test_that("setmeta works (issue #29)", {
  met <- data.table::data.table(id = 1:5,
                              condition = letters[1:5],
                              sex = c("M", "M", "M", "F", "F"),
                              key = "id")
  data <- met[,
           list(t = 1L:100L,
                 x = rnorm(100),
                 y = rnorm(100),
                 eating = runif(100) > .5 ),
                by = "id"]
  
  setmeta(data, met)
  expect_true( all(c("behavr", "data.table") %in% class(data)))
  expect_equal(data[, meta=T], met)

})


#test_that("filtering metadata updates data", {
  # set.seed(1)
  # met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  # data <- met[,list(t=1L:100L, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  # # we update time in id=1, so we can exclude by time
  # data[id == 1, t:= t+100L]
  # d <- behavr(data,met)
  #
  # setmeta(d, meta(d)[id==1])
  # d
  # expect_message(setmeta(d, meta(d)[id==1]),"")

#})
