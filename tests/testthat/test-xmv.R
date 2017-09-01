context("xmv")

test_that("metadata can be mapped", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)

  d[, .(xmv(sex))]

  expect_equivalent(d[xmv(sex) == "M"],
                   data[met[sex=="M"]][,colnames(data),with=F])

  expect_equal(d[,.(sex=xmv(sex))],
               data[met][,.(sex)])

  # we remove metadata
  # Variables should be NA for missing metadata now

  setmeta(d, meta(d)[sex=="M"])
  expect_true(nrow(d[xmv(sex) == "F"])==0)
  d[, condition:= xmv(condition)]
  expect_identical(ifelse(data[met][,sex] == "M", data[met][,condition], NA), d$condition)
})


test_that("xmv cannot be called from outside of `[]`, and only on `behavr` tables", {
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")

  expect_error(xmv(1), "only be called from inside the `\\[\\]`")
  expect_error(xmv(), "only be called from inside the `\\[\\]`")
  expect_error(xmv(letters), "only be called from inside the `\\[\\]`")


  expect_error(met[xmv(1)], "not a behavr object")

})


test_that("missing variables", {
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)
  expect_error(d[xmv(w)], "No metavariable named")

})

