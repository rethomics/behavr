context("xmd")

test_that("metadata can be mapped", {
  set.seed(1)
  met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
  t <- 1L:100L
  data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
  d <- behavr(data,met)

  expect_equivalent(d[xmd(sex) == "M"],
                   data[met[sex=="M"]][,colnames(data),with=F])

  expect_equal(d[,.(sex=xmd(sex))],
               data[met][,.(sex)])

  # we remove metadata
  # Variables should be NA for missing metadata now

  setmeta(d, meta(d)[sex=="M"])
  expect_true(nrow(d[xmd(sex) == "F"])==0)
  d[, condition:= xmd(condition)]
  expect_identical(ifelse(data[met][,sex] == "M", data[met][,condition], NA), d$condition)
})
