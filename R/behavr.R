# @importFrom methods setOldClass
#setOldClass(c("hms", "difftime"))

#library(data.table)
#' @importFrom data.table data.table
#' @importFrom methods setOldClass
setOldClass(c("behavr", "data.table"))

#' todo
#'
#' todo
#'
#' @name behavr
#' @examples
#' set.seed(1)
#' met <- data.table::data.table(id = 1:5,
#'                               condition=letters[1:5],
#'                               sex=c("M","M","M","F", "F"),
#'                               key="id")
#' t <- hms::as.hms(1L:100L)
#' data <- met[  ,
#'               list(t=t,
#'                   x=rnorm(100),
#'                   y=rnorm(100),
#'                   eating=runif(100) > .5 ),
#'               by="id"]
#'
#' d <- behavr(data,met)
#' print(d)
#' summary(d)
NULL

# Construction ------------------------------------------------------------

#' @rdname behavr
#' @param x [data.table] containing all measurments
#' @param metadata [data.table] containing the metadata
#' @param deep_copy logical defining whether the data is to be deep copied (the default). Otherwise, `x` and `metadata` are liked by reference.
#' @details Both `x` and `metadata` should have a **column as a key** with **the same name** (typically named `id`).
#' @export
behavr <- function(x, metadata, deep_copy=T){
  #todo, check input type/key/column names...!!
  # todo, run checks on meta data and data
  data.table::setattr(x,"metadata",metadata)
  data.table::setattr(x,"class",c("behavr","data.table","data.frame"))
}


meta <- function(d, with_n=T, with_t_range=T){
  attr(d,"metadata")
}


#-------------- Indexing

# @inheritDotParams data.table:::`[.data.table`
# @rdname behavr
"[.behavr" <- function(x, ...){
 out <- NextMethod()
 behavr(out, metadata = meta(x), deep_copy = F)
}
