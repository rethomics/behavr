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
# @param deep_copy logical defining whether the data is to be deep copied (the default). Otherwise, `x` and `metadata` are liked by reference.
#' @details Both `x` and `metadata` should have a **column as a key** with **the same name** (typically named `id`).
# #metadata is generally small, so it is always deep copied.
#' @export
behavr <- function(x, metadata){
  check_conform(x, metadata)
  m <- data.table::copy(metadata)
  out <- data.table::copy(x)
  data.table::setattr(out,"metadata",m)
  data.table::setattr(out,"class",c("behavr","data.table","data.frame"))
  return(out)
}

#' Retreive metadata
#'
#' This function returns the meta data from a [behavr] object
#' @param x a [behavr] object
#' @return a [data.table] representing the metadata in `x`
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
#' ##### Show metadata
#' meta(d)
#' class(d)
#' d2 <- d[id==1]
#' meta(d2)
#'
#' ##### Alter metadata
#' # meta(d)[, treatment := interaction(condition,sex)]
#' @seealso [behavr] to generate a `behavr` object, [xmd] to map metadata variables to data
#' @export
meta <- function(x){
  attr(x,"metadata")
}

#' @export
#' @noRd
"[.behavr" <- function(x, ...){
  check_conform(x)
 out <- NextMethod()
 # todo. here coerce to DT if not conform
 data.table::setattr(out,"metadata",meta(x))
 data.table::setattr(out,"class",c("behavr","data.table","data.frame"))
}

