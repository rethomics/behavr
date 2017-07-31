# @importFrom methods setOldClass
#setOldClass(c("hms", "difftime"))

#library(data.table)
#' @importFrom data.table data.table
setClass("behavr", contains = "data.table")

#' todo
#'
#' todo
#'
#' @name behavr
#' @examples
#' \dontrun{
#'   # Will raise an error
#'   #todo
#' }
NULL

# Construction ------------------------------------------------------------

#' @rdname behavr
#' @details todo
#' @param data todo
#' @param metadata todo
#' @export
#library(data.table)
#setClass("behavr", contains = "data.table")
behavr <- function(x, metadata){
  #todo, check input type/key/column names...!!
  # todo, run checks on meta data and data
  data.table::setattr(x,"metadata",metadata)
  data.table::setattr(x,"class",c("behavr","data.table","data.frame")) 
}

meta <- function(d, with_n=T, with_t_range=T){
  attr(d,"metadata")
}

xmd <- function(var){
  mc <- match.call(envir = parent.frame(n=2))
  # todo check we call from within a [] env
  d <- get("x",envir=parent.frame(n=3))
  var <- deparse(substitute(var))
  md <- meta(d)
  col <- md[,c("id",var), with=F]
  d[,.(id)][col][,var, with=F][[1]]
}

#-------------- Indexing

"[.behavr" <- function(x,...){
 out <- NextMethod()
 behavr(out,metadata=meta(x))
}
