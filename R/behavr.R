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

behavr <- function(x, metadata){
  #todo, d[,t] should be hms
  #out <- new("behavr", x)
  # todo, run checks on meta data and data
  data.table::setattr(x,"metadata",metadata)
  new("behavr", x)
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
      mc <- match.call()
      print(1111)
      #mc$x <- substitute(S3Part(x, strictS3 = TRUE))
      dt <- S3Part(x, strictS3 = TRUE)
      # find out if we have inline asignment here... :|
      behavr(dt[...], metadata=meta(x))

      #here, warning if no time?!
      }

# set.seed(1)
# met <- data.table::data.table(id = 1:5, condition=letters[1:5], sex=c("M","M","M","F", "F"), key="id")
# t <- 1L:100L
# data <- met[,list(t=t, x=rnorm(100),y=rnorm(100), eating=runif(100) > .5 ),by="id"]
#
# d <- behavr(data,met)
# d[1,1]
