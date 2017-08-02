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
#' data <- met[  ,
#'               list(t=1L:100L,
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


#' @export
#' @noRd
"[.behavr" <- function(x, ..., meta=FALSE){

  if(meta==TRUE){
    m <- data.table::copy(meta(x))
    old_key <- data.table::key(m)
    out <- m[...]
    if(!identical(old_key, data.table::key(out)))
       stop("You are trying to modify metadata in a way that removes its key. This is not allowed!")
    data.table::setattr(x,"metadata",m)
    return(out)
  }

  else{
    out <- NextMethod()
  }

 # coerce to DT if not conform
 if(!identical(data.table::key(out),data.table::key(x))){
   data.table::setattr(out,"metadata",NULL)
   data.table::setattr(out,"class",c("data.table","data.frame"))
 }
 else{
  data.table::setattr(out,"metadata",meta(x))
  data.table::setattr(out,"class",c("behavr","data.table","data.frame"))
  #check_conform(out)
 }
}
