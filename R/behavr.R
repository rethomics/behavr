#' @importFrom data.table data.table
#' @importFrom methods setOldClass
setOldClass(c("behavr", "data.table"))

#' An S3 class, based on [data.table], to store ethomics data
#'
#' In modern behavioural biology,
#' it is common to record long time series of several *variables* (such as position, angle, fluorescence and so on) on multiple individuals.
#' In addition to large multivariate time series, each individual is associated with a set of
#' *metavariables* (i.e. sex, genotype, treatment and lifespan ), which, together, form the *metadata*.
#' Metavariables are crucial in so far as they generally "contain" the biological question.
#' During analysis, it is therefore important to be able to access, alter and compute interactions between both variables and metavariables.
#' `behavr` is a class that facilitates manipulation and storage of metadata and data in the same object.
#' It is designed to be both memory-efficient and user-friendly. 
#' For instance, it abstracts joins between data and metavariables.
#'
#' @name behavr
#' @seealso
#' * the `behavr` [webpage](https://github.com/rethomics/behavr)
#' * [data.table] -- on which `behavr` is based
#' * [xmv] -- to join metavariables
#' * [rejoin] -- to join all metadata
#' * [bind_behavr_list] -- to merge several `behavr` tables
#' @examples
#' set.seed(1)
#' met <- data.table::data.table(id = 1:5,
#'                               condition = letters[1:5],
#'                               sex = c("M", "M", "M", "F", "F"),
#'                               key = "id")
#' data <- met[  ,
#'               list(t = 1L:100L,
#'                   x = rnorm(100),
#'                   y = rnorm(100),
#'                   eating = runif(100) > .5 ),
#'               by = "id"]
#'
#' d <- behavr(data, met)
#' print(d)
#' summary(d)
NULL

# Construction ------------------------------------------------------------

#' @rdname behavr
#' @param x [data.table] containing all measurments
#' @param metadata [data.table] containing the metadata
#' @details Both `x` and `metadata` should have a **column set as key** with **the same name** (typically named `id`).
#' @export
behavr <- function(x, metadata){
  check_conform(x, metadata)
  m <- data.table::copy(metadata)
  out <- data.table::copy(x)
  data.table::setattr(out,"metadata",m)
  data.table::setattr(out,"class",c("behavr","data.table","data.frame"))
  return(out)
}

#' @noRd
#' @export
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
    if(is.vector(out))
      return(out)
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


#' @rdname behavr
#' @export
is.behavr <- function(x){
  data.table::is.data.table(x) & "behavr" %in% class(x)
 # check conform here?
}

#' Print and summarise a [behavr] table
#'
#' @name print.behavr
#' @param x,object [behavr] table
#' @param ... arguments passed on to further method
#' @seealso
#' * [behavr] -- to generate x
#' * [print.default]
#' * [summary.default]
#' @export
print.behavr <- function(x,...){
    cat("\n ==== METADATA ====\n\n")
    print(x[meta=TRUE],class=TRUE,...)
    cat("\n ====== DATA ======\n\n")
    NextMethod(x, class=TRUE,...)
}

#' @rdname print.behavr
#' @export 
summary.behavr <- function(object, ...){
    
    met <- object[meta=TRUE]
    n_key <- length(data.table::key(met))
    n_mvar <- ncol(met) -  n_key
    n_var <- ncol(object) -  n_key
    
   cat("behavr table with:\n")
    cat(sprintf(" %i\tindividuals\n", nrow(met)))
    cat(sprintf(" %i\tmetavariables\n", n_mvar))
    cat(sprintf(" %i\tvariables\n", n_var))
    cat(sprintf(" %i\tkey (%s)\n", n_key, paste(data.table::key(met),collapse=", ")))

    cat("\n Summary of each individual (one per row):\n")
    if(!"t" %in% colnames(object))
        sum_dt <- object[, .(data_points =.N),by=id]
    else
        sum_dt <- object[, .(data_points =.N,
                             time_range = sprintf("[%s -> %s (%s)]",min(t), max(t),  max(t) -min(t))),by=id]
    print(rejoin(sum_dt))
}
