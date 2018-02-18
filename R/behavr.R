setOldClass(c("behavr", "data.table"))

#' An S3 class, based on [data.table], to store ethomics data
#'
#' In modern behavioural biology,
#' it is common to record long time series of several *variables* (such as position, angle,
#' fluorescence and many others) on multiple individuals.
#' In addition to large multivariate time series, each individual is associated with a set of
#' *metavariables* (i.e. sex, genotype, treatment and lifespan ), which, together, form the *metadata*.
#' Metavariables are crucial in so far as they generally "contain" the biological question.
#' During analysis, it is therefore important to be able to access, alter and compute interactions
#' between both variables and metavariables.
#' `behavr` is a class that facilitates manipulation and storage of metadata and data in the same object.
#' It is designed to be both memory-efficient and user-friendly.
#' For instance, it abstracts joins between data and metavariables.
#' @details
#' A `behavr` table is a [data.table].
#' Therefore, it can be used by any function that would work on a [data.frame] or a [data.table].
#' Most of the operation such as variable creation, subsetting and joins are inherited from the [data.table]
#' `[]` operator, following the convention `DT[i,j,by]` (see data table package for detail).
#' These operations are applied on the data.
#' Metadata can be accessed using `meta=TRUE`: `DT[i,j,by, meta=TRUE]`,
#' which allows extraction of subsets, creation of metavariables, etc.
#' @name behavr
#' @seealso
#' * [data.table] -- on which `behavr` is based
#' * [xmv] -- to join metavariables
#' * [rejoin] -- to join all metadata
#' * [bind_behavr_list] -- to merge several `behavr` tables
#' @references
#' * The relevant [rethomic tutorial section](https://rethomics.github.io/behavr.html#variables-and-metavariables) -- about metavariables and variables in this context
#' @examples
#' # We generate some metadata and data
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
#' # we store them together in a behavr object d
#' # d is a copy of the data
#' d <- behavr(data, met)
#' print(d)
#' summary(d)
#'
#' # we can also convert data to a behavr table without copy:
#' setbehavr(data, met)
#' print(data)
#' summary(data)
#'
#' ### Operations are just like in data.table
#' # row subsetting:
#' d[t < 10]
#' # column subsetting:
#' d[, .(id, t, x)]
#' # making new columns inline:
#' d[, x2 := 1 - x]
#' ### Using `meta = TRUE` applies the operation on the metadata
#' # making new metavariables:
#' d[, treatment := interaction(condition,sex), meta = TRUE]
#' d[meta = TRUE]
#'
NULL

# Construction ------------------------------------------------------------

#' @rdname behavr
#' @param x [data.table] containing all measurements
#' @param metadata [data.table] containing the metadata
#' @details Both `x` and `metadata` should have a **column set as key** with **the same name** (typically named `id`).
#' `behavr()` copies `x`, whilst `setbehavr()` uses reference. `metadata` is always copied.
#' @export
behavr <- function(x, metadata){
  check_conform(x, metadata)
  out <- data.table::copy(x)
  setbehavr(out, metadata)
  return(out)
}

#' @rdname behavr
#' @export
setbehavr <- function(x, metadata){
  check_conform(x, metadata)
  m <- data.table::copy(metadata)
  data.table::setattr(x,"metadata",m)
  data.table::setattr(x,"class",c("behavr","data.table","data.frame"))

}


#' @noRd
#' @export
"[.behavr" <- function(x, ..., meta=FALSE,verbose=FALSE){

  m <- data.table::copy(meta(x))
  old_key <- data.table::key(m)
  if(!identical(old_key, data.table::key(m)))
    stop("Something is wrong with this table.
         Keys in metadata and data are different!")
  if(meta==TRUE){

    out <- m[...]
    # if we modified inline (addresses are the same)
    inline <- ifelse(data.table::address(out) == data.table::address(m), TRUE, FALSE)
    if(inline){
      if(!identical(old_key, data.table::key(out)))
         stop("You are trying to modify metadata in a way that removes its key. This is not allowed!")
      data.table::setattr(x,"metadata",m)
    }
    return(out)
  }


  out <- NextMethod()
  if(!data.table::is.data.table(out))
    return(out)

  # if we modified inline (addresses are the same)
  inline <- ifelse(data.table::address(out) == data.table::address(x), TRUE, FALSE)

 # coerce to DT if not conform
 if(!identical(data.table::key(out), old_key)){
   data.table::setattr(out,"metadata",NULL)
   data.table::setattr(out,"class",c("data.table","data.frame"))
 }
# the result is another behavr table
  else{

  #check_conform(out)

  # this means we return a subset (copy) of x (different address)
  # therefore, we want to from metadata, the id that are not in data
  md <- meta(x)
  if(!inline){
    unique_ids <- unique(out[, data.table::key(out), with=FALSE])
    mismatches <- md[!unique_ids]
    if(nrow(mismatches) > 0){
      if(verbose ==TRUE){
        message(sprintf("Implicitly removing %i individuals from metadata (as they are absent from it)", nrow(mismatches)))
      }
      md <- md[unique_ids]
    }
  }
  data.table::setattr(out,"metadata",md)
  data.table::setattr(out,"class",c("behavr","data.table","data.frame"))
 }
  if(inline)
    invisible(out)
  return(out)
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
#' @param detailed whether summary should be exhaustive
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
summary.behavr <- function(object, detailed = F, ...){
  # trick to avoid NOTES from R CMD check:
  . = .SD = .N =  NULL

  met <- object[meta=TRUE]
  n_key <- length(data.table::key(met))
  n_mvar <- ncol(met) -  n_key
  n_var <- ncol(object) -  n_key
  n_reads <- nrow(object)

  if(!detailed){
    cat("behavr table with:\n")
    cat(sprintf(" %i\tindividuals\n", nrow(met)))
    cat(sprintf(" %i\tmetavariables\n", n_mvar))
    cat(sprintf(" %i\tvariables\n", n_var))
    cat(sprintf(" %s\tmeasurements\n",  format( as.double(n_reads), scientific=TRUE)))
    cat(sprintf(" %i\tkey (%s)\n", n_key, paste(data.table::key(met),collapse=", ")))
  }
  else{
    cat("\n Summary of each individual (one per row):\n")
    if(!"t" %in% colnames(object))
        sum_dt <- object[,
                         .(data_points =.N),
                         by = c(data.table::key(object))]
    else
        sum_dt <- object[,
                         .(data_points =.N,
                             time_range = sprintf("[%s -> %s (%s)]",min(t), max(t),  max(t) -min(t))),
                         by = c(data.table::key(object))]
    print(rejoin(sum_dt))
  }
}
