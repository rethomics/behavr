#setGeneric("rbindlist")
#' Put together a list of [behavr] tables
#' @param l list of [behavr]
#' @export
rbindlist.behavr <- function(l){
  all_md <- lapply(l, behavr::meta)
  meta_key <- lapply(all_md, data.table::key)
  if(!all_identical(meta_key))
    stop("metadata do not have the same key!")

  meta_colnames <- lapply(all_md, colnames)
  if(!all_identical(meta_colnames))
    stop("metadata do not have the same columns!")

  data_key <- lapply(l, data.table::key)
  if(!all_identical(data_key))
    stop("data do not have the same key!")

  data_colnames <- lapply(l, colnames)
  if(!all_identical(data_colnames))
    stop("data do not have the same columns!")

  new_meta <- data.table::rbindlist(all_md)
  data.table::setkeyv(new_meta, meta_key[[1]])
  dupl_meta <- duplicated(new_meta, by=data.table::key(new_meta))
  if(sum(dupl_meta) > 0)
    stop("Duplicated key in metadata, id not unique anymore")
  new_data <- data.table::rbindlist(l)
  data.table::setkeyv(new_data, data_key[[1]])
  behavr(new_data, new_meta)
}



all_identical <- function(l){
  f <- function(x,y) if (identical(x,y)) x else NULL
  !is.null(Reduce(f,l))
}

