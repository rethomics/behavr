#' Put together a list of [behavr] tables
#'
#' Bind all rows of both data and metadata from a list of [behavr] tables into a single one.
#' It checks keys, number and names of columns are the same across all data.
#' In addition, it forbids to bind metadata that would result in duplicates (same id in two different metadata).
#'
#' @param l list of [behavr]
#' @return a single [behavr] object
#' @examples
#' met <- data.table::data.table(id = 1:5,
#'                              condition = letters[1:5],
#'                              sex = c("M", "M", "M", "F", "F"),
#'                              key = "id")
#' data <- met[,list(t = 1L:100L,
#'                   x = rnorm(100),
#'                   y = rnorm(100),
#'                   eating = runif(100) > .5),
#'                   by = "id"]
#' d1 <- behavr(data, met)
#'
#' met[,id := id + 5]
#' data[,id := id + 5]
#' data.table::setkeyv(met, "id")
#' data.table::setkeyv(data, "id")
#'
#' d2 <- behavr(data, met)
#' d_all <- bind_behavr_list(list(d1, d2))
#' print(d_all)
#' @seealso
#' * [behavr] -- the documentation of the `behavr` object
#' @export
bind_behavr_list <- function(l){
  if(!is.list(l))
    stop("l should be a list (of behavr tables)!")
  l <- l[!sapply(l,is.null)]
  if(length(l) == 0)
    return(NULL)
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

