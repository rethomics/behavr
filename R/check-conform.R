#' A helper function that ensures data and metadata are compatible (same key)
#' @noRd
check_conform <- function(x, metadata=NULL){

  if(!data.table::is.data.table(x))
    stop("x is not a data.table!")

  if(is.null(metadata))
    metadata <- meta(x)

  if(!data.table::is.data.table(metadata))
    stop("metadata is not a data.table!")

  k_x <- data.table::key(x)
  k_m <- data.table::key(metadata)

  if(is.null(k_x))
    stop("x has no key")
  if(is.null(k_m))
    stop("metadata has no key")

  if(!identical(k_x, k_m))
    stop("x and metadata have different keys. The keys mus be columns with the same name!")

}
