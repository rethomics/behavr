#' Retrieve and set metadata
#'
#' This function returns the metadata from a [behavr] table.
#' @param x [behavr] object
#' @return a [data.table] representing the metadata in `x`
#' @examples
#' set.seed(1)
#' met <- data.table::data.table(id = 1:5,
#'                               condition = letters[1:5],
#'                               sex = c("M", "M", "M", "F", "F"),
#'                               key = "id")
#' data <- met[,
#'             list(t = 1L:100L,
#'                  x = rnorm(100),
#'                  y = rnorm(100),
#'                  eating = runif(100) > .5 ),
#'              by = "id"]
#'
#' d <- behavr(data, met)
#' ## show metadata
#' meta(d)
#' # same as:
#' d[meta = TRUE]
#' ## set metadata
#' m <- d[meta = TRUE]
#' # only id > 2 is kept
#' setmeta(d, m[id < 3])
#' meta(d)
#' @seealso
#' * [behavr] -- the documentation of the `behavr` object
#' * [xmv] -- to join metavariables
#' @export
#' @name meta
meta <- function(x){
  attr(x,"metadata")
}

#' @param new a new metadata table
#' @rdname meta
#' @export
setmeta <- function(x, new){
#  check_conform(x, new)

# unique_ids <- unique(x[, data.table::key(x), with=FALSE])
# ids_in_md <- unique(new[, data.table::key(x), with=FALSE])
# mismatches <- unique_ids[!ids_in_md]

  # if(nrow(mismatches) > 0){
  #   message(sprintf("Implicitly removing %i individuals from data (as they are absent from it)", nrow(mismatches)))
  #   x <- .x[ids_in_md]
  #   #?data.table::setDT()
  # }
  
  setbehavr(x,new)
 # data.table::setattr(x,"metadata",new)
}
