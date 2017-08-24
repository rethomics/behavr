#' Retreive and set metadata
#'
#' This function returns the meta data from a [behavr] object
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
#' @seealso [behavr] to generate a `behavr` object, [xmv] to map metavariables to data
#' @export
#' @name meta
meta <- function(x){
  attr(x,"metadata")
}

#' @param new a new metadata table
#' @rdname meta
#' @export
setmeta <- function(x, new){
  check_conform(x, new)
  data.table::setattr(x,"metadata",new)
}
