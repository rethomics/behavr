#' Expand a metavariable and map it against the data
#'
#' This function eXpands a MetaVariable from a parent [behavr] object.
#' That is, it matches this variable (from metadata) to the data *by id*.
#'
#' @param var the name of the variable to be extracted
#' @return a vector of the same type as `var`, but of the same length as the number of row in the parent data.
#' Each row of data is matched against metadata for this specific variable.
#' @details This function *can only be called within between the `[]` of a parent* [behavr] object.
#' It is intended to facilitate operations between data and metadata.
#' For instance, when one wants to modify a variable according to a metavariable.
#' @examples
#' #### First, we create some data
#'
#' library(data.table)
#' set.seed(1)
#' data <- data.table(
#'                    id = rep(c("A", "B"), times = c(10, 26)),
#'                    t = c(1:10, 5:30),
#'                    x = rnorm(36), key = "id"
#'                    )
#'
#' metadata = data.table(id = c("A", "B"),
#'                       treatment = c("w", "z"),
#'                       lifespan = c(19, 32),
#'                       ref_x = c(1, 0),
#'                       key = "id")
#' dt <- behavr(data, metadata)
#' summary(dt)
#'
#' #### Subsetting using metadata
#'
#' dt[xmv(treatment) == "w"]
#' dt[xmv(treatment) == "w"]
#' dt[xmv(lifespan) < 30]
#'
#' #### Allocating new columns using metavariable
#'
#' # Just joining lifespan (not necessary)
#' dt[, lif := xmv(lifespan)]
#' print(dt)
#' # Anonymously (more useful)
#' dt[, x2 := x - xmv(ref_x)]
#' print(dt)
#' @seealso
#' * [behavr] -- to formally create a behavr object
#' * [rejoin] -- to join all metadata with data
#' @export
xmv <- function(var){
  if(!within_data_table())
    stop("xmv can only be called from inside the `[]` of a behavr object")

  d <- get("x",envir=parent.frame(n=3))
  check_conform(d)
  var <- deparse(substitute(var))
  md <- meta(d)
  cols <- colnames(md)
  if(!var %in% cols){
    columns <- paste(cols, collapse=", ")
    msg <- sprintf("No metavariable named %s.
                   Available metavariables are: '%s'",
                   var,
                   columns)
    stop(msg)
  }
  col <- md[,c(data.table::key(md),var), with=F]
  join <- col[d[,data.table::key(md),with=F]]
  join[[var]]

}

within_data_table <- function(){
  all(c("x","j","i") %in% ls(envir=parent.frame(n=4)))
}
