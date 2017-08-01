#' Extract a variable from metadata and map it agains the data
#'
#' This function eXtract the MetaData from a parent [behavr] object.
#' That is it expands a variable from the metadata to match data *by id*.
#'
#' @param var the variable to be extracted
#' @return a vecor of the dame type as `var`, but of the same length as the number of row in the parent data.
#' As each row of data is matched against metadata for this specific variable.
#' @details This function *can only be called within between the `[]` of a parent* [behavr] object.
#' It is intended to facilitate operations between data and metadata.
#' For instance, if one want to modify a column of the data according the metadata.
#' @examples
#' library(data.table)
#' set.seed(1)
#' data <- data.table(
#'                    id = rep(c("A","B"), times=c(10,26)),
#'                    t = hms::hms(c(1:10,5:30)),
#'                    x = rnorm(36), key="id"
#'                    )
#'
#' metadata = data.table(id=c("A","B"), treatment=c("w","z"), lifespan=c(19,32), ref_x=c(1,0),key="id")
#' dt <- behavr(data,metadata)
#' summary(dt)
#'
#' #### subseting using metadata
#' dt[xmd(treatment) == "w"]
#' dt[xmd(treatment) == "w"]
#' dt[xmd(lifespan) < 30]
#'
#' #### Allocating new columns using metadata
#' # Just joining lifespan (not necessary)
#' dt[, lif := xmd(lifespan)]
#' print(dt)
#' # Anonymously (more useful)
#' dt[, x2 := x-xmd(ref_x)]
#' print(dt)
#' @export
xmd <- function(var){
  d <- get("x",envir=parent.frame(n=3))
  #check_consistency(d)
  var <- deparse(substitute(var))
  md <- meta(d)
  col <- md[,c(data.table::key(md),var), with=F]
  join <- d[,data.table::key(md),with=F][col]
  join[[var]]
}

