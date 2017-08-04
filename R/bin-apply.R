#' Bin a variable (typically time) and compute an aggregate for each bin
#'
#' This function is typically used to summarise (i.e. computing an aggreate of) a variable (`y`)
#' for bins of a another variable `x` (typically time).
#' .
#'
#' @param data [data.table] or [behavr] table (see details)
#' @param y variable to be aggregated
#' @param x the variable to be binned
#' @param x_bin_length the length of the bins (same using as `x``)
#' @param FUN  function used to aggregate (e.g. [mean], [median], [sum] and so on)
#' @param ... additional arguments to be passed to `FUN`
#' @details
#' `bin_apply` expects data from a single individal.
#' `bin_apply_all` works on multiple individuals identifies by a unique key.
#' @examples
#' query <- data.frame(experiment_id="toy_experiment",
#'                       region_id=1:5)
#' dt <- toy_activity_data(query, duration=days(4))
#'
#' # average by 30min time bins, default
#' dt_binned <- bin_apply_all(dt,moving)
#' # equivalent to
#' dt_binned <- dt[, bin_apply(.SD, moving),by="id"]
#'
#' # More advanced usage
#' dt <- toy_dam_data(query, duration=days(4))
#'
#' # nsum activity ber 60minutes
#' dt_binned <- bin_apply_all(dt,
#'                            activity,
#'                            x=t,
#'                            x_bin_length = mins(60),
#'                            FUN=sum)
#'
#' @export
bin_apply <- function(data, y, x=t, x_bin_length = mins(30), FUN=mean, ...){
  var_name <- deparse(substitute(y))
  b_name <- deparse(substitute(x))
  data.table::setnames(data, c(var_name, b_name), c("..var..", "..b.."))
  tryCatch({
    out <- data[,.(..var.. = ..var..,
                ..b.. = bin_var( ..b.., x_bin_length))]
    out <- out[,
                    .(..var..=FUN(..var..,...)),
                    by=..b..]

    data.table::setnames(out, c("..var..", "..b.."), c(var_name, b_name))
    return(out)
  },
   finally={data.table::setnames(data, c("..var..", "..b.."), c(var_name, b_name))}
  )

}


#' @rdname bin_apply
#' @export
bin_apply_all <- function(data, ...){
  data[, bin_apply(data=.SD, ...),by=eval(data.table::key(data))]
}

bin_var <- function(t, bin_length){
  floor(t /bin_length) * bin_length
}


