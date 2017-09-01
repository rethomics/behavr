#' Bin a variable (typically time) and compute an aggregate for each bin
#'
#' This function is typically used to summarise (i.e. computing an aggregate of) a variable (`y`)
#' for bins of a another variable `x` (typically time).
#'
#'
#' @param data [data.table] or [behavr] table (see details)
#' @param y variable to be aggregated
#' @param x variable to be binned
#' @param x_bin_length length of the bins (same unit as `x`)
#' @param wrap_x_by numeric value defining wrapping period. `NULL`, the default, means no wrapping.
#' @param FUN  function used to aggregate (e.g. [mean], [median], [sum] and so on)
#' @param string_xy logical, whether the names of the variables are quoted
#' @param ... additional arguments to be passed to `FUN`
#' @details
#' `bin_apply` expects data from a single individual, whilst
#' `bin_apply_all` works on multiple individuals identified by a unique key.
#' `wrapping` is typically used to compute averages across several periods.
#' For instance,`wrap_x_by = days(1)`, means bins will aggregate values across several days.
#' In this case, the resulting `x`` can be interpreted as "time relative to the onset of the day" (i.e. Zeitgeber Time).
#' @examples
#' query <- data.frame(experiment_id = "toy_experiment",
#'                       region_id = 1:5)
#' dt <- toy_activity_data(query, duration = days(4))
#'
#' # average by 30min time bins, default
#' dt_binned <- bin_apply_all(dt, moving)
#' # equivalent to
#' dt_binned <- dt[, bin_apply(.SD, moving), by = "id"]
#'
#' # More advanced usage
#' dt <- toy_dam_data(query, duration = days(4))
#'
#' # nsum activity per 60 minutes
#' dt_binned <- bin_apply_all(dt,
#'                            activity,
#'                            x = t,
#'                            x_bin_length = mins(60),
#'                            FUN = sum)
#'
#'
#' # average activity. time in ZT
#' dt_binned <- bin_apply_all(dt,
#'                            activity,
#'                            x = t,
#'                            wrap_x_by = days(1)
#'                            )
#' @seealso
#' * [behavr] -- the documentation of the `behavr` object
#' @export
bin_apply <- function(data, y, x = t, x_bin_length = mins(30),
                      wrap_x_by = NULL, FUN = mean, string_xy = FALSE, ...){


  # trick to avoid NOTES from R CMD check:
  var__ = b__ = .SD =  . = NULL

  if(!string_xy){
    var_name <- deparse(substitute(y))
    b_name <- deparse(substitute(x))
  }
  else{
    var_name <- y
    b_name <- x
  }

  #todo check variables!
  data.table::setnames(data, c(var_name, b_name), c("var__", "b__"))
  tryCatch({
    out <- data[,.(var__ = var__,
                b__ = bin_var( b__, x_bin_length, wrap=wrap_x_by))]
    out <- out[,
                    .(var__=FUN(var__,...)),
                    by=b__]

    data.table::setnames(out, c("var__", "b__"), c(var_name, b_name))
    return(out)
  },
   finally={data.table::setnames(data, c("var__", "b__"), c(var_name, b_name))}
  )

}


#' @rdname bin_apply
#' @export
bin_apply_all <- function(data, ...){
  # trick to avoid NOTES from R CMD check:
  .SD =  NULL

  data[, bin_apply(data=.SD, ...), by = eval(data.table::key(data))]
}

bin_var <- function(t, bin_length, wrap = NULL){
  if(!is.null(wrap))
    t <- t %% wrap
  floor(t /bin_length) * bin_length
}



