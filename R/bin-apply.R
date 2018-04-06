#' Bin a variable (typically time) and compute an aggregate for each bin
#'
#' This function is typically used to summarise (i.e. computing an aggregate of) a variable (`y`)
#' for bins of a another variable `x` (typically time).
#'
#'
#' @param data [data.table] or [behavr] table (see details)
#' @param y variable or expression to be aggregated
#' @param x variable or expression to be binned
#' @param x_bin_length length of the bins (same unit as `x`)
#' @param wrap_x_by numeric value defining wrapping period. `NULL`, the default, means no wrapping (see details).
#' @param FUN  function used to aggregate (e.g. [mean], [median], [sum] and so on)
#' @param ... additional arguments to be passed to `FUN`
#' @details
#' `bin_apply` expects data from a single individual, whilst
#' `bin_apply_all` works on multiple individuals identified by a unique key.
#' `wrapping` is typically used to compute averages across several periods.
#' For instance, `wrap_x_by = days(1)`, means bins will aggregate values across several days.
#' In this case, the resulting `x` can be interpreted as "time relative to the onset of the day" (i.e. Zeitgeber Time).
#' @examples
#' metadata <- data.frame(id = paste0("toy_experiment|",1:5))
#' dt <- toy_activity_data(metadata, duration = days(2))
#'
#' # average by 30min time bins, default
#' dt_binned <- bin_apply_all(dt, moving)
#' # equivalent to
#' dt_binned <- dt[, bin_apply(.SD, moving), by = "id"]
#'
#' # if we want the opposite of moving:
#' dt_binned <- bin_apply_all(dt, !moving)
#'
#' # More advanced usage
#' dt <- toy_dam_data(metadata, duration = days(2))
#'
#' # sum activity per 60 minutes
#' dt_binned <- bin_apply_all(dt,
#'                            activity,
#'                            x = t,
#'                            x_bin_length = mins(60),
#'                            FUN = sum)
#'
#'
#' # average activity. Time in ZT
#' dt_binned <- bin_apply_all(dt,
#'                            activity,
#'                            x = t,
#'                            wrap_x_by = days(1)
#'                            )
#' @seealso
#' * [behavr] -- the documentation of the `behavr` object
#' @export
bin_apply <- function(data, y, x = "t", x_bin_length = mins(30),
                      wrap_x_by = NULL, FUN = mean, ...){


  # trick to avoid NOTES from R CMD check:
  var__ = b__ = .SD =  . = NULL

  alt_var <-deparse(substitute(y))
  alt_b <-deparse(substitute(x))

  var_name <- suppressWarnings(tryCatch({
    ifelse(is.null(y) | is.function(y), alt_var,y)
    }, error = function(e){alt_var}))
  b_name <- suppressWarnings(tryCatch({
    ifelse(is.null(x) | is.function(x), alt_b, x)
  }, error = function(e){alt_b}))


  out <- data[,
              .(
                b__ = eval(parse(text=b_name)),
                var__ = eval(parse(text=var_name))
                )
              ]

  out <- out[, b__ := bin_var( b__, x_bin_length, wrap=wrap_x_by)]
  out <- out[, .(var__ = FUN(var__)),by = b__]

  data.table::setnames(out, c("var__", "b__"), c(var_name, b_name))

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



