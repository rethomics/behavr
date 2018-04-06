#' Stitch behavioural data by putting together the same individuals recorded over different experiments
#' on the basis of a user-defined identifier
#'
#' This function can merge rows of data from the same individual that was recorded over multiple experiments.
#' A usual scenario in which `stitch_on` can be used is when an experiment is interrupted and a new recording is started
#' on the same biological subjects.
#' Stitching assumes the users has defined a *unique id* in the metadata that refers to a specific individual.
#' Then, if any data that comes from the same unique id, it is merged.
#'
#' @inheritParams meta
#' @param on name of a metavariable serving as a unique id (per individual)
#' @param time_ref name of a metavariable used to align time (e.g. `"date"`, or `"datetime"`)
#' @param use_time whether to use time as well as date
#' @param time_variable name of the variable describing time
#' @return a [behavr] table
#' @details
#' When several rows of the metadata match a unique id (several experiments),
#' the first (in time) experiment is used as the reference id.
#' The data from the following one(s) will be added with a time lag equals to the difference between
#' the values of `time_ref`.
#' When data is not aligned to circadian time, it makes sense to set `use_time = TRUE`.
#' Otherwise, the assumption is that the time is already aligned to a circadian reference,
#' so only the date is used.
#' @examples
#' set.seed(1)
#' met1 <- data.table::data.table(uid = 1:5,id = 1:5,
#'                                condition = letters[1:5],
#'                                sex = c("M", "M", "M", "F", "F"),
#'                                key = "id")

#' met2 <- data.table::data.table(uid = 1:4, id = 6:9,
#'                                condition = letters[1:4],
#'                                sex=c("M", "M", "M", "F"),
#'                                key = "id")

#' met1[, datetime := as.POSIXct("2015-01-02")]
#' met2[, datetime := as.POSIXct("2015-01-03")]
#' met <- rbind(met1, met2)
#' data.table::setkeyv(met, "id")
#' t <- 1L:100L
#' data <- met[,list(t = t,
#'                   x = rnorm(100),
#'                   y = rnorm(100),
#'                   eating = runif(100) > .5 ),
#'             by = "id"]
#' d <- behavr(data, met)
#' summary(d)
#' d2 <- stitch_on(d, on = "uid")
#' summary(d2)
#'
#' @seealso
#' * [behavr] -- to formally create a behavr object
#' @export
#' @export
stitch_on <- function(x,
                      on,
                      time_ref = "datetime",
                      use_time = F,
                      time_variable = "t"){
  # trick to avoid NOTES from R CMD check:
  time_ref__ = lag = t__ = bit = . = NULL
  check_conform(x)

  if(!on %in% colnames(meta(x)))
    stop(sprintf("No column named %s in metadata", on))

  if(!time_ref %in% colnames(meta(x)))
    stop(sprintf("No column named %s in metadata. Expecting some kind of date.", time_ref))

  if(!time_variable %in% colnames(x))
    stop(sprintf("No time variable named %s in data", time_variable))


  #check for unique except date/path/...? no prob not

  md <- data.table::copy(meta(x))
  k <- data.table::key(md)

  data.table::setnames(md, time_ref, "time_ref__")
  md <- md[order(time_ref__)]

  # todo check timeref is posix, or date

  if(!use_time){
    md[,
       lag := as.numeric(as.Date(time_ref__) - as.Date(min(time_ref__)), units="secs"),
       by = on]
  }

  else{
    md[,
       lag := as.numeric(time_ref__ - min(time_ref__), units="secs"),
       by = on]
  }


  x2 <- x[md[, c(on, k, "lag"), with=F], on = k, all=FALSE]

  data.table::setnames(x2, time_variable, "t__")
  x2[, t__ := t__ + lag]

  md[,lag:=NULL]
  x2[,lag:=NULL]

  x2[, c(k) := NULL]

  # todo check min/max time for overlap.
  # warn if overlap

  # todo, order by date
  md <- unique(md, by=on)

  x2 <- md[, c(k, on), with=F][x2, on=c(on)]


  back_in_time <- x2[, .(bit = any(diff(t__) < 0)), by=on]

  back_in_time <- back_in_time[bit == TRUE]
  if(nrow(back_in_time) > 0){
    msg <- sprintf("Stitching would result in overlapping points!
                    Issue for %s = %s", on, paste(back_in_time[, on,with=FALSE], colapse = ","))
    stop(msg)
  }
  x2[, c(on) := NULL]

  data.table::setnames(x2, "t__", time_variable)

  data.table::setkeyv(x2, k)

  md <- md[order(time_ref__)]
  data.table::setnames(md, "time_ref__", time_ref)
  data.table::setkeyv(md, k)

  setbehavr(x2,md)
  x2
}



