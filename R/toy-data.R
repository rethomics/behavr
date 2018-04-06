#' Generate toy activity and sleep data mimicking Drosophila behaviour in tubes
#'
#' This function generates random data that emulates some of the features of fruit fly activity and sleep.
#' This is designed **exclusively to provide material for examples and tests** as
#' it generates "realistic" datasets of arbitrary length.
#'
#' @param metadata [data.frame] where every row defines an individual.
#' Typically `metadata` has, at least, the column `id`.
#' The default value (`NULL`), will generate data for a single animal.
#' @param seed random seed used (see [set.seed])
#' @param rate_range parameter defining the boundaries of the rate at which animals wake up.
#' It will be uniformly distributed between animals, but fixed within each animal.
#' @param duration length (in seconds) of the data to generate
#' @param sampling_period sampling period (in seconds) of the resulting data
#' @param ... additional arguments to be passed to `simulate_animal_activity`
#' @return a [behavr] table with the metadata columns as metavariables.
#' In addition to `id` and `t` columns different methods will output different variables:
#' * `toy_activity_data` will have `asleep` and `moving` (1/10s)
#' * `toy_dam_data` will have `activity` (1/60s)
#' * `toy_ethoscope_data` will have `xy_dist_log10x1000`, `has_interacted` and `x` (2/1s)
#' @examples
#' # just one animal, no metadata needed
#' dt <- toy_ethoscope_data(duration = days(1))
#'
#' # advanced, using a metadata
#' metadata <- data.frame(id = paste0("toy_experiment|",1:9),
#'                    condition = c("A", "B", "C"))
#'
#' metadata
#' # Data that could come from the scopr package:
#' dt <- toy_ethoscope_data(metadata, duration = days(1))
#' print(dt)
#'
#' # Some DAM-like data
#' dt <- toy_dam_data(metadata, seed = 2, duration = days(1))
#' print(dt)
#'
#' # data where behaviour is annotated e.g. by a classifier
#' dt <- toy_activity_data(metadata, 1.5)
#' print(dt)
#' @seealso
#' * [behavr] -- to formally create a behavr object
#' @references
#' * The relevant [rethomic tutorial section](https://rethomics.github.io/behavr.html#playing-with-toy-data) -- explainig how to work with toy data.
#' @export
toy_activity_data <- function(metadata = NULL,
                              seed = 1,
                              rate_range = 1/c(60,10),
                              duration = days(5),
                              sampling_period = 10,
                              ...){

  # trick to avoid NOTES from R CMD check:
  id = region_id = experiment_id =   . = .N = NULL

  set.seed(seed)


  if(is.null(metadata))
    query <- data.table::data.table(id = "toy_data" )
  else
      query <- data.table::as.data.table(metadata)

  if(! "id" %in%  colnames(query))
    stop("The provided toy query must have, at least, a named column id")


  data.table::setcolorder(query, c("id", setdiff(colnames(query), "id") ))
  data.table::setkeyv(query, "id")

  q <- query[, .(id)]
  #runif(1,rate_range[1], rate_range[2])
  q[, id := id]
  out <- q[,simulate_animal_activity(duration,
                                     sampling_period,
                                     rate=stats::runif(.N,rate_range[1], rate_range[2]),...),
           keyby="id"]

  setbehavr(out, query)
}


# @return A behavioural `data.table` with the query columns as key and
# the behavioural variables `xy_dist_log10x1000`, `has_interacted` and `x`
#' @rdname toy_activity_data
#' @export
toy_ethoscope_data <- function(...){
  # trick to avoid NOTES from R CMD check:
  .SD = NULL
  activity_dt <- toy_activity_data(...)
  out <- activity_dt[,velocityFromMovement(.SD),by="id"]
  out
}

#' @rdname toy_activity_data
#' @export
toy_dam_data <- function(...){
  # trick to avoid NOTES from R CMD check:
  x = activity = .N = .SD = NULL

  activity_dt <- toy_activity_data(...)
  out <- activity_dt[,velocityFromMovement(.SD),by="id"]

  out[,activity := abs(c(0,diff(sign(.5 - x)))), by="id"]
  out[,activity := as.logical(activity)]
  bin_apply_all(out, activity, x_bin_length = 60, FUN=sum)
}

simulate_animal_activity <- function(max_t=days(5), sampling_period=10, method=activityPropensity,...){
  t <- seq(from=0, to = max_t, by=sampling_period)
  propensity <- method(t,...)
  moving <- propensity > stats::runif(length(t))
  asleep <- sleepContiguous(moving, 1/sampling_period)
  dt <-data.table(t = t, moving=moving, asleep=asleep)
  dt
}

activityPropensity <- function(t, scale=1, rate=mins(1)){
  a <- 1 - abs(sin(2*pi*t/days(1)))
  a <- (.1 + a^3 *0.8) * rate
  a <- a * scale
  delta_t <- diff(t[1:2])
  a <- a * delta_t
  a
}


velocityFromMovement <- function(data,
                                 fs=2){
  # trick to avoid NOTES from R CMD check:
  dt =  velocity_corrected =moving =  velocity = dist =
    xy_dist_log10x1000 = has_interacted = x =  asleep = NULL

  velocity_correction_coef=3e-3
  exp_rate_immobile = 12
  norm_sd_moving =.75
  new_t <- seq(from=data[,min(t)], to=data[,max(t)], by=1/fs)
  new_dt <- data.table(t=new_t, key="t")
  out <- data[new_dt, on="t", roll=T]
  out[,dt := c(t[2]-t[1],diff(t))]
  immo_data <- stats::rexp(nrow(out),exp_rate_immobile)
  moving_data <- stats::rnorm(nrow(out),3,norm_sd_moving)

  out[, velocity_corrected := ifelse(moving,moving_data,immo_data)]
  out[, velocity := velocity_corrected * velocity_correction_coef/dt]
  out[, dist := velocity * dt]
  out[, dist := ifelse(dist <=0, 1e-6,dist)]
  out[, xy_dist_log10x1000 := round(log10(dist) * 1e3)]
  out[, has_interacted := 0]
  out[,x := cumsum(dist) %% 1]
  out[,x := abs(x-0.5)*2]
  out[,x := ifelse(x > 0.9, 0.9, x)]
  out[,x := ifelse(x < 0.1, 0.1, x)]
  out[, moving:=NULL]
  out[, asleep:=NULL]
  out[, velocity:=NULL]
  out[, velocity_corrected:=NULL]
  out[, dist:=NULL]
  out[, dt:=NULL]
}


sleepContiguous <- function(moving,fs,min_valid_time=5*60){
  min_len <- fs * min_valid_time
  r_sleep <- rle(!moving)
  valid_runs <-  r_sleep$length > min_len
  r_sleep$values <- valid_runs & r_sleep$value
  inverse.rle(r_sleep)
}
