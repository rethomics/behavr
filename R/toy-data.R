#' Generate toy activity and sleep data mimiking Drosophila behaviour in tubes
#'
#' This function generates random data that emulates some of the features of fruit fly activity and sleep.
#' This is designed **exclusively to provide material for examples and tests** as it generates "realistic" datasets of arbitrary length.
#'
#' @param query query (i.e. a dataframe where every row defines an animal).
#' Typically queries have, at least, the columns `experiment_id` and `region_id`.
#' The default value (`NULL`), will generate data for a single animal.
#' @param seed random seed used (see [set.seed])
#' @param rate_range a parameter defining the boundaries of rate at which animals wake up.
#' It will be uniformely distributed between animals, but fixed for each animal.
#' @param duration length (in seconds) of the data to generate
#' @param sampling_period sampling period (in seconds) of the resulting data
#' @param ... additional arguments to be passed to `simulate_animal_activity`
#' @return a [behavr] table with the query columns as metavariables.
#' In addition to `id` and `t` columns different methods will output different variables:
#' * `toy_activity_data` will have `asleep` and `moving` (1/10s)
#' * `toy_dam_data` will have `activity` (1/60s)
#' * `toy_ethoscope_data` will have `xy_dist_log10x1000`, `has_interacted` and `x` (2/1s)
#' @examples
#' # just one animal, no query needed
#' dt <- toy_ethoscope_data(duration = days(3))
#'
#' # advanced, using a query
#' query<- data.frame(experiment_id = "toy_experiment",
#'                    region_id = 1:10,
#'                    condition = c("A", "B"))
#'
#'
#' # Data that could come from loadEthoscopeData:
#' dt <- toy_ethoscope_data(query, duration = days(1))
#' print(dt)
#'
#' # Some DAM-like data
#' dt <- toy_dam_data(query, seed = 2, duration = days(3))
#' print(dt)
#'
#' # data where behaviour is annotated e.g. by a classifier
#' dt <- toy_activity_data(query, 3)
#' print(dt)
#' @export
toy_activity_data <- function(query = NULL,
                            seed = 1,
                            rate_range = 1/c(60,10),
                            duration = days(5),
                            sampling_period = 10,
                            ...){
  set.seed(seed)


  if(is.null(query))
    query<- data.frame(experiment_id="toy_experiment", region_id=1)

  query <- data.table::as.data.table(query)

  if(sum(c("experiment_id", "region_id") %in%  colnames(query))!=2)
    stop("The provided toy query must have, at least, columns `experiment_id' and `region_id'")

  query[,id:= as.factor(sprintf("%02d|%s", region_id,experiment_id))]
  data.table::setcolorder(query, c("id", setdiff(colnames(query), "id") ))
  data.table::setkeyv(query, "id")

  q <- query[, .(id)]
  #runif(1,rate_range[1], rate_range[2])
  q[, id := id]
  out <- q[,simulate_animal_activity(duration,
                                       sampling_period,
                                       rate=runif(.N,rate_range[1], rate_range[2]),...),
               keyby="id"]

  behavr(out,query)
}


# @return A behavioural `data.table` with the query columns as key and
# the behavioural variables `xy_dist_log10x1000`, `has_interacted` and `x`
#' @rdname toy_activity_data
#' @export
toy_ethoscope_data <- function(...){
  activity_dt <- toy_activity_data(...)
  out <- activity_dt[,velocityFromMovement(.SD),by="id"]
  out
}

#' @rdname toy_activity_data
#' @export
toy_dam_data <- function(...){
  activity_dt <- toy_activity_data(...)
  out <- activity_dt[,velocityFromMovement(.SD),by="id"]

  out[,activity := abs(c(0,diff(sign(.5 - x)))), by="id"]
  out[,activity := as.logical(activity)]
  bin_apply_all(out, activity, x_bin_length = 60, FUN=sum)
}

simulate_animal_activity <- function(max_t=days(5), sampling_period=10, method=activityPropensity,...){
  t <- seq(from=0, to = max_t, by=sampling_period)
  propensity <- method(t,...)
  moving <- propensity > runif(length(t))
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
  velocity_correction_coef=3e-3
  exp_rate_immobile = 12
  norm_sd_moving =.75
  new_t <- seq(from=data[,min(t)], to=data[,max(t)], by=1/fs)
  new_dt <- data.table(t=new_t, key="t")
  out <- data[new_dt, on="t", roll=T]
  out[,dt := c(t[2]-t[1],diff(t))]
  immo_data <- rexp(nrow(out),exp_rate_immobile)
  moving_data <- rnorm(nrow(out),3,norm_sd_moving)

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
