#@include
#' Time conversion utilities
#'
#' Trivial functions to convert time to seconds -- as `rethomics` uses second as a conventionnal unit of time.
#'
#' @details  Given an dummy function that takes time in second like: `myFunction(t)`,
#' it is always preferqable to call `myFunction(days(1.5))` rather than `myFunction(60*60*24*1.5)`.
#'
#' @param x Numerical vector to be converted in second
#' @return Number of seconds corresponding to `x` (1d = 86400s, 1h = 3600s and 1min = 60s)
#' @name time_conversion
NULL
#' @rdname time_conversion
#' @export
days <- function(x){
  x * 86400
}
#' @rdname time_conversion
#' @export
hours <- function(x){
  x * 3600
}
#' @rdname time_conversion
#' @export
mins <- function(x){
  x * 60
}
