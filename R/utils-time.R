#@include
#' Time conversion utilities
#'
#' Trivial functions to convert time to seconds -- since `behavr` uses second as a conventional unit of time.
#'
#' @details  Most functions in the `rethomics` framework will use seconds as a unit of time.
#' It is always preferable to call a function like `my_function(days(1.5))` rather than `my_function(60 * 60 * 24 * 1.5)`.
#'
#' @param x numeric vector to be converted in second
#' @return number of seconds corresponding to `x` (1d = 86400s, 1h = 3600s and 1min = 60s)
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
