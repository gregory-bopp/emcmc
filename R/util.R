#' Prepend Zeros to Integer and Convert to String
#'
#' @param x (integer) to be converted to string. If x has more than \code{width}
#' digits, no 0s will be prepended.
#' @param width (integer) desired fixed width of string (default = 10 characters)
#'
#' @return
zero_pad_strd <- function(x, width = 10){
  stringr::str_pad(formatC(x, format = 'd'), width = width, pad = '0')
}
