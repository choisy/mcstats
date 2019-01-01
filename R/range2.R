#' Range of values
#'
#' Wrapper around the base R \code{range()} function that allows to expand the
#' width of the range.
#'
#' The range is reduced or extended symetrically, both sides of the current
#' range.
#'
#' @param ... Any numeric or character objects.
#' @param n Positive numeric single value indicating the factor we wish to
#'          reduce (n < 1) or extand (x > 1) the range.
#' @param na.rm Logical indicating if NAs should be omitted.
#'
#' @return Same output as the base R \code{range()} function.
#'
#' @examples
#' x <- rnorm(100)
#' range(x)
#' range2(x, n = 2)
#' diff(range2(x, n = 2)) / 2
#' diff(range2(x, n = .5)) / .5
#'
#' @export
#'
range2 <- function(..., n = 1, na.rm = FALSE) {
  the_range <- range(..., na.rm = na.rm)
  n * c(-1, 1) * diff(the_range) / 2 + mean(the_range)
}
