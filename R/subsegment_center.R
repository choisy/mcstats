#' Centers of subsegments
#'
#' Takes a vector of numeric values and return a vector of numeric values that
#' correspond to the centers of n subsegments.
#'
#' The vector x = c(x1, x2, x3, x4, ...) is interpreted as the centers of
#' segments [y1, y2], [y2, y3], [y3, y4], etc... The function splits each of
#' these segments into a number of subsegments of equal size and return the
#' center of these segments.
#'
#' @param x A vector of numeric values.
#' @param n A vector on integer values specifying how many subsegments we wish
#'          per initial segment. It should either contain one sigle value or
#'          have the same length as x.
#'
#' @return A list of vectors of numeric values. Each slot of this list
#' corresponds to a segments of x and contains the centers fo the subsegments.
#'
#' @examples
#' subsegment_center(1:3, 5)
#'
#' @export
#'
subsegment_center <- function(x, n) {
  the_centers <- centers(x, TRUE)
  bys <- diff(the_centers) / n
  Map(seq, from = the_centers[seq_along(x)] + bys / 2, by = bys, le = n)
}
