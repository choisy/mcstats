#' Find the middles of intervals
#'
#' Find the middles of intervals as defined by the values of their boundaries.
#' Makes more sense if the values of \code{x} are ordered, but doesn't have to.
#'
#' Basically a wrapper around the base R \code{diff()} function. In case
#' \code{with_borders = TRUE}, the first and last values are found by
#' reflection, see \code{Examples}.
#'
#' @param x A vector of numeric values
#' @param with_borders A logical value stating whether the middle values should
#'                     only be in-between the first and last values of \code{x}
#'                     (\code{FALSE}, default value) or be expanded outside this
#'                     range (\code{TRUE}). See \code{Details} for more
#'                     information.
#'
#' @return A vector of numeric values of shorter (if \code{with_borders = FALSE})
#'         or longer (if \code{with_borders = TRUE}) by one element than the
#'         inputed \code{x} vector.
#'
#' @examples
#' # A simple example:
#' x <- 1:10
#' centers(x)
#' centers(x, TRUE)
#'
#' # The effect of the option on the length of the output:
#' length(centers(x))
#' length(centers(x, TRUE))
#'
#' # Works also if the input is not sorted (but makes less sense):
#' centers(sample(x))
#'
#' # Let's consider another example to illustrate the effect of the option:
#' x <- c(1, 2, 4, 7, 11, 16, 22)
#' # where
#' diff(x)
#' # Now:
#' centers(x)
#' # These values are easy to understand. Let's now see:
#' centers(x, TRUE)
#' # Here is how the first (0.5) and last (25) values are computed. The first
#' # one is computed so that the first value of x (1) is in the middle of the
#' # interval defined by the first two values of centers(x, TRUE) (0.5 and 1.5).
#' # Similarly, the last value (25) is computed so that the last value of x (22)
#' # is in the middle of the interval defined by the last two values of
#' # centers(x, TRUE) (19 and 25).
#'
#' @export
#'
centers <- function(x, with_borders = FALSE) {
  ctrs <- x[-1] - diff(x) / 2
  if (with_borders)
    return(c(2 * x[1] - ctrs[1], ctrs, 2 * tail(x, 1) - tail(ctrs, 1)))
  ctrs
}

