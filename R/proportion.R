#' Estimate proportions with confidence intervals
#'
#' This is a wrapper around the \code{\link[stats]{binom.test}} function.
#' @param x number of successes, or a vector of length 2 giving the numbers of
#'          successes and failures, respectively.
#' @param n number of trials; ignored if x has length 2.
#' @param ci confidence level for the returned confidence interval.
#' @author Marc Choisy
#' @seealso \code{\link[stats]{binom.test}}
#' @importFrom purrr map2
#' @export
# The following function estimates proportions with confidence interval
proportion <- function(x, n, ci = .95) {
  fct <- function(x, n) {
    tmp <- binom.test(x, n, conf.level = ci)
    c(tmp$est, tmp$conf)
  }
  map2(x, n, fct) %>% as.data.frame %>%  # because "map2" returns a list
    t %>% as.data.frame %>%              # because "t" returns a matrix
    setNames(c("estimate", "upper", "lower")) %>%
    `row.names<-`(NULL)
}
