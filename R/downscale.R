#' Statistical temporal downscaling
#'
#' Performs statistical temporal downscaling of a vector of numerical values,
#' using linear models.
#'
#' The downscaling is performed, complying to the following two constraints:
#' \itemize{
#'   \item within each time interval, the aggregate of the infered values should
#'         be equal to the initial aggregate datum;
#'   \item between all the time intervals, there should be a transition as
#'         smooth as possible.
#' }
#' The first constraint is met by fitting, in each interval, a linear model that
#' goes through the center c(x, y) inputted point as the point of the model on
#' the previous segment that intersect the limit between the segment and the
#' previous one. See
#' \href{https://choisy.github.io/2018/12/28/statistical-temporal-downscaling}{here}
#' for more detailed information. The first point of the first segments is found
#' by minimizing the total smoothness of the model. The minimization is
#' performed over an interval that can be specified as an option by the user.
#'
#' @param y A vector of numerical values containing the aggregate data.
#' @param x A vector of numerical values of the same length as y, containing the
#'          centers of the intervals over which the corresponding y values are
#'          supposed to aggregates.
#' @param n A vector of integer values either of length 1 or of the same length
#'          as the vectors y and x. Gives the number of disaggregated points we
#'          wish per interval.
#' @param interval Interval argument of the base R optimize function used to
#'                 look for the best first infered value. See the \code{Details}
#'                 section for more information. By default set to three times
#'                 the range of y.
#'
#' @return A vector of numerical values.
#'
#' @examples
#' # Let's consider 10 time intervals of centers from 1 to 10 and let's say we
#' # want to infer 12 values per interval:
#' x <- 1:10
#' n <- 12
#' # The limits between these intervals are
#' the_centers <- centers(x, TRUE)
#' # And the x coordinates of the inferred values will be
#' new_x <- unlist(subsegment_center(x, n))
#' # Let's start with a simple example where the initial aggregate data are
#' # values from 1 to 10:
#' y1 <- 1:10
#' y1b <- downscale(y1, x, n)
#' plot(new_x, y1b)
#' points(x, y1, col = "blue", pch = 19)
#' abline(v = the_centers, col = "red")
#' # On this figure the vertical red lines show the limits of the intervals, the
#' # blue dots show the aggregated values available for each of these intervals,
#' # and the black dots show the inferred disaggregated values within each
#' # interval. The figure shows that the continuity between intervals is
#' # respected (second of the above-listed constraints). Let's now check that
#' # the first constraint is also well respected (i.e. that the aggregates of
#' # the inferred values are equal to the initial aggregate data):
#' plot(y1, colMeans(matrix(y1b, n)),
#'      xlab = "initial aggregate data",
#'      ylab = "aggregates of inferred values")
#' abline(0, 1)
#' # That works fine! Let's now consider a slightly more complicated example
#' # that scrambles the values of y:
#' y2 <- sample(y1)
#' y2b <- downscale(y2, x, n)
#' plot(new_x, y2b)
#' points(x, y2, col = "blue", pch = 19)
#' abline(v = the_centers, col = "red")
#' plot(y2, colMeans(matrix(y2b, n)),
#'      xlab = "initial aggregate data",
#'      ylab = "aggregates of inferred values")
#'      abline(0, 1)
#'
#' # Works well too!
#'
#' @export
#'
downscale <- function(y, x, n, interval = NULL) {

  nb <- length(x)
  the_centers <- centers(x, TRUE)
  if (is.null(interval)) interval = 3 * range(y)

  # The function that makes the list of linear models (one per interval).
  # Note that it uses `nb` and `the_centers` defined above.
  make_models <- function(y2_0) {
    models <- vector("list", nb)
    y2 <- c(y2_0, rep(NA, nb))
    for(i in 1:nb) {
      x_val <- c(the_centers[i], x[i])
      y_val <- c(y2[i], y[i])
      models[[i]] <- m <- lm(y_val ~ x_val)
      y2[i + 1] <- predict(m, data.frame(x_val = the_centers[i + 1]))
    }
    models
  }

  # The unsmoothness function that we wish to minimize:
  unsmoothness <- function(list_of_models) {
      sum(abs(diff(sapply(list_of_models, function(x) coef(x)[2]))))
  }

  # Using the function `smoothness()` to find the best `y2_0` value:
  best_y2_0 <- optimize(function(x) unsmoothness(make_models(x)), interval)$minimum

  # Generating the list of models with the best `y2_0` value:
  models <- make_models(best_y2_0)

  # Now that everything is ready:
  unlist(
    Map(
      predict,
      models,
      lapply(
        subsegment_center(x, n),
        function(x) data.frame(x_val = x)
      )
    )
  )
}
