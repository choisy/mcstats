#' Anova Tables for Various Statistical Models
#'
#' This function is a modification of the car::Anova function, testing the
#' significativity of main effects without the interactions terms.
#'
#' @importFrom car Anova
#' @export
#' @author Marc Choisy
#'
Anova2 <- function(mod, ...) {
  tmp <- Anova(mod, ...)
  rbind(Anova(update(mod, drop_interactions(formula(mod))), ...),
        tmp[grep(":", row.names(tmp), value = TRUE), ])
}
