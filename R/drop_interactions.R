#' Drop Interactions
#'
#' This function drops interaction terms from a statistical model formula.
#'
#' Returns a formula without interaction terms.
#'
#' @param formula an object of class \code{\link[stats]{formula}}.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom MuMIn expand.formula
#' @export
#' @author Marc Choisy.
drop_interactions <- function(formula) {
  env <- environment(formula)
  formula %<>% expand.formula %>% as.character
  formula[3] %<>%
    strsplit("\\+") %>%
    unlist %>%
    grep(":", ., value = TRUE, invert = TRUE) %>%
    paste(collapse = "+")
  formula[c(2, 1, 3)] %>%
    paste(collapse = "") %>%
    as.formula(env)
}
