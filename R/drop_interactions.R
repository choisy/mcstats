#' Drop Interactions
#'
#' This function drop interaction terms from a statistical model formula
#'
#' Returns a formula without interaction terms
#'
#' @param formula an object of class formula
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom MuMIn expand.formula
#' @export
#' @author Marc Choisy
# This function drops interactions terms from a formula
drop_interactions <- function(x) {
  env <- environment(x)
  x %<>% expand.formula %>% as.character
  x[3] %<>%
    strsplit("\\+") %>%
    unlist %>%
    grep(":", ., value = TRUE, invert = TRUE) %>%
    paste(collapse = "+")
  x[c(2, 1, 3)] %>%
    paste(collapse = "") %>%
    as.formula(env)
}
