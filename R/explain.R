#' Explain forester model
#'
#' The `explain()` is a wrapper for `DALEX` methods for model explanations.
#' If possible it will use methods for tree based models.
#'
#' @param model a model created with the `train()` function
#' @param ... other parameters
#'
#' @return
#' @export
#'
#' @examples
#' library(DALEX)
#' library(magrittr)
#' titanic_imputed %>%
#'     train("survived") %>%
#'     forester::explain()
#'
explain <- function(model, ...) {
  print("This is only a placeholder")
}
