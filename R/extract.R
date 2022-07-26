#' Extract knowledge from forester models
#'
#' The `extract()` function extracts some useful statistics, like important features
#' i.e. these used by the model, ranking of feature importances.
#'
#' @param model a model created with the `train()` function
#' @param ... other parameters
#'
#' @return set of ggplot objects that summarise the model
#' @export
#'
#' @examples
#' library(DALEX)
#' library(magrittr)
#' titanic_imputed %>%
#'     train("survived") %>%
#'     forester::extract()
#'
extract <- function(model, ...) {
  print("This is only a placeholder - extract")
}
