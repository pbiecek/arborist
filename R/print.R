#' Print forester models
#'
#' The generic `print()` function creates and prints numeric sumamry
#' of forester models.
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
#'     forester::print()
#'
print <- function(model, ...) {
  print("This is only a placeholder")
}
