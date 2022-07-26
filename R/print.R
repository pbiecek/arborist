#' Print forester models
#'
#' The generic `print()` function creates and prints numeric sumamry
#' of forester models.
#'
#' @param x a model created with the `train()` function
#' @param ... other parameters
#'
#' @return side effect - prints something on the screen
#' @export
#'
#' @examples
#' library(DALEX)
#' library(magrittr)
#' titanic_imputed %>%
#'     train("survived")
#'
print.forester <- function(x, ...) {
  class(x) <- setdiff(class(x), "forester")
  print(x)
}
