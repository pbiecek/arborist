#' Plot forester models
#'
#' The generic `plot()` function creates and prints numeric summary
#' of forester models.
#' See https://cran.r-project.org/web/packages/randomForestExplainer/index.html
#' and https://github.com/ModelOriented/EIX
#' for some examples.
#'
#' @param model a model created with the `train()` function
#' @param ... other parameters
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' library(DALEX)
#' library(magrittr)
#' titanic_imputed %>%
#'     train("survived") %>%
#'     forester::plot()
#'
plot <- function(model, ...) {
  print("This is only a placeholder")
}
