#' Train models with forester
#'
#' The `train()` is the core function of this package.
#' The only obligatory arguments are `data` and `target`.
#' Setting and changing other arguments will affect model
#' validation strategy, tested model families and so on.
#'
#' @param data `data.frame` or `matrix` - data which will be
#' used to build models. By default model will be trained
#' on all columns in the `data`
#' @param y target variable. It can be either
#' (1) a vector of the same number of observations as `data` or
#' (2) a character name of variable in the `data` that contains
#' the target variable
#' @param type a character, one of `classification`/`regression`/`guess`.
#' sets the type of the task. If `guess` (the default option) then
#' forester will figure out `type` based on the number of unique values
#' in the `y` variable
#' @param engine a vector of tree-based models that shall be testes.
#' Possible values are: `ranger`, `xgboost`, `lightgbm`, `catboost`.
#' All models from this vector will be trained and the best one will be returned.
#' @param loss a character with the name of the loss function.
#' If `default` then the default loss function for a given task will be used
#' @param validation validation/model selection a character of
#' data.frame. If data.frame then this data will be used for model
#' assessment and selection. If character, then it will be a name of
#' the hyperparameter.
#' @param tuning a character with the name of the tuning procedure.
#' one out of `default`/`portfolio`/`bayesian`/`random search`.
#' If `default` then no tuning, just default hyperparameteres are used.
#' If `portfolio` then static portfolio of hyperparametes are tried.
#' If `bayesian` then Bayesian optimization is performed.
#' If `random search` then random search is performed.
#' @param keep shall all models be returned (`keep = TRUE`) or only the best one
#' (`keep = FALSE`, default option)?
#'
#' @return
#' @export
#'
#' @examples
#' library(DALEX)
#' train(titanic_imputed, "survived")
#'
train <- function(data,
                  y,
                  type = "guess",
                  engine = c("ranger", "xgboost", "lightgbm"),
                  loss = "default",
                  validation = "default",
                  tuning = "default",
                  keep = FALSE) {
  print("This is only an architecture")
}
