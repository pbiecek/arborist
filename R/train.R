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
#' @return an DALEX explainer (or set of explainers if keep=TRUE)
#' @export
#'
#' @examples
#' library(DALEX)
#' train(titanic_imputed, "survived")
#'
train <- function(data,
                  y,
                  type = "guess",
                  engine = c("randomForest", "ranger", "xgboost", "lightgbm"),
                  loss = "default",
                  validation = "default",
                  tuning = "default",
                  keep = FALSE) {
  list_of_models <- list()

  # add random forest models
  if ("randomForest" %in% engine) {
    list_of_models <- c(list_of_models,
                        train_randomForest(data, y, loss = "default", validation = "default", tuning = "default"))
  }

  # do validation and select the best model
  best <- 1

  best_model <- list_of_models[[best]]
  if (keep == TRUE) {
    attr(best_model, "all_models") <- list_of_models
  }

  # return the best model
  best_model
}


#' Train a simple randomForest model
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
#' @param loss see `train` function
#' @param validation see `train` function
#' @param tuning see `train` function
#' @param label label for the model
#' @importFrom stats as.formula
#' @importFrom randomForest randomForest
#' @importFrom DALEX explain
#' @return an DALEX explainer
#' @export
#' @examples
#' library(DALEX)
#' train_randomForest(titanic_imputed, "survived")
#'
train_randomForest <- function(data,
       y,
       type = "guess",
       loss = "default",
       validation = "default",
       tuning = "default",
       label = "Random Forest") {
  # check if we can do the work
  stopifnot(validation == "default") # "Currently only default validation is implemented"
  stopifnot(loss == "default") # "Currently only default loss is implemented"
  stopifnot(tuning == "default") # "Currently only default selection of hyperparameters is implemented"

  # train a randomForest model
  if (!is.character(y) && length(y)>1) { # vector not character with name
        data$y <- y      # TODO: one should check if `y` was not a variable in this dataset
        y <- "y"
  } #now we can be sure that y has name of the variable and data consists this name

  model <- randomForest::randomForest(as.formula(paste0(y, " ~ .")),
        data = data)
  class(model) <- c("foresterRandomForest", "forester", class(model))
  # get an explainer
  DALEX::explain(model,
        data = data[,setdiff(colnames(data), y), drop = FALSE], # without y variable
        y = data[,y],
        label = label)
}
