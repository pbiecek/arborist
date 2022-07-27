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
#' @param data_test data that will be used for evaluation
#' @param y_test data that will be used for evaluation
#' @param train_test_ratio if data_test is not provided then the orifinal data will be divided in train/test
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
#' @param verbose logical. If TRUE (default) then diagnostic messages will be printed
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
                  data_test = NULL,
                  y_test = NULL,
                  train_test_ratio = 4,
                  type = "guess",
                  engine = c("randomForest", "ranger", "xgboost", "lightgbm"),
                  loss = "default",
                  validation = "default",
                  tuning = "default",
                  keep = FALSE,
                  verbose = FALSE) {
  stopifnot(tuning %in% c("default", "portfolio"))

  # TEST: is y a name of a variable?
  if (!is.character(y) && length(y)>1) { # vector not character with name
    data$y <- y      # TODO: one should check if `y` was not a variable in this dataset
    y <- "y"
    if (!is.null(data_test))
      data_test$y <- y_test
  } #now we can be sure that y has name of the variable and data consists this name

  # TEST: do we have validation data?
  if (is.null(data_test)) {
    n       <- nrow(data)
    n_train <- round(n*train_test_ratio/(train_test_ratio+1))
    n_test  <- n - n_train
    permut  <- sample(n)
    data_test <- data[-permut[1:n_train], ]
    data      <- data[permut[1:n_train], ]
  }


  # PROCESS: create models
  list_of_models <- list()
  if ("randomForest" %in% engine) {
    list_of_models <- c(list_of_models,
                        train_randomForest(data, y = y, data_test = data_test, loss = "default", validation = "default", tuning = "default", verbose = verbose))
  }
  if ("ranger" %in% engine) {
    list_of_models <- c(list_of_models,
                        train_ranger(data, y = y, data_test = data_test, loss = "default", validation = "default", tuning = "default", verbose = verbose))
  }

  # PROCESS: evaluate performance for all models
  performance_of_models <- lapply(list_of_models, DALEX::model_performance)



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
#' @param data_test data for model validation
#' @param type a character, one of `classification`/`regression`/`guess`.
#' sets the type of the task. If `guess` (the default option) then
#' forester will figure out `type` based on the number of unique values
#' in the `y` variable
#' @param loss see `train` function
#' @param validation see `train` function
#' @param tuning see `train` function
#' @param label label for the model
#' @param verbose see `train` function
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
       data_test,
       type = "guess",
       loss = "default",
       validation = "default",
       tuning = "default",
       label = "Random Forest",
       verbose = FALSE) {
  # check if we can do the work
  stopifnot(validation == "default") # "Currently only default validation is implemented"
  stopifnot(loss == "default") # "Currently only default loss is implemented"

  model <- randomForest::randomForest(as.formula(paste0(y, " ~ .")),
        data = data)
  class(model) <- c("foresterRandomForest", "forester", class(model))
  # get an explainer
  DALEX::explain(model,
         data = data_test[,setdiff(colnames(data_test), y), drop = FALSE], # without y variable
         y = data_test[,y],
         label = label,
         verbose = verbose)
}


#' Train a simple ranger model
#'
#' @param data see the `train` function
#' @param y see the `train` function
#' @param data_test see the `train` function
#' @param type a character, one of `classification`/`regression`/`guess`.
#' sets the type of the task. If `guess` (the default option) then
#' forester will figure out `type` based on the number of unique values
#' in the `y` variable
#' @param loss see `train` function
#' @param validation see `train` function
#' @param tuning see `train` function
#' @param label label for the model
#' @param verbose see `train` function
#' @importFrom ranger ranger
#' @return an DALEX explainer
#' @export
train_ranger <- function(data,
         y,
         data_test,
         type = "guess",
         loss = "default",
         validation = "default",
         tuning = "default",
         label = "Ranger",
         verbose = FALSE) {
  # check if we can do the work
  stopifnot(validation == "default") # "Currently only default validation is implemented"
  stopifnot(loss == "default") # "Currently only default loss is implemented"

  model <- ranger::ranger(as.formula(paste0(y, " ~ .")),
                                      data = data)
  class(model) <- c("foresterRanger", "forester", class(model))
  # get an explainer
  DALEX::explain(model,
                 data = data_test[,setdiff(colnames(data_test), y), drop = FALSE], # without y variable
                 y = data_test[,y],
                 label = label,
                 verbose = verbose)
}
