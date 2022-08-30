#' Create embeddings with the arborist package
#'
#' The `embed()` function create an embedding for data set
#' with the use of a random forest model.
#' Two different approaches to embeddings are implemented.
#' One is based on SHAP values while the second is a binary encoding
#' of data flow through the forest.
#'
#' @param data `data.frame` or `matrix` - data which will be
#' embedded by a model
#' @param model a tree-based model that will be used to create embeddings.
#' If this object is an explainer, then a model will be extracted.
#' @param type a character with the name of the embedding procedure.
#' one out of `binary`/`shap`/`bayesian`.
#' If `binary` then the resulting embedding will have as many dimensions as
#' we have trees in the forest Each tree will contribute to a single dimension.
#' The embedding procedure is described in the ,,All you need are trees'' ebook.
#' If `shap` then Shapley values are used as embeddings. The size of
#' the embedding corresponds to the number of variables in the 'model'.
#'
#' @return a matrix with embeddings created for observations from the 'data'
#' @export
#' @importFrom randomForest getTree
#' @importFrom treeshap treeshap
#' @importFrom treeshap randomForest.unify
#'
#' @examples
#' library("DALEX")
#' model <- train(titanic_imputed, "survived", engine = "randomForest")
#' embeddings_binary <- embed(titanic_imputed, model, type = "binary")
#' head(embeddings_binary)
#' embeddings_shap   <- embed(titanic_imputed, model, type = "shap")
#' head(embeddings_shap)
#'
embed <- function(data,
                  model,
                  type = "binary") {
  # if model is an explainer, then extract the real model from this object
  if ("explainer" %in% class(model)) model <- model$model

  # currently only random forest model is implemented
  stopifnot("randomForest" %in% class(model))

  embeddings <- switch (type,
          'binary' = get_embedding_forest_binary(data, model),
          'shap'   = get_embedding_forest_shap(data, model)
  )

  # return embeddings
  embeddings
}


#' Calculate shap embeddings for a random forest model
get_embedding_forest_shap <- function(data, forest, ...) {
  stopifnot("randomForest" %in% class(forest))

  unified_model <- randomForest.unify(forest, data)
  embeddings <- treeshap(unified_model, data)

  embeddings
}

#' Calculate binary embeddings for a random forest model
get_embedding_forest_binary <- function(data, forest, length_of_embedding = 8) {
  stopifnot("randomForest" %in% class(forest))

  embeddings <- sapply(1:forest$ntree, function(j) {
    tree <- randomForest::getTree(forest, j, labelVar = TRUE)
    # make sure that these are characters
    tree[, "split var"] <- as.character(tree[, "split var"])
    get_embedding_tree_binary(data, tree, length_of_embedding = length_of_embedding)
  })
  embeddings
}

#' Calculate binary embeddings for a tree from forest model
get_embedding_tree_binary <- function(data, tree, length_of_embedding = 8) {
  embeddings <- c()

  for (i in 1:nrow(data)) {
    node <- 1
    emb_val <- 0
    emb_pos <- 0
    while (tree[node, "status"] != -1) {
      variable <- tree[node, "split var"]
      # is numeric or factor
      # if factor the encoding is a bit complex
      if (is.numeric(data[i, variable])) {
        # numeric variable, we can use standard <= operator
        is_left <- data[i, variable] <= tree[node, "split point"]
      } else {
        # categorical variable, we need to use binary representation
        tval <- data[i, variable]
        # let's see the binary representation
        binary_representation <- as.numeric(intToBits(tree[node, "split point"]))
        is_left <- binary_representation[as.numeric(tval)] > 0
      }
      # ok, shall we go to the left?
      if (is_left) {
        node <- tree[node, "left daughter"]
        emb_val <- emb_val * 2
        emb_pos <- emb_pos + 1
      } else {
        node <- tree[node, "right daughter"]
        emb_val <- emb_val * 2 +1
        emb_pos <- emb_pos + 1
      }
    }
    embeddings[i] <- emb_val * 2^(length_of_embedding - emb_pos)
  }
  embeddings
}

