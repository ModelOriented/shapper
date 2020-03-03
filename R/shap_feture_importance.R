#' @title SHAP Feature Importance
#' Calculates SHAP feature importance values. SHAP values are calculated for each instance of the given data. 
#' The mean of the absolute SHAP values for each feature is then returned.
#' (See Molnar 2020: https://christophm.github.io/interpretable-ml-book/shap.html#shap-feature-importance)
#' 
#' Implements the kmeans function of the SHAP python lib to help summarize large data sets.
#' 
#' 
#' @param x a model to be explained, or an explainer created with function \code{\link[DALEX]{explain}}.
#' @param data validation dataset. Used to determine univariate distributions, calculation of quantiles,
#' correlations and so on. It will be extracted from `x` if it’s an explainer.
#' @param predict_function predict function that operates on the model `x`. Since the model is a black box,
#' the `predict_function` is the only interface to access values from the model. It should be a function that
#' takes at least a model `x` and data and returns vector of predictions. If model response has more than
#' a single number (like multiclass models) then this function should return a marix/data.frame of the size
#' `m` x `d`, where `m` is the number of observations while `d` is the dimensionality of model response.
#' It will be extracted from `x` if it’s an explainer.
#' @param ... other parameters.
#' @param label name of the model. By default it’s extracted from the class attribute of the model
#' @param method an estimation method of SHAP values. Currently the only availible is `KernelSHAP`.
#' @param nsamples number of samples or "auto". Note that number must be as integer. Use `as.integer()`.
#' @param kmeans (bool) activate summarizing the dataset to decrease background data sample size for increased performance.
#' @param k number of summary instances generated through kmeans
#' 
#' @return A named vector of feature importance values
#'
#' In order to use shapper with other python virtual environment following R command are required to execute
#' reticulate::use_virtualenv("path_to_your_env")
#' or for conda
#' reticulate::use_conda("name_of_conda_env")
#' before attaching shapper.
#'
#' @importFrom reticulate r_to_py
#'
#' @export
#' @aliases shap
#'
#' @rdname shap_feature_importance

shap_feature_importance <- function(x, ...) {
  UseMethod("shap_feature_importance")
}


#' @export
#' @rdname shap_feature_importance
shap_feature_importance.explainer <- function(x,
                                                 method = "KernelSHAP",
                                                 nsamples = "auto",
                                                 kmeans = FALSE,
                                                 k = 150,
                                                 ...) {
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  shap_feature_importance.default(
    model,
    data,
    predict_function,
    label = label,
    method = method,
    nsamples = nsamples,
    ...
  )
}


#' @importFrom utils tail
#' @export
#' @rdname shap_feature_importance
shap_feature_importance.default <-
  function(x,
           data,
           predict_function = predict,
           label = tail(class(x), 1),
           method = "KernelSHAP",
           nsamples = "auto",
           kmeans = FALSE,
           k = 150,
           ...) {

    # transform factors to numerics and keep factors' levels
    data_classes <- sapply(data, class)
    factors <- list()
    data_numeric <- data
    for (col in names(data_classes)) {
      if (data_classes[col] == "factor") {
        factors[[col]] <- levels(data[, col])
        data_numeric[, col] <- as.numeric(data_numeric[, col]) - 1
      }
    }

    # force nsamples to be an integer
    if (is.numeric(nsamples))
      nsamples <- as.integer(round(nsamples))

    # kmeans to summarize data
    if (kmeans) {
        data_numeric <- shap_reference$kmeans(data_numeric, as.integer(k))
        n_rows <- k
    } else {
        n_rows <- nrow(data)
    }

    p_function <- function(new_data) {
      new_data <- as.data.frame(new_data)
      colnames(new_data) <- colnames(data)
      for (col in names(factors)) {
        new_data[, col] <- factor(new_data[, col],
                                  levels = c(0:(length(factors[[col]]) - 1)),
                                  labels = factors[[col]])
      }
      res <- as.data.frame(predict_function(x, new_data))
      if (nrow(res) == 1) {
        res[2, ] <- 0
        res <- r_to_py(res)
        res$drop(res$index[1], inplace = TRUE)
      }
      return(res)
    }

    explainer = shap_reference$KernelExplainer(p_function, data_numeric)

    abs_shap_vals = matrix(0, nrow=length(data_classes), ncol=0, dimnames = list(names(data_classes), c()))

    # iterate over all instances and calculate shap values for each feature
    for (i in 1:n_rows) {
        new_observation <- data[i,]
        new_observation_releveled <- new_observation
        new_observation_numeric <- new_observation
        for (col in names(factors)) {
        new_observation_releveled[, col] <-
            factor(new_observation_releveled[, col], levels = factors[[col]])
        new_observation_numeric[, col] <-
            as.numeric(new_observation_releveled[, col]) - 1
        }

        shap_values = explainer$shap_values(new_observation_numeric, nsamples = nsamples)
        expected_value = explainer$expected_value

        attribution <- numeric()
        for (i in 1:nrow(new_observation)) {
            for (j in 1:length(shap_values)) {
                shap_attributes <- shap_values[[j]]
                if (is.matrix(shap_attributes)) {
                    attribution <- c(attribution, shap_attributes[i, ])
                } else {
                    attribution <- c(attribution, shap_attributes[i])
                }
            }
        }

        # select only shap values with ID 1, add absolute shap value to matrix
        abs_shap_vals <- cbind(abs_shap_vals, as.vector(abs(attribution[1:length(data_classes)])))
    }

    # return mean of all shap values for each feature
    return(rowMeans(abs_shap_vals))
    
  }

