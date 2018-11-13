#' @title Individual Variable Effect
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
#' @param new_observation an observation/observations to be explained. Required for local/instance level
#' explainers. Columns in should correspond to columns in the data argument.
#' @param ... other parameters.
#' @param label name of the model. By default it’s extracted from the class attribute of the model
#' @param method an estimation method of SHAP values. Currently the only availible is `KernelSHAP`.
#' @param nsamples number of samples
#'
#' @return a list
#' \itemize{
#'   \tem a matrix of SHAP values `m` x `d`.
#' For models with a single output this returns one matrix
#' For models with multiple outputs this returns a list of such matrices.
#' \item vector ov `m`x`d` expected values
#' }
#'
#'
#'
#' @importFrom reticulate r_to_py
#'
#' @export
#' @rdname individual_variable_effect

individual_variable_effect <- function(x, ...){
  UseMethod("individual_variable_effect")
}




#' @export
#' @rdname individual_variable_effect
individual_variable_effect.explainer <- function(x, new_observation,
                                                     method = "KernelSHAP", ...){
  # extracts model, data and predict function from the explainer
  model <- x$model
  data <- x$data
  predict_function <- x$predict_function
  label <- x$label

  individual_variable_effect.default(model, data, predict_function,
                             new_observation = new_observation,
                             label = label,
                             method = method,
                             nsamples = nsamples,
                             ...)
}




#' @export
#' @rdname individual_variable_effect
individual_variable_effect.default <- function(x, data, predict_function,
                                                   new_observation,
                                                   label,
                                                   method = "KernelSHAP",
                                                   nsamples = 100,
                                                   ...){
  model <- x
  p_function <- function(data) {
    predict_function(x = model, data = data)
  }
  # TODO add another methods
  explainer = shap$KernelExplainer(p_function, data)
  new_observation_pandas <- r_to_py(new_observation)
  shap_values = explainer$shap_values(new_observation_pandas, nsamples = nsamples)
  expected_value = explainer$expected_value

  result <- list(shap_values = shap_values,
                 expected_value = expected_value,
                 new_observation = new_observation)
  # TODO add other attributes
  class(result) <- c("individual_variable_effect", class(shap_values))
  return(result)
}
