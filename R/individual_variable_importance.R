#' @title KernelExplainer
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
#' @importFrom reticulate r_to_py
#'
#' @export
#' @rdname individual_variable_importance

individual_variable_importance <- function(x, ...){
  UseMethod("individual_variable_importance")
}




#' @export
#' @rdname individual_variable_importance
individual_variable_importance.explainer <- function(x, new_observation,
                                                     method = "KernelSHAP", ...){
 # TODO
}




#' @export
#' @rdname individual_variable_importance
individual_variable_importance.default <- function(x, data, predict_function,
                                                   new_observation,
                                                   label,
                                                   method = "KernelSHAP",
                                                   nsamples = 100,
                                                   ...){
  model <- x
  p_function <- function(data) {
    predict_function(x = model, data = data)
  }
  explainer = shap$KernelExplainer(p_function, data)
  new_observation_pandas <- r_to_py(new_observation)
  shap_values = explainer$shap_values(new_observation_pandas, nsamples = nsamples)
  return(shap_values)

}
