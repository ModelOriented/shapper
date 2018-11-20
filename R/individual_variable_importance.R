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
#' @param nsamples number of samples. Note that number must be as integer. Use `as.integer()`.
#'
#' @return an object of class individual_variable_effect with shap values of each variable for each new observation.
#' Columns:
#' \itemize{
#'   \item first d columns contains variable values.
#'   \item _id_ - id of observation, number of row in `new_observation` data.
#'   \item _ylevel_ - level of y
#'   \item _yhat_ -predicted value for level of y
#'   \item _yhat_mean_ - expected value of prediction, mean of all predictions
#'   \item _vname_ - variable name
#'   \item _attribution_ - attribution of variable
#'   \item _sign_ a sign of attribution
#'   \item _label_ a label of model
#' }
#'
#'
#'
#' @importFrom reticulate r_to_py
#'
#' @examples
#' \dontrun{
#' library("shapper")
#' library("DALEX")
#' library("randomForest")
#' Y_train <- HR$status
#' x_train <- HR[ , -6]
#' x_train$gender <- as.numeric(x_train$gender)
#' set.seed(123)
#' model_rf <- randomForest(x = x_train, y = Y_train)
#' p_fun <- function(x, data){
#'   predict(x, newdata = data, type = "prob")
#'   }
#'   res <- individual_variable_effect(x = model_rf, data = x_train,
#'                                     predict_function = p_fun,
#'                                     new_observation = x_train[1,])
#' }
#'
#' @export
#' @rdname individual_variable_effect

individual_variable_effect <- function(x, ...){
  UseMethod("individual_variable_effect")
}




#' @export
#' @rdname individual_variable_effect
individual_variable_effect.explainer <- function(x, new_observation,
                                                     method = "KernelSHAP", nsamples = 100,
                                                     ...){
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
                                                   label = class(x)[1],
                                                   method = "KernelSHAP",
                                                   nsamples = 100L,
                                                   ...){
  expanded_data <- one_hot_encoder(data)
  decoding_data <- attr(expanded_data, "decoder")
  if(length(decoding_data$assign) != length(unique(decoding_data$assign))){
    stop("KernelExplainer assumes feature independence, can't do automatic one-hot encoding for factors with more than 2 levels. Please, consider fitting model on encoded data set")
  }


  p_function <- function(data) {
    # print(head(data))
    data <- one_hot_decoder(data = data, decoding_data = decoding_data)
    predict_function(x = x, data = data)
  }
  # TODO add other methods
  explainer = shap$KernelExplainer(p_function, expanded_data)

  expanded_new_observation <- one_hot_encoder(new_observation)
  # to get rid of problem with one-row converison
  fake_expanded_new_observation <- rbind(expanded_new_observation, expanded_new_observation[1,])
  # now remove it in pandas
  new_observation_pandas <- r_to_py(fake_expanded_new_observation)
  new_observation_pandas$drop(new_observation_pandas$index[nrow(fake_expanded_new_observation)-1], inplace = TRUE)

  shap_values = explainer$shap_values(new_observation_pandas, nsamples = nsamples)
  expected_value = explainer$expected_value
  predictions <- p_function(expanded_new_observation)
  variables <- colnames(data)

  # create data to return
  new_data <- new_observation
  new_data$`_id_` <- c(1:nrow(new_data))

  # add multiple predictions
  new_data <- new_data[rep(1:nrow(new_data), each = length(shap_values)), ]
  new_data$`_ylevel_` <- rep(colnames(predictions), times = nrow(new_observation))
  new_data$`_yhat_` <- as.vector(t(predictions))
  new_data$`_yhat_mean_` <- rep(expected_value, times = nrow(new_observation))

  # add multiple variables
  new_data <- new_data[rep(1:nrow(new_data), each = ncol(data)), ]
  new_data$`_vname_` <- rep(variables, times = length(predictions))

  attribution <- numeric()
  for(i in 1: nrow(new_observation)){
    for(j in 1:length(shap_values)){
      attribution <- c(attribution, shap_values[[j]][i,] )
    }
  }
  new_data$`_attribution_` <- attribution
  new_data$`_sign_` <- factor(sign(new_data$`_attribution`))
  new_data$`_sign_` <- ifelse(new_data$`_sign_` == 1, "+", "-")

  new_data$`_label_` <- label

  class(new_data) <- c("individual_variable_effect", "data.frame")
  return(new_data)
}
