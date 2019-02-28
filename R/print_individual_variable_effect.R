#' Print Individual Variable Effects
#'
#' @param x an individual variable importance explainer created with the \code{\link{individual_variable_effect}} function.
#' @param ... further arguments passed to or from other methods.
#'
#' #' @examples
#' have_shap <- reticulate::py_module_available("shap")
#'
#' if(have_shap){
#'   library("shapper")
#'   library("DALEX")
#'   library("randomForest")
#'   Y_train <- HR$status
#'   x_train <- HR[ , -6]
#'   set.seed(123)
#'   model_rf <- randomForest(x = x_train, y = Y_train, ntree= 50)
#'   p_function <- function(model, data) predict(model, newdata = data, type = "prob")
#'
#'   ive_rf <- individual_variable_effect(model_rf, data = x_train, predict_function = p_function,
#'                                      new_observation = x_train[1:2,], nsamples = 50)
#'   print(ive_rf)
#' }else{
#'     print('Python testing environment is required.')
#' }
#'
#' @importFrom utils head
#' @export

print.individual_variable_effect <- function(x, ...) {
  class(x) <- "data.frame"
  print(head(x))
}
