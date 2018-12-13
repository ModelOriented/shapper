#' Print Individual Variable Importances
#'
#' @param x an individual variable importance explainer created with the \code{\link{`individual_variable_importance`}} function.
#' @param ... further arguments passed to or from other methods.
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
#'   ive <- individual_variable_effect(x = model_rf, data = x_train,
#'                                     predict_function = p_fun,
#'                                     new_observation = x_train[1,])
#'  ive
#' }
#'
#' @export

print.individual_variable_effect <- function(x, ...) {
  class(x) <- "data.frame"
  print(head(x))
}
