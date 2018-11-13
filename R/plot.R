#' Plot Generic for Break Down Objects
#'
#' @param x the model model of 'individual_variable_effect' class
#' @param ... other parameters
#' @param class number of class if model predicts multiple classes
#' @param observation number of observation if more than one was provided to 'individual_variable_effect'
#'
#'
#' @export
plot.individual_variable_effect <- function(x, ..., class = 1, observation = 1){
  shap_values <- x$shap_values
  expected_value <- x$expected_value
  new_observation <- x$new_observation

  shap$force_plot(expected_value[class], shap_values[[class]], new_observation[observation, ])
}

