#' @title Plots Attributions for Variables of Individual Prediction
#'
#' @description  Function 'plot.individual_variable_effect' plots variables effects plots.
#'
#' @param x an individual variable effect explainer produced with function `individual_variable_effect()`
#' @param ... other explainers that shall be plotted together
#' @param id of observation. By default first observation is taken.
#' @param digits number of decimal places (round) or significant digits (signif) to be used. See the \code{rounding_function} argument.
#' @param rounding_function function that is to used for rounding numbers. It may be \code{signif()} which keeps a specified number of significant digits. Or the default \code{round()} to have the same precision for all components
#' @param show_predcited show arrows for predicted values.
#' @param show_attributions show attributions values.
#' @param cols A vector of characters defining faceting groups on columns dimension. Possible values: 'label', 'id', 'ylevel'.
#' @param rows A vector of characters defining faceting groups on rows dimension. Possible values: 'label', 'id', 'ylevel'.
#'
#'
#' @import ggplot2
#'
#' @return a ggplot2 object
#'
#' @examples
#' \dontrun{
#' library("shapper")
#' library("DALEX")
#' library("randomForest")
#' Y_train <- HR$status
#' x_train <- HR[ , -6]
#' set.seed(123)
#' model_rf <- randomForest(x = x_train, y = Y_train)
#' p_function <- function(model, data) predict(model, newdata = data, type = "prob")
#'
#' ive_rf <- individual_variable_effect(model_rf, data = x_train, predict_function = p_function,
#'                                      new_observation = x_train[1:2,], nsamples = 50)
#' plot(ive_rf)
#' }
#'
#' @method plot individual_variable_effect
#'
#' @importFrom stats reorder
#'
#' @export
plot.individual_variable_effect <- function(x, ..., id = 1, digits = 2, rounding_function = round,
                                            show_predcited = TRUE, show_attributions = TRUE,
                                            cols = c("label", "id"), rows = "ylevel") {

  `_id_` <- `_attribution_` <- `_sign_` <- `_vname_` <- `_varvalue_` <- `_yhat_mean_` <- `_yhat_` <- NULL


  dfl <- c(list(x), list(...))
  x <- do.call(rbind, dfl)
  class(x) <- "data.frame"


  x <- x[x$`_id_` == id, ]
  values <- as.vector(x[1 , x$`_vname_`[1:length(unique(x$`_vname_`))]])
  variable_values <- values[x$`_vname_`]
  numeric_values <- sapply(variable_values, is.numeric)
  variable_values[numeric_values] <- rounding_function(variable_values[numeric_values], digits)
  x$`_varvalue_` <- t(variable_values)
  x$`_ext_vname_` <- paste(x$`_vname_`, "=", x$`_varvalue_`)
  x$`_ext_vname_` <- reorder(x$`_ext_vname_`, x$`_attribution_`, function(z) -sum(abs(z)))
  levels(x$`_ext_vname_`) <- paste(sapply(1:6, substr, x="        ", start=1), levels(x$`_ext_vname_`))


  maybe_prediction_arrow <- if(show_predcited == TRUE) {
    geom_segment(aes(x = "_predicted_",xend = "_predicted_",
      y = `_yhat_`, yend = `_yhat_mean_`), size = 1, color="black",
      arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "closed"))
  } else {
    maybe_prediction_arrow <- NULL
  }

  maybe_prediction_text <- if(show_attributions == TRUE & show_predcited == TRUE){
    geom_text(aes(x = "_predicted_",
      y = `_yhat_`, label = rounding_function(`_yhat_`, digits)), nudge_x = 0.45, color="black")
  } else {
    NULL
  }

  maybe_attributions <- if(show_attributions == TRUE){
    geom_text(aes(label = rounding_function(`_attribution_`, digits)), nudge_x = 0.45)
  } else {
    NULL
  }

  rows <- paste0("`_", rows, "_`")
  cols <- paste(paste0("`_", cols, "_`"), collapse = "+")
  grid_formula <- as.formula(paste(rows, "~", cols))

  id_labeller <- function(value) paste0("id = ", value)
  label_labeller <- function(value) {
    if(length(unique(x$`_label_`)) > 1) return(value)
    ""
    }


  ggplot(x, aes(x= `_ext_vname_`, xend=`_ext_vname_`,
                yend = `_yhat_mean_`, y = `_yhat_mean_` + `_attribution_`,
                color=`_sign_`)) +
    geom_segment(arrow = arrow(length=unit(0.20,"cm"), ends="first", type = "closed")) +
    maybe_attributions +
    maybe_prediction_arrow +
    maybe_prediction_text +
    geom_hline(aes(yintercept = `_yhat_mean_`)) +
    facet_grid(grid_formula,
      labeller = labeller(`_id_` = as_labeller(id_labeller), `_label_` = as_labeller(label_labeller))) +
    scale_color_manual(values =  c(`-` = "#d8b365", `0` = "#f5f5f5", `+` = "#5ab4ac",
                                   X = "darkgrey")) +
    coord_flip() + theme_minimal() + theme(legend.position="none") +
    xlab("") + ylab("Shapley values") + ggtitle("Shapley values")


}
