#' @title Plots Attributions for Variables of Individual Prediction
#'
#' @description  Function 'plot.individual_variable_effect' plots variables effects plots.
#'
#' @param x an individual variable effect explainer produced with function `individual_variable_effect()`
#' @param ... other explainers that shall be plotted together
#' @param id of observation. By default first observation is taken.
#' @param digits number of decimal places (round) or significant digits (signif) to be used. See the \code{rounding_function} argument.
#' @param rounding_function function that is to used for rounding numbers. It may be \code{signif()} which keeps a specified number of significant digits. Or the default \code{round()} to have the same precision for all components
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
#' x_train$gender <- as.numeric(x_train$gender)
#' set.seed(123)
#' model_rf <- randomForest(x = x_train, y = Y_train)
#' p_fun <- function(x, data){
#'   predict(x, newdata = data, type = "prob")
#'   }
#'   res <- individual_variable_effect(x = model_rf, data = x_train,
#'                                     predict_function = p_fun,
#'                                     new_observation = x_train[1,])
#'                                     plot(res)
#' }
#'
#' @method plot individual_variable_effect
#'
#' @importFrom stats reorder
#'
#' @export
plot.individual_variable_effect <- function(x, ..., id = 1, digits = 3, rounding_function = round) {

  `_id_` <- `_attribution_` <- `_sign_` <- `_vname_` <- `_varvalue_` <- `_yhat_mean_` <- `_yhat_` <- NULL

  dfl <- c(list(x), list(...))
  x <- do.call(rbind, dfl)
  class(x) <- "data.frame"

  x <- x[x$`_id_` %in% id, ]

  x$`_vname_` <- reorder(x$`_vname_`, x$`_attribution_`, function(z) -sum(abs(z)))
  levels(x$`_vname_`) <- paste(sapply(1:6, substr, x="        ", start=1), levels(x$`_vname_`))

  ggplot(x, aes(x=`_vname_`, xend=`_vname_`,
                yend = `_yhat_mean_`, y = `_yhat_mean_` + `_attribution_`,
                color=`_sign_`)) +
    geom_segment(arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed")) +
    geom_text(aes(label = round(`_attribution_`, 2)), nudge_x = 0.45) +
    geom_segment(aes(x = "_predicted_",xend = "_predicted_",
                     y = `_yhat_`, yend = `_yhat_mean_`), size = 2, color="black",
                 arrow = arrow(length=unit(0.30,"cm"), ends="first", type = "closed")) +
    geom_text(aes(x = "_predicted_",
                  y = `_yhat_`, label = round(`_yhat_`, 2)), nudge_x = 0.45, color="black") +
    geom_hline(aes(yintercept = `_yhat_mean_`)) +
    facet_grid(`_id_` + `_ylevel_` ~ `_label_`) +
    scale_color_manual(values =  c(`-` = "#d8b365", `0` = "#f5f5f5", `+` = "#5ab4ac",
                                   X = "darkgrey")) +
    coord_flip() + theme_minimal() + theme(legend.position="none") + xlab("") + ylab("")

}
