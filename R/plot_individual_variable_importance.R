#' Plots Attributions for Variables of Individual Prediction
#'
#' Function 'plot.individual_variable_effect' plots variables effects plots.
#'
#' @param x an individual variable effect explainer produced with function `individual_variable_effect()`
#' @param id of observation. By default first observation is taken.
#' @param ylevel level of y. By default first level is taken.
#' @param digits number of decimal places (round) or significant digits (signif) to be used.
#' See the \code{rounding_function} argument.
#' @param rounding_function function that is to used for rounding numbers.
#' It may be \code{signif()} which keeps a specified number of significant digits.
#' Or the default \code{round()} to have the same precision for all components
#' @param ... other explainers that shall be plotted together
#'
#' @import ggplot2
#'
#' @return a ggplot2 object
#'
#' @export
plot.individual_variable_effect <- function(x, id = 1, ylevel = NULL, digits = 3, rounding_function = round,  ...) {

  if(is.null(ylevel)) ylevel <- x$`_ylevel_`[1]

  df <- x[x$`_id_` == id & x$`_ylevel_` == ylevel, ]

  varvalue <- df[1, 1:nrow(df)]
  nums <- vapply(varvalue, is.numeric, FUN.VALUE = logical(1))
  varvalue[,nums] <- rounding_function(varvalue, digits)
  df$`_varvalue_` <- t(varvalue)
  df <- df[order(df$`_sign_`, -abs(df$`_attribution_`)), ]

  model_output <- rounding_function(df$`_yhat_`[1], digits)
  base_value <- rounding_function(df$`_yhat_mean_`[1], digits)

  ggplot(df, aes(x = `_id_`, y = -`_attribution_`, fill = `_sign_`, label = `_attribution_`, color = `_vname_`)) +
    geom_col(position = position_stack(), color = "grey") +
    scale_y_continuous(
      labels=function(x) rounding_function(x+df$`_yhat_`[1], digits),
      sec.axis = sec_axis(~.,
        breaks = c(0-base_value, 0),
        labels = c(paste("base value =", base_value), paste("model output =", model_output))
      )
    ) +
    scale_x_discrete(expand = c(0,0)) +
    guides(fill=FALSE, color = FALSE) +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.x=element_blank()
    ) +
    geom_text(
      aes(label = paste(`_vname_`, "=", `_varvalue_`, "\n", rounding_function(`_attribution_`, digits))),
      position = position_stack(vjust = 0.5),
      color = "black"
    ) +
    geom_errorbar(aes(ymax=0, ymin=0), lwd=2, color = "black")



}
