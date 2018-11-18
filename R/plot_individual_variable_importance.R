#' Plots Attributions for Variables of Individual Prediction
#'
#' Function 'plot.individual_variable_effect' plots variables effects plots.
#'
#' @param x an individual variable effect explainer produced with function `individual_variable_effect()`
#' @param id of observation. By default first observation is taken.
#' @param ylevel level of y. By default first level is taken.
#' @param ... other explainers that shall be plotted together
#'
#' @import ggplot2
#'
#' @return a ggplot2 object
#'
#' @export
plot.individual_variable_effect <- function(x, id = 1, ylevel = NULL, ...) {

  if(is.null(ylevel)) ylevel <- x$`_ylevel_`[1]

  df <- x[x$`_id_` == id & x$`_ylevel_` == ylevel, ]
  df$`_sign_` <- factor(sign(df$`_attribution`))

  varvalue <- df[1, 1:nrow(df)]
  nums <- vapply(varvalue, is.numeric, FUN.VALUE = logical(1))
  varvalue[,nums] <- round(varvalue, digits = 2)
  df$`_varvalue_` <- t(varvalue)
  df <- df[order(df$`_sign_`, -abs(df$`_attribution_`)), ]

  ggplot(df, aes(x = `_id_`, y = `_attribution_`, fill = `_sign_`, label = `_attribution_`, color = `_vname_`)) +
    geom_col(position = position_stack(), color = "grey") +
    scale_y_continuous(labels=function(x)round(x+df$`_yhat_`[1], 2),
                       position = "right") +
    guides(fill=FALSE, color = FALSE) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank()) +
   geom_text(aes(label = paste(`_vname_`, "=", `_varvalue_`)),
             position = position_stack(vjust = 0.5),
             color = "black") +
    coord_flip()



}
