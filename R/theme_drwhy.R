#' DrWhy Theme for ggplot objects
#'
#' @param n number of colors for color palette
#'
#' @return theme for ggplot2 objects
#' @export
#' @rdname theme_drwhy
theme_drwhy_colors <- function(n = 2) {
  if (n == 1) return("#4378bf")
  if (n == 2) return(c( "#4378bf", "#8bdcbe"))
  if (n == 3) return(c( "#4378bf", "#f05a71", "#8bdcbe"))
  if (n == 4) return(c( "#4378bf", "#f05a71", "#8bdcbe", "#ffa58c"))
  if (n == 5) return(c( "#4378bf", "#f05a71", "#8bdcbe", "#ae2c87", "#ffa58c"))
  if (n == 6) return(c( "#4378bf", "#46bac2", "#8bdcbe", "#ae2c87", "#ffa58c", "#f05a71"))
  c( "#4378bf", "#46bac2", "#371ea3", "#8bdcbe", "#ae2c87", "#ffa58c", "#f05a71")[((0:(n-1)) %% 7) + 1]
}
