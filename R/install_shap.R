#' @title Install shap Python library
#'
#' @param method Installation method. By default, "auto".
#' It is passed to the \code{\link[reticulate]{py_install}} function form package `reticulate`.
#' @param conda Path to conda executable.
#' It is passed to the \code{\link[reticulate]{py_install}} function form package `reticulate`.
#'
#' @export
install_shap <- function(method = "auto", conda = "auto") {
  reticulate::py_install("shap", method = method, conda = conda)
}
