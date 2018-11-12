#' @title Install shap Python library
#'
#'
#' @export
install_shap <- function(method = "auto", conda = "auto") {
  reticulate::py_install("shap", method = method, conda = conda)
}
