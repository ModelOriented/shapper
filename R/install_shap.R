#' @title Install shap Python library
#'
#' @param method Installation method. By default, "auto".
#' It is passed to the \code{\link[reticulate]{py_install}} function form package `reticulate`.
#' @param conda Path to conda executable.
#' It is passed to the \code{\link[reticulate]{py_install}} function form package `reticulate`.
#'
#' @examples
#' \dontrun{
#'   install_shap((method = "auto", conda = "auto")
#' }
#'
#' @import reticulate
#'
#' @export
install_shap <- function(method = "auto", conda = "auto") {
  py_install("shap", method = method, conda = conda)

  tryCatch({import("shap")},
     error = function(e) {message("Could not import shap libary. Try install python dependencies. Use reticulate::py_install('numpy').")})
}
