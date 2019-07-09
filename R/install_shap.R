#' @title Install shap Python library
#'
#' @param method Installation method. By default, "auto".
#' It is passed to the \code{\link[reticulate]{py_install}} function from package `reticulate`.
#' @param conda Path to conda executable.
#' It is passed to the \code{\link[reticulate]{py_install}} function from package `reticulate`.
#' @param envname Name of environment to install shapp package into. If NULL it will install into default
#' It is passed to the \code{\link[reticulate]{py_install}} function from package `reticulate`.
#' 
#' To use conda installation execute install_shap(method = "conda", envname = nameofenv)
#'
#' @examples
#' \dontrun{
#'   install_shap((method = "auto", conda = "auto")
#' }
#'
#' @importFrom reticulate py_install import
#'
#' @export

install_shap <-
  function(method = "auto",
           conda = "auto",
           envname = NULL) {
    py_install("shap",
               envname = envname,
               method = method,
               conda = conda)
    tryCatch({
      import("shap")
    },
    error = function(e) {
      message(
        "Could not import shap libary. Try installing python dependencies: reticulate::py_install(c('numpy', 'pandas')).\n
                                  Check if the python-tk package is installed."
      )
    })
  }

