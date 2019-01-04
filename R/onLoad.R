# global reference to shap (will be initialized in .onLoad)
shap_reference <- NULL

.onLoad <- function(libname, pkgname) {
  shap_reference <<- reticulate::import("shap", delay_load = TRUE)
}
