# global reference to shap (will be initialized in .onLoad)
shap <- NULL

.onLoad <- function(libname, pkgname) {
  shap <<- reticulate::import("shap", delay_load = TRUE)
}
