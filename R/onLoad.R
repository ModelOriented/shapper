# global reference to shap (will be initialized in .onLoad)
shap_reference <- NULL

.onLoad <- function(libname, pkgname) {
  tryCatch(
  shap_reference <<- reticulate::import("shap", delay_load = TRUE)
  )
}
