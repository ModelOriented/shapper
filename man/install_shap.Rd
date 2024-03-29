% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install_shap.R
\name{install_shap}
\alias{install_shap}
\title{Install shap Python library}
\usage{
install_shap(method = "auto", conda = "auto", envname = NULL)
}
\arguments{
\item{method}{Installation method. By default, "auto".
It is passed to the \code{\link[reticulate]{py_install}} function from package `reticulate`.}

\item{conda}{Path to conda executable.
It is passed to the \code{\link[reticulate]{py_install}} function from package `reticulate`.}

\item{envname}{Name of environment to install shapp package into. If NULL it will install into default
It is passed to the \code{\link[reticulate]{py_install}} function from package `reticulate`.

To use conda installation execute install_shap(method = "conda", envname = nameofenv)
Please keep in mind that winodws accepts only conda instalations}
}
\description{
Install shap Python library
}
\examples{
\dontrun{
install_shap(method = "auto", conda = "auto")
}

}
