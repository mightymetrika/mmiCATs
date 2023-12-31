% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_im_lmRob.R
\name{fit_model}
\alias{fit_model}
\title{Fit a Generalized Linear Model with Robust Estimation}
\usage{
fit_model(engine, formula, data, ...)
}
\arguments{
\item{engine}{A character string specifying the engine to be used for model
fitting. Must be either "robust" or "robustbase".}

\item{formula}{An object of class \code{\link[stats]{formula}} (or one that
can be coerced to that class): a symbolic description of the
model to be fitted.}

\item{data}{A dataframe}

\item{...}{Additional arguments to be passed to the underlying fitting function.}

\item{family}{A description of the error distribution and link function to be
used in the model (See \code{\link[stats]{family}} for details
of family functions).}

\item{method}{Fitting method to be used. Can vary depending on the selected
engine.}
}
\value{
A fitted model.
}
\description{
This function fits a generalized linear model using robust estimation methods.
It allows the use of either the 'robust' or 'robustbase' packages for fitting
the model.
}
\seealso{
\code{\link[robust]{glmRob}}, \code{\link[robustbase]{glmrob}}
}
\keyword{internal}
