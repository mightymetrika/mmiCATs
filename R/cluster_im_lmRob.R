#' Cluster-Adjusted Confidence Intervals And p-Values Robust Linear Models
#'
#' Performs cluster-adjusted inference on a robust linear model object, using
#' robust linear regression within each cluster. This function is designed to
#' handle models where observations are clustered, and standard errors need to be
#' adjusted to account for this clustering. The function applies a robust linear
#' regression model to each cluster and then aggregates the results.
#'
#' @param robmod A robust linear model object created using robust::lmRob() or
#'               robustbase::lmrob().
#' @param formula A formula or a string that can be coerced to a formula.
#' @param dat A data frame containing the data used in the model.
#' @param cluster A formula indicating the clustering
#'                variable in `dat`.
#' @param ci.level Confidence level for the confidence intervals, default is 0.95.
#' @param drop Logical; if TRUE, drops clusters where the model does not converge.
#' @param return.vcv Logical; if TRUE, the variance-covariance matrix of the
#'                   cluster-averaged coefficients will be returned.
#' @param engine Set the engine to "robust" to use robust::lmRob() or "robustbase"
#'               to use robustbase::lmrob(). Default is "robust".
#' @param ... Additional arguments to be passed to the robust::lmRob() or the
#'            robustbase::lmrob() function.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{p.values}{A matrix of p-values for each independent variable.}
#'   \item{ci}{A matrix with the lower and upper bounds of the confidence intervals
#'             for each independent variable.}
#'   \item{vcv.hat}{The variance-covariance matrix of the cluster-averaged
#'                  coefficients, returned if `return.vcv` is TRUE.}
#'   \item{beta.bar}{The cluster-averaged coefficients, returned if `return.vcv`
#'                   is TRUE.}
#' }
#'
#' @examples
#' form <- Sepal.Length ~ Petal.Length + Petal.Width
#' mod <- robust::lmRob(formula = form, dat = iris)
#' cluster_im_lmRob(robmod = mod, formula = form, dat = iris,cluster = ~Species)
#'
#' @export
cluster_im_lmRob <-function(robmod, formula, dat, cluster, ci.level = 0.95,
                             drop = TRUE, return.vcv = FALSE, engine = "robust",
                             ...){

  # Check if robmod is a lmRob object from either robust or robustbase package
  if (!(inherits(robmod, "lmRob") | inherits(robmod, "lmrob"))) {
    stop("robmod must be a robust linear model object created using robust::lmRob() or robustbase::lmrob().")
  }

  # Check if formula is a valid R formula
  if (!(inherits(formula, "formula") |
        inherits(stats::as.formula(formula),
                 "formula"))) {
    stop("formula must be a valid R formula.")
  }

  # Check if dat is a data frame
  if (!is.data.frame(dat)) {
    stop("dat must be a data frame.")
  }

  # Check if cluster is a valid R formula
  if (!inherits(cluster, "formula")) {
    stop("cluster must be a valid R formula indicating the clustering variable.")
  }

  # Check if ci.level is a numeric value between 0 and 1
  if (!is.numeric(ci.level) || ci.level <= 0 || ci.level >= 1) {
    stop("ci.level must be a numeric value between 0 and 1.")
  }

  # Check if drop is a logical value
  if (!is.logical(drop)) {
    stop("drop must be a logical value.")
  }

  # Check if return.vcv is a logical value
  if (!is.logical(return.vcv)) {
    stop("return.vcv must be a logical value.")
  }

  # Check if engine is either "robust" or "robustbase"
  if (!(engine %in% c("robust", "robustbase"))) {
    stop("engine must be either 'robust' or 'robustbase'.")
  }

  # Extract model info
  .info <- info(formula = formula, cluster = cluster, dat = dat, robmod = robmod)

  # Function to process each cluster
  process_cluster <- function(clust_i, pdata, formula, ind_variables, drop, ...){
    clust.ind <- which(.info$clust == clust_i)  # select obs in cluster i
    clust.dat <- pdata[clust.ind,]  # create the cluster i data set

    clust.mod <- fit_model(engine, formula = formula, data = clust.dat, ...)

    fail <- is.null(clust.mod)                                         # determine whether the GLM process created an error

    # Handle convergence and dropped variables
    b.clust_i <- fail_drop(drop, fail, clust.mod, ind_variables)

  }

  # Use lapply to iterate over each unique cluster
  results <- lapply(unique(.info$clust), process_cluster, pdata = .info$dat, formula = formula,
                    ind_variables = .info$ind.variables, drop = drop, ...)

  # Process results and return
  return(process_results(results, ind_variables = .info$ind.variables,
                         ci.level, drop, return.vcv))

}

#' Fit a Linear Model with Robust Estimation
#'
#' This function fits a linear model using robust estimation methods.
#' It allows the use of either the 'robust' or 'robustbase' packages for fitting
#' the model.
#'
#' @param engine A character string specifying the engine to be used for model
#'               fitting. Must be either "robust" or "robustbase".
#' @param formula An object of class formula (or one that can be coerced to that
#'                class): a symbolic description of the model to be fitted.
#' @param data A dataframe
#' @param ... Additional arguments to be passed to the underlying fitting function.
#'
#' @return A fitted model.
#'
#'
#' @keywords internal
fit_model <- function(engine, formula, data, ...) {
  switch(engine,
         "robust" = suppressWarnings(tryCatch(robust::lmRob(formula = formula, data = data, ...),
                                              error = function(e) NULL)),
         "robustbase" = suppressWarnings(tryCatch(robustbase::lmrob(formula = formula, data = data, ...),
                                                  error = function(e) NULL)),
         stop("The engine parameter must be set to 'robust' or 'robustbase'.")
  )
}
