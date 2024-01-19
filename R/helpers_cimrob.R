#' Extract Model Information for Cluster-Adjusted Robust Inference
#'
#' This internal function extracts essential information from a model object and
#' associated data, specifically for use in cluster-adjusted robust inference
#' functions like `cluster_im_glmRob` and `cluster_im_lmRob`. It handles variable
#' extraction, clustering information, and filtering the dataset based on the
#' model's usage.
#'
#' @param formula A formula used in the model fitting.
#' @param cluster A formula or a character string indicating the clustering
#'                variable in `dat`.
#' @param dat A data frame containing the data used in the model.
#' @param robmod A robust model object from which to extract information.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{variables}{Vector of all variable names used in the model.}
#'   \item{clust.name}{Name of the clustering variable.}
#'   \item{dat}{Filtered data set containing only the observations used in the
#'              model.}
#'   \item{clust}{Vector representing the cluster index for each observation in
#'                `dat`.}
#'   \item{ind.variables.full}{Names of all independent variables including
#'                             dropped ones, if any, in the model.}
#'   \item{ind.variables}{Names of non-dropped independent variables used in the
#'                        model.}
#' }
#'
#' @keywords internal
info <- function(formula = NULL, cluster, dat, robmod){

  # Get variables in model
  variables <- ifelse(inherits(robmod, "lmRob") | inherits(robmod, "lmrob"),
                      all.vars(stats::as.formula(formula)),
                      all.vars(robmod$formula))

  # Get clustering variable
  clust.name <- all.vars(cluster)

  # Filter to observations actively used in the model
  dat <- dat[which(rownames(dat) %in% rownames(robmod$model)),]

  # Get cluster index
  clust <- as.vector(dat[[clust.name]])

  # Get independent variables that are in the model
  ind.variables.full <- names(stats::coefficients(robmod))

  # Get non-dropped independent variables in the model
  ind.variables <- rownames(summary(robmod)$coefficients)

  return(list(variables = variables, clust.name = clust.name, dat = dat,
              clust = clust, ind.variables.full = ind.variables.full,
              ind.variables = ind.variables))

}

#' Handle Model Fitting Failures and Variable Dropping
#'
#' This internal function is designed to handle failures in cluster-specific model
#' fitting and variable dropping issues in the context of cluster-adjusted robust
#' inference functions. It checks for model fitting failures and whether independent
#' variables have been dropped in the model.
#'
#' @param drop Logical; if TRUE, allows the function to return `NA` for failed
#'             model fits, otherwise stops execution with an error message.
#' @param fail Logical; indicates whether the model fitting process has failed.
#' @param clust.mod A model object resulting from cluster-specific fitting.
#' @param ind_variables A character vector of independent variable names expected
#'                      in the model.
#'
#' @return If `fail` is TRUE and `drop` is FALSE, the function stops with an
#'         error message.
#'         If `fail` is TRUE and `drop` is TRUE, it returns `NA`.
#'         If `fail` is FALSE, it returns the coefficients for the independent
#'         variables in `clust.mod`.
#'
#' @keywords internal
fail_drop <- function(drop, fail, clust.mod, ind_variables){
  # Check for failure in model fitting
  if (fail == T) {
    if (drop == F) {
      stop("Cluster-specific model returned error (try drop = TRUE)", call. = FALSE)
    }
    return(NA) # If drop is TRUE and model fitting failed, return NA
  }

  # At this point, we know the model fitting did not fail
  # Check if the number of coefficients matches the number of independent variables
  if (length(rownames(summary(clust.mod)$coefficients)) != length(ind_variables)) {
    stop("Cluster-specific model(s) dropped variables; ensure that all variables vary within clusters", call. = FALSE)
  }

  # Return the coefficients for the independent variables
  stats::coefficients(clust.mod)[ind_variables]
}

#' Process Cluster-Adjusted Robust Inference Results
#'
#' This internal function processes results from cluster-specific model fittings
#' for cluster-robust inference functions. It combines results, computes
#' variance-covariance matrix, standard errors, t-statistics, p-values, and
#' confidence intervals for each independent variable.
#'
#' @param results A list of results from cluster-specific model fittings.
#' @param ind_variables A vector of independent variable names for which the
#'                      results are computed.
#' @param ci.level Confidence level for the confidence intervals.
#' @param drop Logical; if TRUE, clusters with failed model fits are omitted
#'             from the results.
#' @param return.vcv Logical; if TRUE, returns the variance-covariance matrix of
#'                   the cluster-averaged coefficients.
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
#' @keywords internal
process_results <- function(results, ind_variables, ci.level, drop, return.vcv){
  # Combine the results into a matrix
  b.clust <- do.call(rbind, results)

  if(drop){
    b.clust <- stats::na.omit(b.clust)
    if(nrow(b.clust) == 0){stop("all clusters were dropped (see help file).")}
  }

  G <- nrow(b.clust)
  if(G == 0){stop("all clusters were dropped (see help file).")}

  # calculate the avg beta across clusters
  b.hat <- colMeans(b.clust)

  # sweep out the avg betas
  b.dev <- sweep(b.clust, MARGIN = 2, STATS = b.hat)

  # calculate VCV matrix
  vcv.hat <- stats::cov(b.dev)
  rownames(vcv.hat) <- ind_variables
  colnames(vcv.hat) <- ind_variables

  # calculate standard error
  s.hat <- sqrt(diag(vcv.hat))

  # calculate t-statistic
  t.hat <- sqrt(G) * (b.hat / s.hat)

  names(b.hat) <- ind_variables

  # compute p-val based on # of clusters
  p.out <- 2*pmin( stats::pt(t.hat, df = G-1, lower.tail = TRUE), stats::pt(t.hat, df = G-1, lower.tail = FALSE) )


  # compute CIs
  ci.lo <- b.hat - stats::qt((1-ci.level)/2, df=(G-1), lower.tail=FALSE)*(s.hat/sqrt(G))
  ci.hi <- b.hat + stats::qt((1-ci.level)/2, df=(G-1), lower.tail=FALSE)*(s.hat/sqrt(G))

  out <- matrix(p.out, ncol=1)
  rownames(out) <- ind_variables

  out.ci <- cbind(ci.lo, ci.hi)
  rownames(out.ci) <- ind_variables
  colnames(out.ci) <- c("CI lower", "CI higher")

  # Combine results into a list
  out.list <- list(
    p.values = out,
    ci = out.ci,
    vcv.hat = if(return.vcv) vcv.hat,
    beta.bar = if(return.vcv) b.hat
  )

  return(out.list)
}

