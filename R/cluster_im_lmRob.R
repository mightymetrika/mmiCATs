#' Cluster-Robust Inference for Linear Models
#'
#' Performs cluster-robust inference on a linear model object, using robust linear regression within each cluster. This function is designed to handle models where observations are clustered, and standard errors need to be adjusted to account for this clustering. The function applies a robust linear regression model to each cluster and then aggregates the results.
#'
#' @param robmod A robust linear model object, typically created using `lmrob()` from the `robustbase` package.
#' @param formula A formula
#' @param dat A data frame containing the data used in the model.
#' @param cluster A formula or a character string indicating the clustering variable in `dat`.
#' @param ci.level Confidence level for the confidence intervals, default is 0.95.
#' @param report Logical; if TRUE, prints the cluster-adjusted p-values and confidence intervals.
#' @param drop Logical; if TRUE, drops clusters where the model does not converge.
#' @param return.vcv Logical; if TRUE, the variance-covariance matrix of the cluster-averaged coefficients will be returned.
#' @param ... Additional arguments to be passed to the `lmRob()` function.
#'
#' @return An invisible list containing the following elements:
#' \describe{
#'   \item{p.values}{A matrix of p-values for each independent variable.}
#'   \item{ci}{A matrix with the lower and upper bounds of the confidence intervals for each independent variable.}
#'   \item{vcv.hat}{The variance-covariance matrix of the cluster-averaged coefficients, returned if `return.vcv` is TRUE.}
#'   \item{beta.bar}{The cluster-averaged coefficients, returned if `return.vcv` is TRUE.}
#' }
#'
#' @examples
#' form <- Sepal.Length ~ Petal.Length + Petal.Width
#' mod <- robust::lmRob(formula = form, dat = iris)
#' cluster_im_lmRob(robmod = mod, formula = form, dat = iris,cluster = ~Species)
#'
#' @export
cluster_im_lmRob <-function(robmod, formula, dat, cluster, ci.level = 0.95, report = TRUE,
                             drop = TRUE, return.vcv = FALSE,
                             ...){

  variables <- all.vars(stats::as.formula(formula))                                           # what variables are in this model?
  clust.name <- all.vars(cluster)                                       # what is the name of the clustering variable?
  used.idx <- which(rownames(dat) %in% rownames(robmod$model))             # what were the actively used observations in the model?
  dat <- dat[used.idx,]                                                 # keep only active observations
  clust <- as.vector(unlist(dat[[clust.name]]))                         # store cluster index in convenient vector
  G<-length(unique(clust))                                              # how many clusters are in this model?
  ind.variables.full <- names(stats::coefficients(robmod))                        # what independent variables are in this model?
  ind.variables <- rownames(summary(robmod)$coefficients)                  # what non-dropped independent variables in this model?

  b.clust <- matrix(data = NA, nrow = G, ncol = length(ind.variables))  # a matrix to store the betas
  n.clust <- c()

  G.o <- G
  # Function to process each cluster
  process_cluster <- function(clust_i, pdata, formula, ind_variables, drop){
    clust.ind <- which(clust == clust_i)  # select obs in cluster i
    clust.dat <- pdata[clust.ind,]  # create the cluster i data set

    # clust.mod <- suppressWarnings(tryCatch(robustbase::lmrob(formula,
    #                                                          data = clust.dat,
    #                                                          ...),  # run cluster model
    #                                        error = function(e){return(NULL)}))
    clust.mod <- suppressWarnings(tryCatch(robust::lmRob(formula,
                                                         data = clust.dat),  # run cluster model
                                           error = function(e){return(NULL)}))


    # if(is.null(clust.mod) == FALSE ){
    #   if(clust.mod$converged == 0){clust.mod <- NULL}                  # judge GLM as failure if convergence not achieved
    # }
    fail <- is.null(clust.mod)                                         # determine whether the GLM process created an error


    # should we stop if one cluster-specific model does not converge?
    if(drop==FALSE){
      if(fail == T){stop("cluster-specific model returned error (try drop = TRUE)", call.=FALSE)}

      # detect whether variables were dropped in individual clusters
      if(length(rownames(summary(clust.mod)$coefficients)) != length(ind_variables)){
        stop("cluster-specific model(s) dropped variables; ensure that all variables vary within clusters", call.=FALSE)
      }

      b.clust_i <- stats::coefficients(clust.mod)[ind_variables]

    }else{
      if(fail == F){
        # detect whether variables were dropped in individual clusters
        if(length(rownames(summary(clust.mod)$coefficients)) != length(ind_variables)){
          stop("cluster-specific model(s) dropped variables; ensure that all variables vary within clusters", call.=FALSE)
        }
        b.clust_i <- stats::coefficients(clust.mod)[ind_variables]
      }else{
        b.clust_i  <- NA
      }
    }
  }

  # Use lapply to iterate over each unique cluster
  results <- lapply(unique(clust), process_cluster, pdata = dat, formula = formula,
                    ind_variables = ind.variables, drop = drop)

  # Combine the results into a matrix
  b.clust <- do.call(rbind, results)


  if(drop==TRUE){
    dropped.nc <- 0                                                             # store number of non-converged clusters
    b.clust <- stats::na.omit(b.clust)
    dropped.nc <- length(attr(b.clust, "na.action"))                            # record number of models dropped
    G.t <- nrow(b.clust)
    if(G.t == 0){stop("all clusters were dropped (see help file).")}
  }

  G <- nrow(b.clust)
  if(G == 0){stop("all clusters were dropped (see help file).")}


  b.hat <- colMeans(b.clust)                                # calculate the avg beta across clusters
  b.dev <- sweep(b.clust, MARGIN = 2, STATS = b.hat)        # sweep out the avg betas
  vcv.hat <- stats::cov(b.dev)                                     # calculate VCV matrix
  rownames(vcv.hat) <- ind.variables
  colnames(vcv.hat) <- ind.variables
  s.hat <- sqrt(diag(vcv.hat))                              # calculate standard error

  t.hat <- sqrt(G) * (b.hat / s.hat)                        # calculate t-statistic

  names(b.hat) <- ind.variables

  # compute p-val based on # of clusters
  p.out <- 2*pmin( stats::pt(t.hat, df = G-1, lower.tail = TRUE), stats::pt(t.hat, df = G-1, lower.tail = FALSE) )


  # compute CIs
  ci.lo <- b.hat - stats::qt((1-ci.level)/2, df=(G-1), lower.tail=FALSE)*(s.hat/sqrt(G))
  ci.hi <- b.hat + stats::qt((1-ci.level)/2, df=(G-1), lower.tail=FALSE)*(s.hat/sqrt(G))

  out <- matrix(p.out, ncol=1)
  rownames(out) <- ind.variables

  out.p <- cbind( ind.variables, round(out, 3))
  out.p <- rbind(c("variable name", "cluster-adjusted p-value"), out.p)

  out.ci <- cbind(ci.lo, ci.hi)
  rownames(out.ci) <- ind.variables
  colnames(out.ci) <- c("CI lower", "CI higher")

  print.ci <- cbind(ind.variables, ci.lo, ci.hi)
  print.ci <- rbind(c("variable name", "CI lower", "CI higher"), print.ci)


  printmat <- function(m){
    utils::write.table(format(m, justify="right"), row.names=F, col.names=F, quote=F, sep = "   ")
  }

  if(report==T){
    cat("\n", "Cluster-Adjusted p-values: ", "\n", "\n")
    printmat(out.p)

    cat("\n", "Confidence Intervals (centered on cluster-averaged results):", "\n", "\n")
    printmat(print.ci)

    if(G.o > G){
      cat("\n", "Note:", G.o - G, "clusters were dropped (see help file).", "\n", "\n")
    }

    if(length(ind.variables) < length(ind.variables.full)){
      cat("\n", "\n", "****", "Note: ", length(ind.variables.full) - length(ind.variables), " variables were unidentified in the model and are not reported.", "****", "\n", sep="")
      cat("Variables not reported:", "\n", sep="")
      cat(ind.variables.full[!ind.variables.full %in% ind.variables], sep=", ")
      cat("\n", "\n")
    }

  }


  out.list<-list()
  out.list[["p.values"]] <- out
  out.list[["ci"]] <- out.ci
  if(return.vcv == TRUE){out.list[["vcv.hat"]] <- vcv.hat}
  if(return.vcv == TRUE){out.list[["beta.bar"]] <- b.hat}
  return(invisible(out.list))

}
