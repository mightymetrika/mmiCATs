cluster_im_glmRob <-function(robmod, dat, cluster, ci.level = 0.95,
                             drop = TRUE, return.vcv = FALSE, engine = "robust",
                             ...){

  # Extract model info
  .info <- info(formula = NULL, cluster = cluster, dat = dat, robmod = robmod)

  # Function to process each cluster
  process_cluster <- function(clust_i, pdata, formula, family, method, ind_variables, drop, ...){
    clust.ind <- which(.info$clust == clust_i)  # select obs in cluster i
    clust.dat <- pdata[clust.ind,]  # create the cluster i data set

    clust.mod <- fit_model_g(engine, formula = formula, family = family,
                             data = clust.dat, method = method, ...)

    if(is.null(clust.mod) == FALSE & engine == "robustbase"){
      if(clust.mod$converged == 0){clust.mod <- NULL}                  # judge GLM as failure if convergence not achieved
    }
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
  results <- lapply(unique(.info$clust), process_cluster, pdata = .info$dat, formula = robmod$formula,
                    family = robmod$family, method = robmod$method,
                    ind_variables = .info$ind.variables, drop = drop, ...)

  # Combine the results into a matrix
  b.clust <- matrix(data = NA, nrow = length(unique(.info$clust)), ncol = length(.info$ind.variables))
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
  rownames(vcv.hat) <- .info$ind.variables
  colnames(vcv.hat) <- .info$ind.variables
  s.hat <- sqrt(diag(vcv.hat))                              # calculate standard error

  t.hat <- sqrt(G) * (b.hat / s.hat)                        # calculate t-statistic

  names(b.hat) <- .info$ind.variables

  # compute p-val based on # of clusters
  p.out <- 2*pmin( stats::pt(t.hat, df = G-1, lower.tail = TRUE), stats::pt(t.hat, df = G-1, lower.tail = FALSE) )


  # compute CIs
  ci.lo <- b.hat - stats::qt((1-ci.level)/2, df=(G-1), lower.tail=FALSE)*(s.hat/sqrt(G))
  ci.hi <- b.hat + stats::qt((1-ci.level)/2, df=(G-1), lower.tail=FALSE)*(s.hat/sqrt(G))

  out <- matrix(p.out, ncol=1)
  rownames(out) <- .info$ind.variables

  out.p <- cbind( .info$ind.variables, round(out, 3))
  out.p <- rbind(c("variable name", "cluster-adjusted p-value"), out.p)

  out.ci <- cbind(ci.lo, ci.hi)
  rownames(out.ci) <- .info$ind.variables
  colnames(out.ci) <- c("CI lower", "CI higher")

  print.ci <- cbind(.info$ind.variables, ci.lo, ci.hi)
  print.ci <- rbind(c("variable name", "CI lower", "CI higher"), print.ci)


  printmat <- function(m){
    utils::write.table(format(m, justify="right"), row.names=F, col.names=F, quote=F, sep = "   ")
  }

  out.list<-list()
  out.list[["p.values"]] <- out
  out.list[["ci"]] <- out.ci
  if(return.vcv == TRUE){out.list[["vcv.hat"]] <- vcv.hat}
  if(return.vcv == TRUE){out.list[["beta.bar"]] <- b.hat}
  return(invisible(out.list))

}

fit_model_g <- function(engine, formula, data, family, method, ...) {
  switch(engine,
         "robust" = suppressWarnings(tryCatch(robust::glmRob(formula = formula, family = family, data = data, method = method, ...),
                                              error = function(e) NULL)),
         "robustbase" = suppressWarnings(tryCatch(robustbase::glmrob(formula = formula, family = family, data = data, method = method, ...),
                                                  error = function(e) NULL)),
         stop("The engine parameter must be set to 'robust' or 'robustbase'.")
  )
}

