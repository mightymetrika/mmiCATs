# Extracting variables and clusters
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

  return(invisible(out.list))
}

