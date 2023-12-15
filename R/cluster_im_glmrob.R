cluster_im_glmRob <-function(robmod, dat, cluster, ci.level = 0.95, report = TRUE,
                             drop = TRUE, return.vcv = FALSE,
                             ...){


  form <- robmod$formula                                                   # what is the formula of this model?
  variables <- all.vars(form)                                           # what variables are in this model?
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
  process_cluster <- function(clust_i, data, formula, family, method, ind.variables, drop, ...){
    clust.ind <- which(clust == clust_i)  # select obs in cluster i
    clust.dat <- data[clust.ind,]  # create the cluster i data set
    clust.mod <- suppressWarnings(tryCatch(robust::glmRob(formula,
                                                          data = clust.dat,
                                                          family = family,
                                                          method = method,
                                                          ...),  # run cluster model
                                           error = function(e){return(NULL)}))

    if(is.null(clust.mod) == FALSE ){
      if(clust.mod$converged == 0){clust.mod <- NULL}                  # judge GLM as failure if convergence not achieved
    }
    fail <- is.null(clust.mod)                                         # determine whether the GLM process created an error


    # should we stop if one cluster-specific model does not converge?
    if(drop==FALSE){
      if(fail == T){stop("cluster-specific model returned error (try drop = TRUE)", call.=FALSE)}

      # detect whether variables were dropped in individual clusters
      if(length(rownames(summary(clust.mod)$coefficients)) != length(ind.variables)){
        stop("cluster-specific model(s) dropped variables; ensure that all variables vary within clusters", call.=FALSE)
      }

      b.clust_i <- stats::coefficients(clust.mod)[ind.variables]

    }else{
      if(fail == F){
        # detect whether variables were dropped in individual clusters
        if(length(rownames(summary(clust.mod)$coefficients)) != length(ind.variables)){
          stop("cluster-specific model(s) dropped variables; ensure that all variables vary within clusters", call.=FALSE)
        }
        b.clust_i <- stats::coefficients(clust.mod)[ind.variables]
      }else{
        b.clust_i  <- NA
      }
    }
  }

  # Use lapply to iterate over each unique cluster
  results <- lapply(unique(clust), process_cluster, data = dat, formula = form,
                    family = robmod$family, method = robmod$method,
                    ind.variables = ind.variables, drop = drop, ...)

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










# cluster_im_glmrob <-function(robmod, dat, cluster, ci.level = 0.95, report = TRUE,
#                              drop = TRUE, truncate = FALSE, return.vcv = FALSE,
#                              ...){
#
#
#   form <- robmod$formula                                                   # what is the formula of this model?
#   variables <- all.vars(form)                                           # what variables are in this model?
#   clust.name <- all.vars(cluster)                                       # what is the name of the clustering variable?
#   used.idx <- which(rownames(dat) %in% rownames(robmod$model))             # what were the actively used observations in the model?
#   dat <- dat[used.idx,]                                                 # keep only active observations
#   clust <- as.vector(unlist(dat[[clust.name]]))                         # store cluster index in convenient vector
#   G<-length(unique(clust))                                              # how many clusters are in this model?
#   ind.variables.full <- names(stats::coefficients(robmod))                        # what independent variables are in this model?
#   ind.variables <- rownames(summary(robmod)$coefficients)                  # what non-dropped independent variables in this model?
#
#   b.clust <- matrix(data = NA, nrow = G, ncol = length(ind.variables))  # a matrix to store the betas
#   n.clust <- c()
#
#   G.o <- G
#   for(i in 1:G){
#
#     clust.ind <- which(clust == unique(clust)[i])                                             # select obs in cluster i
#
#     clust.dat <- dat[clust.ind,]                                                              # create the cluster i data set
#     clust.mod <- suppressWarnings(tryCatch(robust::glmRob(form,
#                                                           data = clust.dat,
#                                                           family = robmod$family,
#                                                           method = robmod$method,
#                                                           ...),  # run cluster model
#                                            error = function(e){return(NULL)}))
#
#     if(is.null(clust.mod) == FALSE ){
#       if(clust.mod$converged == 0){clust.mod <- NULL}                  # judge GLM as failure if convergence not achieved
#     }
#     fail <- is.null(clust.mod)                                         # determine whether the GLM process created an error
#
#
#     # should we stop if one cluster-specific model does not converge?
#     if(drop==FALSE){
#       if(fail == T){stop("cluster-specific model returned error (try drop = TRUE)", call.=FALSE)}
#
#       # detect whether variables were dropped in individual clusters
#       if(length(rownames(summary(clust.mod)$coefficients)) != length(ind.variables)){
#         stop("cluster-specific model(s) dropped variables; ensure that all variables vary within clusters", call.=FALSE)
#       }
#
#       b.clust[i,] <- stats::coefficients(clust.mod)[ind.variables]                                    # store the cluster i beta coefficient
#
#     }else{
#       if(fail == F){
#         # detect whether variables were dropped in individual clusters
#         if(length(rownames(summary(clust.mod)$coefficients)) != length(ind.variables)){
#           stop("cluster-specific model(s) dropped variables; ensure that all variables vary within clusters", call.=FALSE)
#         }
#
#         b.clust[i,] <- stats::coefficients(clust.mod)[ind.variables]                                  # store the cluster i beta coefficient
#       }else{
#         b.clust[i,] <- NA
#       }
#     }
#
#   }
#
#   if(drop==TRUE){
#     dropped.nc <- 0                                                             # store number of non-converged clusters
#     b.clust <- stats::na.omit(b.clust)
#     dropped.nc <- length(attr(b.clust, "na.action"))                            # record number of models dropped
#     G.t <- dim(b.clust)[1]
#     if(G.t == 0){stop("all clusters were dropped (see help file).")}
#   }
#
#   # remove clusters with outlying betas
#   dropped <- 0                                                                 # store number of outlying estimates
#   if(truncate==TRUE){                                                          # if drop outlying betas...
#
#     IQR <- apply(FUN=stats::quantile, MARGIN=2, X=b.clust, probs=c(0.25, 0.75))       # calculate inter-quartile range
#
#     b.clust.save <- b.clust                                                    # non-outlying beta identifier
#     for(i in 1:dim(b.clust)[2]){
#       b.clust.save[,i] <- ifelse( abs(b.clust[,i]) >                           # determine which estimates to save
#                                     (abs(mean(b.clust[,i])) + 6*abs(IQR[2,i] - IQR[1,i])), 0, 1)
#     }
#
#     save.clust <- apply(X=b.clust.save, MARGIN=1, FUN=min)                     # which rows had outlying betas
#     dropped <- dim(b.clust)[1] - sum(save.clust)                               # how many clusters dropped?
#
#     b.clust.adj <- cbind(b.clust, save.clust)                                  # append the outlier ID to beta matrix
#
#     b.clust <- subset(b.clust,                                                 # drop the outlying betas
#                       subset=(save.clust==1), select=1:dim(b.clust)[2])
#
#   }
#
#   G <- dim(b.clust)[1]
#   if(G == 0){stop("all clusters were dropped (see help file).")}
#
#
#   b.hat <- colMeans(b.clust)                                # calculate the avg beta across clusters
#   b.dev <- sweep(b.clust, MARGIN = 2, STATS = b.hat)        # sweep out the avg betas
#   #s.hat <- sqrt( (1 / (G-1)) * colSums(b.dev^2) )          # manually calculate the SE of beta estimates across clusters (deprecated)
#   vcv.hat <- stats::cov(b.dev)                                     # calculate VCV matrix
#   rownames(vcv.hat) <- ind.variables
#   colnames(vcv.hat) <- ind.variables
#   s.hat <- sqrt(diag(vcv.hat))                              # calculate standard error
#
#   t.hat <- sqrt(G) * (b.hat / s.hat)                        # calculate t-statistic
#
#   names(b.hat) <- ind.variables
#
#   # compute p-val based on # of clusters
#   p.out <- 2*pmin( stats::pt(t.hat, df = G-1, lower.tail = TRUE), stats::pt(t.hat, df = G-1, lower.tail = FALSE) )
#
#
#   # compute CIs
#   ci.lo <- b.hat - stats::qt((1-ci.level)/2, df=(G-1), lower.tail=FALSE)*(s.hat/sqrt(G))
#   ci.hi <- b.hat + stats::qt((1-ci.level)/2, df=(G-1), lower.tail=FALSE)*(s.hat/sqrt(G))
#
#   out <- matrix(p.out, ncol=1)
#   rownames(out) <- ind.variables
#
#   out.p <- cbind( ind.variables, round(out, 3))
#   out.p <- rbind(c("variable name", "cluster-adjusted p-value"), out.p)
#
#   out.ci <- cbind(ci.lo, ci.hi)
#   rownames(out.ci) <- ind.variables
#   colnames(out.ci) <- c("CI lower", "CI higher")
#
#   print.ci <- cbind(ind.variables, ci.lo, ci.hi)
#   print.ci <- rbind(c("variable name", "CI lower", "CI higher"), print.ci)
#
#
#   printmat <- function(m){
#     utils::write.table(format(m, justify="right"), row.names=F, col.names=F, quote=F, sep = "   ")
#   }
#
#   if(report==T){
#     cat("\n", "Cluster-Adjusted p-values: ", "\n", "\n")
#     printmat(out.p)
#
#     cat("\n", "Confidence Intervals (centered on cluster-averaged results):", "\n", "\n")
#     printmat(print.ci)
#
#     if(G.o > G){
#       cat("\n", "Note:", G.o - G, "clusters were dropped (see help file).", "\n", "\n")
#     }
#
#     if(length(ind.variables) < length(ind.variables.full)){
#       cat("\n", "\n", "****", "Note: ", length(ind.variables.full) - length(ind.variables), " variables were unidentified in the model and are not reported.", "****", "\n", sep="")
#       cat("Variables not reported:", "\n", sep="")
#       cat(ind.variables.full[!ind.variables.full %in% ind.variables], sep=", ")
#       cat("\n", "\n")
#     }
#
#   }
#
#
#   out.list<-list()
#   out.list[["p.values"]] <- out
#   out.list[["ci"]] <- out.ci
#   if(return.vcv == TRUE){out.list[["vcv.hat"]] <- vcv.hat}
#   if(return.vcv == TRUE){out.list[["beta.bar"]] <- b.hat}
#   return(invisible(out.list))
#
# }
