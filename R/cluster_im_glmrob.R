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

    # Handle convergence and dropped variables
    b.clust_i <- fail_drop(drop, fail, clust.mod, ind_variables)
  }

  # Use lapply to iterate over each unique cluster
  results <- lapply(unique(.info$clust), process_cluster, pdata = .info$dat, formula = robmod$formula,
                    family = robmod$family, method = robmod$method,
                    ind_variables = .info$ind.variables, drop = drop, ...)

  # Process results and return
  return(process_results(results, ind_variables = .info$ind.variables,
                         ci.level, drop, return.vcv))

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

