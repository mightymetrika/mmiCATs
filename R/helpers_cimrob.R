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
