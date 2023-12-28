# cluster_im_lmRob <-function(robmod, dat, cluster, formula, ...){
#
#   #form <- robmod$call$formula
#   #form <- catsmod
#   variables <- all.vars(formula)
#   clust.name <- all.vars(cluster)
#   used.idx <- which(rownames(dat) %in% rownames(robmod$model))
#   dat <- dat[used.idx,]
#   clust <- as.vector(unlist(dat[[clust.name]]))
#   ind.variables <- rownames(summary(robmod)$coefficients)
#
#   # Function to process each cluster
#   process_cluster <- function(clust_i, pdata, formula, ind_variables, ...){
#     clust.ind <- which(clust == clust_i)  # select obs in cluster i
#     clust.dat <- pdata[clust.ind,]  # create the cluster i data set
#
#     clust.mod <- robust::lmRob(formula,
#                   data = clust.dat,
#                   ...)
#
#     return(clust.mod)
#   }
#
#   # Use lapply to iterate over each unique cluster
#   results <- lapply(unique(clust), process_cluster, pdata = dat,
#                     formula = formula,
#                     ind_variables = ind.variables, ...)
#
#   return(results)
#
# }
#
#
# pwr_func_lmer <- function(dataset = iris,
#                           grp = "Species",
#                           catsmod = "Sepal.Length ~ Petal.Length + Petal.Width",
#                           reps = 5) {
#
#   #catsmod <<- stats::as.formula(catsmod)
#   formula <- stats::as.formula(catsmod)
#
#   simulate <- function() {
#
#     dataset$grp <- sample(dataset$grp)
#
#
#     # Fit main model
#     lmrob_fit <- robust::lmRob(catsmod, data = dataset)
#
#     # Fit clust model
#     #cats_fit_rob_cluster <- cluster_im_lmRob(lmrob_fit, dat = dataset, cluster = stats::as.formula(paste0("~ ",grp)))
#     cats_fit_rob_cluster <- cluster_im_lmRob(lmrob_fit, dat = dataset, cluster = stats::as.formula(paste0("~ ", grp)), formula = formula)
#
#
#     return(cats_fit_rob_cluster)
#   }
#
#   all_results <- replicate(reps, simulate(), simplify = FALSE)
#
#   return(all_results)
# }
#
# # Works
# library(robust)
#
# dataset = iris
# grp = "Species"
# catsmod = "Sepal.Length ~ Petal.Length + Petal.Width"
# reps = 5
#
#
# dataset$grp <- sample(dataset$grp)
# lmrob_fit <- robust::lmRob(stats::as.formula(catsmod), data = dataset)
# (cats_fit_rob_cluster <- cluster_im_lmRob(lmrob_fit, dat = dataset,
#                                          cluster = stats::as.formula(paste0("~ ",grp))))
#
# # Clear workspace
# rm(cats_fit_rob_cluster, dataset, lmrob_fit, catsmod, grp, reps)
#
#
# # Does Not Work
# library(robust)
# tst <- pwr_func_lmer()
#
#
#
# library(robust)
# data(stack.dat)
# stack.rob <- lmRob(Loss ~ ., data = stack.dat)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# betas = list("int" = 0, "x1" = -5, "x2" = 2, "x3" = 10)
# dists = list("x1" = stats::rnorm, "x2" = stats::rbinom, "x3" = stats::rnorm)
# distpar = list("x1" = list(mean = 0, sd = 1), "x2" = list(size = 1, prob = 0.4), "x3" = list(mean = 1, sd = 2))
# N = 25
# reps = 10
# alpha = 0.05
# var_intr = "x1"
# grp = "ID"
# mod = paste0("out ~ x1 + x2 + x3 + (1|", grp, ")")
# catsmod = "out ~ x1 + x2 + x3"
# r_slope = "x1"
# r_int = "int"
# n_time = 20
# mean_i = 0
# var_i = 1
# mean_s = 0
# var_s = 1
# cov_is = 0
# mean_r = 0
# var_r = 1
# cor_mat = diag(2)
# corvars = list(c("x1", "x3"))
#
#
#
#
#
# robmod = lmrob_fit
# dat = data
# cluster = stats::as.formula(paste0("~ ",grp))
# drop = TRUE
# report = FALSE
# ci.level = 0.95
# return.vcv = FALSE
#
#
#
# clust_i = unique(clust)
# data = dat
# formula = form
# ind_variables = ind.variables
# drop = drop






# var_intr = "Petal.Length"
# alpha = 0.05
#
# # lmer
# lmer_fit <- lmerTest::lmer(Sepal.Length ~ Petal.Length + Petal.Width + (1|Species), data = iris)
# lmer_tidy <- broom.mixed::tidy(lmer_fit, conf.int = TRUE)
#
# (lmer_coef <- lmer_tidy[lmer_tidy$term == var_intr, "estimate"])
# (lmer_p <- lmer_tidy[lmer_tidy$term == var_intr, "p.value"] < alpha)
# (lmer_low <- lmer_tidy[lmer_tidy$term == var_intr, "conf.low"])
# (lmer_high <- lmer_tidy[lmer_tidy$term == var_intr, "conf.high"])
#
# # Cats Truncate
# cats_prefit_trunc <- stats::glm(Sepal.Length ~ Petal.Length + Petal.Width, data=iris)
# cats_fit_trunc <- clusterSEs::cluster.im.glm(cats_prefit_trunc, dat = iris, cluster = ~ Species,
#                                        report = T, truncate = TRUE, return.vcv = TRUE)
#
# cats_tidy_trunc <- broom::tidy(cats_prefit_trunc)
#
# (cats_coef_trunc <- cats_tidy_trunc[cats_tidy_trunc$term == var_intr, "estimate"])
# (cats_coef_func <- cats_fit_trunc$beta.bar[[var_intr]])
# (cats_p_trunc <- cats_fit_trunc$p.values[var_intr,])
#
# # Cats
# cats_prefit <- stats::glm(Sepal.Length ~ Petal.Length + Petal.Width, data=iris)
# cats_fit <- clusterSEs::cluster.im.glm(cats_prefit, dat = iris, cluster = ~ Species,
#                                              report = T, truncate = FALSE)
#
# cats_tidy <- broom::tidy(cats_prefit)
#
# (cats_coef <- cats_tidy_trunc[cats_tidy$term == var_intr, "estimate"])
# (cats_p <- cats_fit$p.values[var_intr,])
# (cats_low <- cats_fit$ci[var_intr,1])
# (cats_high <- cats_fit$ci[var_intr,2])
#
# # Cats Rob
# cats_prefit_rob <- robust::lmRob(Sepal.Length ~ Petal.Length + Petal.Width, data=iris)
# cats_fit_rob <- cluster_im_lmRob(cats_prefit_rob, Sepal.Length ~ Petal.Length + Petal.Width,
#                                  dat = iris, cluster = ~ Species, return.vcv = TRUE)
#
# cats_prefit_rob <- robustbase::lmrob(Sepal.Length ~ Petal.Length + Petal.Width, data=iris)
# cats_fit_rob <- cluster_im_lmRob(cats_prefit_rob, dat = iris, cluster = ~ Species)
#
# cats_tidy_rob <- broom::tidy(cats_prefit_rob)
#
# (cats_coef_rob <- cats_tidy_rob[cats_tidy_rob$term == var_intr, "estimate"])
# (cats_p_rob <- cats_fit_rob$p.values[var_intr,])
#
#
#
#
# pwr_func_lmer2 <- function(betas = list("int" = 0, "x1" = -5, "x2" = 2, "x3" = 10),
#                           var_intr = "x1") {
#   true_coefficient <- betas[[var_intr]]
#
#   return(true_coefficient)
# }




















#
#
#
#
# test_that("pieces of pwr_func_lmer works", {
#
#   betas = list("int" = 1, "x1" = -5, "x2" = 0, "x3" = 2)
#   dists = list("x1" = stats::rnorm, "x2" = stats::rbinom, "x3" = stats::rnorm)
#   distpar = list("x1" = list(mean = 0, sd = 1), "x2" = list(size = 1, prob = 0.4), "x3" = list(mean = 1, sd = 2))
#   N = 25
#   reps = 100
#   alpha = 0.05
#   var_intr = "x1"
#   grp = "ID"
#   mod = paste0("out ~ x1 + x2 + x3 + (1|", grp, ")")
#   catsmod = "out ~ x1 + x2 + x3"
#   r_slope = "x1"
#   r_int = "int"
#   n_time = 20
#   mean_i = 0
#   var_i = 1
#   mean_s = 0
#   var_s = 1
#   cov_is = 0
#   mean_r = 0
#   var_r = 1
#   cor_mat = diag(2)
#   corvars = list(c("x1", "x3"))
#
  # tst <- pwr_func_lmer(dists = list("int" = rnorm(25*5, 30, 12), "x1" = rnorm(25*5, 15, 10), "x2" = rbinom(25*5, 1, 0.4)))
  # tst <- pwr_func_lmer(reps = 100)#,
  #                      n_time = 20,
  #                      mean_i = 2.51,
  #                      var_i = 0.665,
  #                      betas = list("int" = 2.51, "x1" = 0.89, "x2" = 0.25, "x3" = -0.02))#,
  #                      cor_mat = matrix(c(1,0.2,0.2, 1), 2, 2))
#
#   expect_equal(2 * 2, 4)
# })

# tst <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + Petal.Length + (1|Species),
#                       data = iris)
