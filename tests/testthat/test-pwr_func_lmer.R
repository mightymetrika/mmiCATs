# var_intr = "Petal.Length"
# alpha = 0.05
#
# # lmer
# lmer_fit <- lmerTest::lmer(Sepal.Length ~ Petal.Length + Petal.Width + (1|Species), data = iris)
# lmer_tidy <- broom.mixed::tidy(lmer_fit)
#
# (lmer_coef <- lmer_tidy[lmer_tidy$term == var_intr, "estimate"])
# (lmer_p <- lmer_tidy[lmer_tidy$term == var_intr, "p.value"] < alpha)
#
# # Cats Truncate
# cats_prefit_trunc <- stats::glm(Sepal.Length ~ Petal.Length + Petal.Width, data=iris)
# cats_fit_trunc <- clusterSEs::cluster.im.glm(cats_prefit_trunc, dat = iris, cluster = ~ Species,
#                                        report = T, truncate = TRUE)
#
# cats_tidy_trunc <- broom::tidy(cats_prefit_trunc)
#
# (cats_coef_trunc <- cats_tidy_trunc[cats_tidy_trunc$term == var_intr, "estimate"])
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
#
# # Cats Rob
# cats_prefit_rob <- robust::lmRob(Sepal.Length ~ Petal.Length + Petal.Width, data=iris)
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
#   tst <- pwr_func_lmer(dists = list("int" = rnorm(25*5, 30, 12), "x1" = rnorm(25*5, 15, 10), "x2" = rbinom(25*5, 1, 0.4)))
#   tst <- pwr_func_lmer(reps = 100)#,
#                        cor_mat = matrix(c(1,0.2,0.2, 1), 2, 2))
#
#   expect_equal(2 * 2, 4)
# })

# tst <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + Petal.Length + (1|Species),
#                       data = iris)
