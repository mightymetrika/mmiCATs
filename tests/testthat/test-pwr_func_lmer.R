# test_that("pieces of pwr_func_lmer works", {
#
#   N = 25
#   n_time = 5
#   betas = list("int" = 1, "x1" = -5, "x2" = 0)
#   #dists = list("int" = rnorm(N*n_time, 30, 12), "x1" = rnorm(N*n_time, 15, 10), "x2" = rbinom(N*n_time, 1, 0.4))
#   dists = list("x1" = rnorm(N*n_time, 15, 10), "x2" = rbinom(N*n_time, 1, 0.4))
#   reps = 10
#   alpha = 0.05
#   var_intr = "x1"
#   grp = "ID"
#   mod = paste0("out ~ x1 + x2 + (1|", grp, ")")
#   r_slope = "x1"
#   r_int = "int"
#
#   mean_i = 0
#   var_i = 1
#   mean_s = 0
#   var_s = 1
#   cov_is = 0
#   mean_r = 0
#   var_r = 1
#
#   tst <- pwr_func_lmer(dists = list("int" = rnorm(25*5, 30, 12), "x1" = rnorm(25*5, 15, 10), "x2" = rbinom(25*5, 1, 0.4)))
#   tst <- pwr_func_lmer(reps = 100,
#                        cor_mat = matrix(c(1,0.2,0.2, 1), 2, 2))
#
#   expect_equal(2 * 2, 4)
# })

# tst <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + Petal.Length + (1|Species),
#                       data = iris)
