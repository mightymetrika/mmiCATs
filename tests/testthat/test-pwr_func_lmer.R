# betas = list("int" = 0, "x1" = -5, "x2" = 2, "x3" = 10)
# dists = list("x1" = stats::rnorm, "x2" = stats::rbinom, "x3" = stats::rnorm)
# distpar = list("x1" = list(mean = 0, sd = 1), "x2" = list(size = 1, prob = 0.4), "x3" = list(mean = 1, sd = 2))
# N = 25
# reps = 10
# alpha = 0.05
# var_intr = "x1"
# grp = "ID"
# mod = paste0("out ~ x1 + x2 + x3 + (x3|ID)")
# catsmod = "out ~ x1 + x2 + x3"
# r_slope = "x1"
# r_int = "int"
# n_time = 4
# mean_i = 0
# var_i = 1
# mean_s = 0
# var_s = 1
# cov_is = 0
# mean_r = 0
# var_r = 1
# cor_mat = NULL
# corvars = NULL



test_that("pwr_func_lmer works and produces output of the correct length with no
          correlation", {

            set.seed(235)

            pwr_out <- suppressWarnings(pwr_func_lmer(reps = 5))

            expect_equal(length(pwr_out), 9)
            expect_setequal(names(pwr_out), c("model", "mean_coef",
                                              "rejection_rate", "rejection_rate_se",
                                              "rmse", "rrmse", "coverage", "avg_ci_width", "success"))
            expect_setequal(pwr_out$model, c("lme", "ri", "cats", "cats_trunc",
                                             "cats_robust", "cats_robustbase"))
          })


test_that("pwr_func_lmer works and produces output of the correct length", {

  set.seed(475)

  pwr_out <- suppressWarnings(pwr_func_lmer(reps = 5, cor_mat = diag(2), corvars = list(c("x1", "x3"))))

  expect_equal(length(pwr_out), 9)
  expect_setequal(names(pwr_out), c("model", "mean_coef",
                                    "rejection_rate", "rejection_rate_se",
                                    "rmse", "rrmse", "coverage", "avg_ci_width", "success"))
  expect_setequal(pwr_out$model, c("lme", "ri", "cats", "cats_trunc",
                                   "cats_robust", "cats_robustbase"))

})


test_that("pwr_func_lmer works and produces output of the correct length for a
          different cor_mat", {

  set.seed(345)

  pwr_out <- suppressWarnings(pwr_func_lmer(betas = list("int" = 0, "x1" = -0.25, "x2" = 2, "x3" = 10),
                                            reps = 5, cor_mat = matrix(data = c(1,0.2, 0.13, 1), nrow =2, ncol =2),
                                            corvars = list(c("x1", "x3"))))

  expect_equal(length(pwr_out), 9)
  expect_setequal(names(pwr_out), c("model", "mean_coef",
                                    "rejection_rate", "rejection_rate_se",
                                    "rmse", "rrmse", "coverage", "avg_ci_width",
                                    "success"))
  expect_setequal(pwr_out$model, c("lme", "ri", "cats", "cats_trunc",
                                   "cats_robust", "cats_robustbase"))

})

test_that("pwr_func_lmer works and produces NaN for mean coefficient and 0 success
           for cats when samplewithin clusters is too small", {

            set.seed(345)

            pwr_out <- suppressWarnings(pwr_func_lmer(betas = list("int" = 0, "x1" = -0.25, "x2" = 2, "x3" = 10),
                                                      n_time = 5,
                                                      reps = 5, cor_mat = matrix(data = c(1,0.2, 0.13, 1), nrow =2, ncol =2),
                                                      corvars = list(c("x1", "x3"))))

            expect_equal(length(pwr_out), 9)

            #Check that the mean coefficient for cats is NaN and that success is 0
            expect_equal(pwr_out[3,2], NaN)
            expect_equal(pwr_out[3,9], 0)

            expect_setequal(names(pwr_out), c("model", "mean_coef",
                                              "rejection_rate", "rejection_rate_se",
                                              "rmse", "rrmse", "coverage", "avg_ci_width",
                                              "success"))
            expect_setequal(pwr_out$model, c("lme", "ri", "cats", "cats_trunc",
                                             "cats_robust", "cats_robustbase"))

          })

test_that("xcats model works", {

            set.seed(100)

            pwr_out <- suppressWarnings(pwr_func_lmer(betas = list("int" = 0, "x1" = .25, "x2" = 1.5),
                                                      dists = list("x1" = stats::rnorm, "x2" = stats::rnorm),
                                                      distpar = list("x1" = list(mean = 0, sd = 1), "x2" = list(mean = 0, sd = 4)),
                                                      N = 20,
                                                      reps = 1,
                                                      alpha = 0.05,
                                                      var_intr = "x1",
                                                      grp = "ID",
                                                      mod = paste0("out ~ x1 + x2 + (x2|ID)"),
                                                      catsmod = "out ~ x1 + x2",
                                                      r_slope = "x2",
                                                      r_int = "int",
                                                      n_time = 20,
                                                      mean_i = 0,
                                                      var_i = 1,
                                                      mean_s = 0,
                                                      var_s = 1,
                                                      cov_is = 0.1,
                                                      mean_r = 0,
                                                      var_r = 1,
                                                      cor_mat = matrix(c(1,0.1,0.1,1), 2, 2),
                                                      corvars = list(c("x1", "x2"))))

            expect_equal(length(pwr_out), 9)
            expect_setequal(names(pwr_out), c("model", "mean_coef",
                                              "rejection_rate", "rejection_rate_se",
                                              "rmse", "rrmse", "coverage", "avg_ci_width",
                                              "success"))
            expect_setequal(pwr_out$model, c("lme", "ri", "cats", "cats_trunc",
                                             "cats_robust", "cats_robustbase"))
          })

