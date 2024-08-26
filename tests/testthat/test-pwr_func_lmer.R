test_that("pwr_func_lmer works and produces output of the correct length with no
          correlation", {

            set.seed(235)

            pwr_out <- suppressWarnings(pwr_func_lmer(reps = 2))

            expect_equal(length(pwr_out), 9)
            expect_setequal(names(pwr_out), c("model", "mean_coef",
                                              "rejection_rate", "rejection_rate_se",
                                              "rmse", "rrmse", "coverage", "avg_ci_width", "success"))
            expect_setequal(pwr_out$model, c("lme", "ri", "lme_kr", "ri_kr", "cats", "cats_trunc",
                                             "cats_robust", "cats_robustbase"))
          })


test_that("pwr_func_lmer works and produces output of the correct length", {

  set.seed(475)

  pwr_out <- suppressWarnings(pwr_func_lmer(reps = 2, cor_mat = diag(2), corvars = list(c("x1", "x3"))))

  expect_equal(length(pwr_out), 9)
  expect_setequal(names(pwr_out), c("model", "mean_coef",
                                    "rejection_rate", "rejection_rate_se",
                                    "rmse", "rrmse", "coverage", "avg_ci_width", "success"))
  expect_setequal(pwr_out$model, c("lme", "ri", "lme_kr", "ri_kr", "cats", "cats_trunc",
                                   "cats_robust", "cats_robustbase"))

})


test_that("pwr_func_lmer works and produces output of the correct length for a
          different cor_mat", {

  set.seed(345)

  pwr_out <- suppressWarnings(pwr_func_lmer(betas = list("int" = 0, "x1" = -0.25, "x2" = 2, "x3" = 10),
                                            reps = 2, cor_mat = matrix(data = c(1,0.2, 0.13, 1), nrow =2, ncol =2),
                                            corvars = list(c("x1", "x3"))))

  expect_equal(length(pwr_out), 9)
  expect_setequal(names(pwr_out), c("model", "mean_coef",
                                    "rejection_rate", "rejection_rate_se",
                                    "rmse", "rrmse", "coverage", "avg_ci_width",
                                    "success"))
  expect_setequal(pwr_out$model, c("lme", "ri", "lme_kr", "ri_kr", "cats", "cats_trunc",
                                   "cats_robust", "cats_robustbase"))

})

test_that("pwr_func_lmer works and produces NaN for mean coefficient and 0 success
           for cats when samplewithin clusters is too small", {

            set.seed(345)

            pwr_out <- suppressWarnings(pwr_func_lmer(betas = list("int" = 0, "x1" = -0.25, "x2" = 2, "x3" = 10),
                                                      n_time = 5,
                                                      reps = 2, cor_mat = matrix(data = c(1,0.2, 0.13, 1), nrow =2, ncol =2),
                                                      corvars = list(c("x1", "x3"))))

            expect_equal(length(pwr_out), 9)

            #Check that the mean coefficient for cats is NaN and that success is 0
            expect_equal(pwr_out[pwr_out$model == "cats",2], NaN)
            expect_equal(pwr_out[pwr_out$model == "cats",9], 0)

            expect_setequal(names(pwr_out), c("model", "mean_coef",
                                              "rejection_rate", "rejection_rate_se",
                                              "rmse", "rrmse", "coverage", "avg_ci_width",
                                              "success"))
            expect_setequal(pwr_out$model, c("lme", "ri", "lme_kr", "ri_kr", "cats", "cats_trunc",
                                             "cats_robust", "cats_robustbase"))

          })

test_that("pwr_func_lmer throws error when non-normally distributed variable is
          included in corvars", {

            set.seed(345)
            testthat::expect_error(pwr_func_lmer(betas = list("int" = 0, "x1" = -0.25, "x2" = 2, "x3" = 10),
                                                 n_time = 5,
                                                 reps = 2,
                                                 cor_mat = matrix(data = c(1,0.2, 0.13, 1), nrow =2, ncol =2),
                                                 corvars = list(c("x1", "x2"))))
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
            expect_setequal(pwr_out$model, c("lme", "ri", "lme_kr", "ri_kr", "cats", "cats_trunc",
                                             "cats_robust", "cats_robustbase"))
          })

test_that("pwr_func_lmer works with linear time index", {
  set.seed(123)

  pwr_out <- suppressWarnings(pwr_func_lmer(
    betas = list("int" = 0, "x1" = -5, "x2" = 2, "x3" = 10, "time" = 0.5),
    reps = 2,
    n_time = 10,
    time_index = "linear"
  ))

  expect_equal(length(pwr_out), 9)
  expect_setequal(names(pwr_out), c("model", "mean_coef", "rejection_rate", "rejection_rate_se",
                                    "rmse", "rrmse", "coverage", "avg_ci_width", "success"))
  expect_setequal(pwr_out$model, c("lme", "ri", "lme_kr", "ri_kr", "cats", "cats_trunc",
                                   "cats_robust", "cats_robustbase"))
})

test_that("pwr_func_lmer works with custom time index function", {
  set.seed(456)

  custom_time <- function(n) sqrt(seq_len(n))

  pwr_out <- suppressWarnings(pwr_func_lmer(
    betas = list("int" = 0, "x1" = -5, "x2" = 2, "x3" = 10, "time" = 1),
    reps = 2,
    n_time = 15,
    time_index = custom_time
  ))

  expect_equal(length(pwr_out), 9)
  expect_setequal(names(pwr_out), c("model", "mean_coef", "rejection_rate", "rejection_rate_se",
                                    "rmse", "rrmse", "coverage", "avg_ci_width", "success"))
  expect_setequal(pwr_out$model, c("lme", "ri", "lme_kr", "ri_kr", "cats", "cats_trunc",
                                   "cats_robust", "cats_robustbase"))
})

test_that("pwr_func_lmer handles different n_time values with linear time index", {
  set.seed(789)

  pwr_out <- suppressWarnings(pwr_func_lmer(
    betas = list("int" = 0, "x1" = -5, "x2" = 2, "x3" = 10, "time" = 0.3),
    reps = 2,
    N = 3,
    n_time = c(5, 10, 15),
    time_index = "linear"
  ))

  expect_equal(length(pwr_out), 9)
  expect_setequal(names(pwr_out), c("model", "mean_coef", "rejection_rate", "rejection_rate_se",
                                    "rmse", "rrmse", "coverage", "avg_ci_width", "success"))
  expect_setequal(pwr_out$model, c("lme", "ri", "lme_kr", "ri_kr", "cats", "cats_trunc",
                                   "cats_robust", "cats_robustbase"))
})

test_that("pwr_func_lmer works with linear time index and time as random slope", {
  set.seed(123)

  pwr_out <- suppressWarnings(pwr_func_lmer(
    betas = list("int" = 0, "x1" = -5, "x2" = 2, "x3" = 10, "time" = 0.5),
    reps = 2,
    n_time = 10,
    r_slope = "time",
    time_index = "linear"
  ))

  expect_equal(length(pwr_out), 9)
  expect_setequal(names(pwr_out), c("model", "mean_coef", "rejection_rate", "rejection_rate_se",
                                    "rmse", "rrmse", "coverage", "avg_ci_width", "success"))
  expect_setequal(pwr_out$model, c("lme", "ri", "lme_kr", "ri_kr", "cats", "cats_trunc",
                                   "cats_robust", "cats_robustbase"))
})
