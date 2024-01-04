test_that("pwr_func_lmer works and produces output of the correct length with no
          correlation", {

            pwr_out <- suppressWarnings(pwr_func_lmer(reps = 5))

            expect_equal(length(pwr_out), 8)
            expect_setequal(names(pwr_out), c("model", "mean_coef",
                                              "rejection_rate", "rejection_rate_se",
                                              "rmse", "rrmse", "coverage", "avg_ci_width"))
            expect_setequal(pwr_out$model, c("lme", "ri", "cats", "cats_trunc",
                                             "cats_robust", "cats_robustbase"))
          })


test_that("pwr_func_lmer works and produces output of the correct length", {

  pwr_out <- suppressWarnings(pwr_func_lmer(reps = 5, cor_mat = diag(2), corvars = list(c("x1", "x3"))))

  expect_equal(length(pwr_out), 8)
  expect_setequal(names(pwr_out), c("model", "mean_coef",
                                    "rejection_rate", "rejection_rate_se",
                                    "rmse", "rrmse", "coverage", "avg_ci_width"))
  expect_setequal(pwr_out$model, c("lme", "ri", "cats", "cats_trunc",
                                   "cats_robust", "cats_robustbase"))

})


test_that("pwr_func_lmer works and produces output of the correct length for a
          different cor_mat", {

  pwr_out <- suppressWarnings(pwr_func_lmer(betas = list("int" = 0, "x1" = -0.25, "x2" = 2, "x3" = 10),
                                            reps = 5, cor_mat = matrix(data = c(1,0.2, 0.13, 1), nrow =2, ncol =2)))

  expect_equal(length(pwr_out), 8)
  expect_setequal(names(pwr_out), c("model", "mean_coef",
                                    "rejection_rate", "rejection_rate_se",
                                    "rmse", "rrmse", "coverage", "avg_ci_width"))
  expect_setequal(pwr_out$model, c("lme", "ri", "cats", "cats_trunc",
                                   "cats_robust", "cats_robustbase"))

})



