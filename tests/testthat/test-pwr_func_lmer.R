test_that("pwr_func_lmer works and produces output of the correct length", {

  pwr_out <- suppressWarnings(pwr_func_lmer(reps = 5))

  expect_equal(length(pwr_out), 6)
  expect_equal(length(pwr_out$cats_robust), 6)
  expect_equal(length(pwr_out$cats_robustbase), 6)

})


test_that("pwr_func_lmer works and produces output of the correct length for a
          different cor_mat", {

  pwr_out <- suppressWarnings(pwr_func_lmer(reps = 5, cor_mat = matrix(data = c(1,0.2, 0.13, 1), nrow =2, ncol =2)))

  expect_equal(length(pwr_out), 6)
  expect_equal(length(pwr_out$cats_robust), 6)
  expect_equal(length(pwr_out$cats_robustbase), 6)

})


# NEED TO FIX THIS #
test_that("pwr_func_lmer works and produces output of the correct length with no
          correlation", {

            # THIS SHOULD WORK WHEN THE SECOND ARGUMENTS ARE NULL
            pwr_out <- suppressWarnings(pwr_func_lmer(reps = 5, cor_mat = diag(1), corvars = "x1"))

            expect_equal(length(pwr_out), 6)
            expect_equal(length(pwr_out$cats_robust), 6)
            expect_equal(length(pwr_out$cats_robustbase), 6)

          })
