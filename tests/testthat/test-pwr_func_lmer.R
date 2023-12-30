test_that("pwr_func_lmer works and produces output of the correct length", {

  pwr_out <- suppressWarnings(pwr_func_lmer(reps = 5))

  expect_equal(length(pwr_out), 6)
  expect_equal(length(pwr_out$cats_robust), 6)
  expect_equal(length(pwr_out$cats_robustbase), 6)

})
