test_that("study1_simulate_data creates a clean constant-slope data set", {
  set.seed(101)

  dat <- study1_simulate_data(
    n_clusters = 4,
    cluster_size = 10,
    beta = 0.25,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = "none",
    contamination_prop = 0.05,
    contamination_size = 10,
    leverage_size = 10
  )

  expect_equal(nrow(dat), 40)
  expect_equal(nlevels(dat$cluster), 4)
  expect_equal(as.integer(table(dat$cluster)), rep(10L, 4))
  expect_false(any(dat$contaminated))
  expect_equal(dat$x, dat$x_clean)
  expect_equal(dat$out, dat$out_clean)
})


test_that("vertical contamination is applied within every cluster", {
  set.seed(102)

  dat <- study1_simulate_data(
    n_clusters = 4,
    cluster_size = 40,
    beta = 0,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = "vertical",
    contamination_prop = 0.05,
    contamination_size = 10,
    leverage_size = 10
  )

  contaminated_by_cluster <- tapply(
    dat$contaminated,
    dat$cluster,
    sum
  )

  expect_equal(as.integer(contaminated_by_cluster), rep(2L, 4))
  expect_equal(dat$x, dat$x_clean)
  expect_true(all(
    abs(dat$out[dat$contaminated] - dat$out_clean[dat$contaminated]) == 10
  ))
})


test_that("bad leverage contamination changes x and the outcome", {
  set.seed(103)

  dat <- study1_simulate_data(
    n_clusters = 4,
    cluster_size = 20,
    beta = 0,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 2,
    contamination = "bad_leverage",
    contamination_prop = 0.05,
    contamination_size = 8,
    leverage_size = 6
  )

  expect_equal(sum(dat$contaminated), 4)
  expect_true(all(abs(dat$x[dat$contaminated]) == 12))
  expect_true(all(
    abs(dat$out[dat$contaminated] - dat$out_clean[dat$contaminated]) == 8
  ))
})


test_that("pwr_func_study1 returns summaries and replicate-level results", {
  result <- pwr_func_study1(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0,
    contamination = "none",
    reps = 2,
    methods = c("cr2", "cats", "cats_trunc"),
    seed = 104,
    keep_replicates = TRUE
  )

  expect_named(result, c("summary", "replicates", "settings"))
  expect_equal(nrow(result$summary), 3)
  expect_equal(nrow(result$replicates), 6)
  expect_setequal(
    result$summary$model,
    c("cr2", "cats", "cats_trunc")
  )
  expect_true(all(c(
    "bias",
    "rejection_rate",
    "rejection_rate_se",
    "rmse",
    "coverage",
    "coverage_se",
    "failure_rate"
  ) %in% names(result$summary)))
  expect_false("rrmse" %in% names(result$summary))
})


test_that("pwr_func_study1 is reproducible", {
  result_one <- pwr_func_study1(
    n_clusters = 6,
    cluster_size = 20,
    reps = 2,
    methods = "cr2",
    seed = 105,
    keep_replicates = TRUE
  )
  result_two <- pwr_func_study1(
    n_clusters = 6,
    cluster_size = 20,
    reps = 2,
    methods = "cr2",
    seed = 105,
    keep_replicates = TRUE
  )

  expect_equal(result_one$settings, result_two$settings)
  expect_equal(
    result_one$replicates[setdiff(names(result_one$replicates), "runtime_sec")],
    result_two$replicates[setdiff(names(result_two$replicates), "runtime_sec")]
  )
  expect_equal(
    result_one$summary[setdiff(names(result_one$summary), "mean_runtime_sec")],
    result_two$summary[setdiff(names(result_two$summary), "mean_runtime_sec")]
  )
})


test_that("pwr_func_study1 can run all six methods", {
  skip_on_cran()
  skip_if_not_installed("pbkrtest")

  result <- suppressWarnings(pwr_func_study1(
    n_clusters = 5,
    cluster_size = 20,
    beta = 0.25,
    contamination = "vertical",
    reps = 1,
    seed = 106,
    keep_replicates = TRUE
  ))

  expect_setequal(result$summary$model, study1_method_names())
  expect_equal(nrow(result$replicates), 6)
})


test_that("pwr_func_study1 validates key inputs", {
  expect_error(
    pwr_func_study1(n_clusters = 2, reps = 1),
    "n_clusters"
  )
  expect_error(
    pwr_func_study1(methods = "not_a_method", reps = 1),
    "methods"
  )
  expect_error(
    pwr_func_study1(contamination_prop = 1.1, reps = 1),
    "contamination_prop"
  )
})


test_that("replicate-level results can be omitted", {
  result <- pwr_func_study1(
    n_clusters = 5,
    cluster_size = 20,
    reps = 1,
    methods = "cr2",
    seed = 107,
    keep_replicates = FALSE
  )

  expect_null(result$replicates)
})
