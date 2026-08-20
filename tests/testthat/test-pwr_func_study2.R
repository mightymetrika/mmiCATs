test_that("study2_simulate_data creates the specified random-slope DGP", {
  set.seed(901)

  dat <- study2_simulate_data(
    n_clusters = 6,
    cluster_size = 12,
    beta = 0.20,
    intercept = 0.40,
    random_intercept_sd = 0.80,
    random_slope_sd = 0.10,
    residual_sd = 0.70,
    x_sd = 1.20,
    contamination = "none",
    contamination_prop = 0.05,
    contamination_size = 6
  )

  expect_equal(nrow(dat), 72)
  expect_equal(nlevels(dat$cluster), 6)
  expect_equal(as.integer(table(dat$cluster)), rep(12L, 6))
  expect_false(any(dat$contaminated))
  expect_equal(dat$x, dat$x_clean)
  expect_equal(dat$out, dat$out_clean)

  reconstructed <- 0.40 +
    dat$random_intercept +
    dat$true_cluster_slope * dat$x +
    dat$residual

  expect_equal(dat$out_clean, reconstructed, tolerance = 1e-12)
  expect_equal(
    dat$true_cluster_slope,
    0.20 + dat$random_slope,
    tolerance = 1e-12
  )
  expect_equal(
    dat$random_slope,
    0.10 * dat$random_slope_standardized,
    tolerance = 1e-12
  )
})


test_that("Study 2 common draws rescale the same random slopes", {
  settings <- list(
    n_clusters = 8,
    cluster_size = 20,
    beta = 0.10,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = "vertical",
    contamination_prop = 0.05,
    contamination_size = 6
  )

  set.seed(902)
  low <- do.call(
    study2_simulate_data,
    c(settings, list(random_slope_sd = 0.05))
  )

  set.seed(902)
  high <- do.call(
    study2_simulate_data,
    c(settings, list(random_slope_sd = 0.10))
  )

  expect_equal(low$x, high$x)
  expect_equal(low$random_intercept, high$random_intercept)
  expect_equal(low$residual, high$residual)
  expect_equal(
    low$random_slope_standardized,
    high$random_slope_standardized
  )
  expect_equal(
    high$random_slope,
    2 * low$random_slope,
    tolerance = 1e-12
  )
  expect_equal(low$contaminated, high$contaminated)

  expected_clean_difference <-
    (high$random_slope - low$random_slope) * low$x

  expect_equal(
    high$out_clean - low$out_clean,
    expected_clean_difference,
    tolerance = 1e-12
  )
})


test_that("Study 2 vertical contamination matches Study 1 conventions", {
  set.seed(903)

  dat <- study2_simulate_data(
    n_clusters = 5,
    cluster_size = 40,
    beta = 0,
    intercept = 0,
    random_intercept_sd = 1,
    random_slope_sd = 0.10,
    residual_sd = 1.50,
    x_sd = 1,
    contamination = "vertical",
    contamination_prop = 0.05,
    contamination_size = 6
  )

  contaminated_by_cluster <- tapply(
    dat$contaminated,
    dat$cluster,
    sum
  )

  expect_equal(
    as.integer(contaminated_by_cluster),
    rep(2L, 5)
  )
  expect_equal(dat$x, dat$x_clean)
  expect_true(all(
    abs(
      dat$out[dat$contaminated] -
        dat$out_clean[dat$contaminated]
    ) == 9
  ))
})


test_that("Study 2 convergence classification separates singularity", {
  singular_message <- paste(
    "boundary (singular) fit:",
    "see help('isSingular')"
  )
  gradient_message <- paste(
    "Model failed to converge with max|grad| = 0.01",
    "(tol = 0.002, component 1)"
  )

  no_message <- study2_classify_convergence(
    messages = NULL,
    optimizer_code = 0L
  )
  singular_only <- study2_classify_convergence(
    messages = singular_message,
    optimizer_code = 0L
  )
  mixed_messages <- study2_classify_convergence(
    messages = c(singular_message, gradient_message),
    optimizer_code = 0L
  )
  nonzero_code <- study2_classify_convergence(
    messages = singular_message,
    optimizer_code = 1L
  )

  expect_true(no_message$converged)
  expect_true(singular_only$converged)
  expect_equal(
    singular_only$singularity_messages,
    singular_message
  )
  expect_length(singular_only$optimizer_messages, 0L)

  expect_false(mixed_messages$converged)
  expect_equal(
    mixed_messages$optimizer_messages,
    gradient_message
  )

  expect_false(nonzero_code$converged)
})


test_that("singular finite results remain usable", {
  usable_singular <- list(
    estimate = 0.10,
    std_error = 0.04,
    df = 8,
    p_value = 0.04,
    conf_low = 0.01,
    conf_high = 0.19,
    retained_clusters = 10,
    converged = TRUE,
    singular = TRUE
  )
  genuine_failure <- usable_singular
  genuine_failure$converged <- FALSE

  expect_true(study2_result_is_usable(usable_singular))
  expect_false(study2_result_is_usable(genuine_failure))
})


make_study2_validation_data <- function() {
  set.seed(904)

  study2_simulate_data(
    n_clusters = 30,
    cluster_size = 20,
    beta = 0.25,
    intercept = 0.40,
    random_intercept_sd = 0.80,
    random_slope_sd = 0.20,
    residual_sd = 0.70,
    x_sd = 1,
    contamination = "none",
    contamination_prop = 0.05,
    contamination_size = 6
  )
}


test_that("study2_fit_rs matches direct Kenward-Roger calculations", {
  skip_on_cran()
  skip_if_not_installed("pbkrtest")

  dat <- make_study2_validation_data()
  alpha <- 0.05

  fit <- lmerTest::lmer(
    out ~ x + (1 + x || cluster),
    data = dat,
    REML = TRUE
  )
  fit_summary <- summary(fit, ddf = "Kenward-Roger")
  coefficient_row <- stats::coef(
    fit_summary
  )["x", , drop = FALSE]

  estimate <- unname(coefficient_row[1L, "Estimate"])
  std_error <- unname(coefficient_row[1L, "Std. Error"])
  df <- unname(coefficient_row[1L, "df"])
  p_value <- unname(coefficient_row[1L, "Pr(>|t|)"])
  critical_value <- stats::qt(1 - alpha / 2, df = df)
  fitted_sds <- study2_extract_random_effect_sds(fit)

  result <- study2_fit_rs(dat = dat, alpha = alpha)

  expect_equal(result$estimate, estimate, tolerance = 1e-8)
  expect_equal(result$std_error, std_error, tolerance = 1e-8)
  expect_equal(result$df, df, tolerance = 1e-8)
  expect_equal(result$p_value, p_value, tolerance = 1e-8)
  expect_equal(
    result$conf_low,
    estimate - critical_value * std_error,
    tolerance = 1e-8
  )
  expect_equal(
    result$conf_high,
    estimate + critical_value * std_error,
    tolerance = 1e-8
  )
  direct_convergence <- study2_classify_convergence(
    messages = fit@optinfo$conv$lme4$messages,
    optimizer_code = fit@optinfo$conv$opt
  )

  expect_identical(
    result$converged,
    direct_convergence$converged
  )
  expect_equal(
    result$optimizer_warning,
    study1_collapse_messages(
      direct_convergence$optimizer_messages
    )
  )
  expect_identical(
    result$singular,
    lme4::isSingular(fit, tol = 1e-4)
  )
  expect_equal(
    result$estimated_random_intercept_sd,
    unname(fitted_sds["random_intercept_sd"]),
    tolerance = 1e-10
  )
  expect_equal(
    result$estimated_random_slope_sd,
    unname(fitted_sds["random_slope_sd"]),
    tolerance = 1e-10
  )
  expect_equal(result$retained_clusters, nlevels(dat$cluster))
})


test_that("pwr_func_study2 returns Study 1 style outputs", {
  skip_on_cran()
  skip_if_not_installed("pbkrtest")

  result <- suppressWarnings(pwr_func_study2(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.10,
    random_slope_sd = 0.05,
    contamination = "none",
    reps = 2,
    methods = c("rs", "ri", "cr2"),
    seed = 905,
    keep_replicates = TRUE
  ))

  expect_named(result, c("summary", "replicates", "settings"))
  expect_equal(nrow(result$summary), 3)
  expect_equal(nrow(result$replicates), 6)
  expect_setequal(
    result$summary$model,
    c("rs", "ri", "cr2")
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
  expect_true(all(c(
    "realized_mean_slope",
    "realized_random_slope_sd",
    "estimated_random_intercept_sd",
    "estimated_random_slope_sd",
    "optimizer_warning",
    "optimizer_code"
  ) %in% names(result$replicates)))
  expect_equal(result$settings$random_slope_sd, 0.05)
  expect_equal(
    result$settings$random_slope_variance,
    0.05^2
  )
})


test_that("pwr_func_study2 is reproducible", {
  skip_on_cran()
  skip_if_not_installed("pbkrtest")

  result_one <- suppressWarnings(pwr_func_study2(
    n_clusters = 6,
    cluster_size = 20,
    random_slope_sd = 0.10,
    reps = 2,
    methods = c("rs", "ri"),
    seed = 906,
    keep_replicates = TRUE
  ))
  result_two <- suppressWarnings(pwr_func_study2(
    n_clusters = 6,
    cluster_size = 20,
    random_slope_sd = 0.10,
    reps = 2,
    methods = c("rs", "ri"),
    seed = 906,
    keep_replicates = TRUE
  ))

  expect_equal(result_one$settings, result_two$settings)
  expect_equal(
    result_one$replicates[
      setdiff(names(result_one$replicates), "runtime_sec")
    ],
    result_two$replicates[
      setdiff(names(result_two$replicates), "runtime_sec")
    ]
  )
  expect_equal(
    result_one$summary[
      setdiff(names(result_one$summary), "mean_runtime_sec")
    ],
    result_two$summary[
      setdiff(names(result_two$summary), "mean_runtime_sec")
    ]
  )
})


test_that("method order does not change Study 2 numerical results", {
  skip_on_cran()
  skip_if_not_installed("pbkrtest")

  first <- suppressWarnings(pwr_func_study2(
    n_clusters = 8,
    cluster_size = 20,
    random_slope_sd = 0.10,
    reps = 2,
    methods = c("rs", "ri", "cr2"),
    seed = 907,
    keep_replicates = TRUE
  ))
  second <- suppressWarnings(pwr_func_study2(
    n_clusters = 8,
    cluster_size = 20,
    random_slope_sd = 0.10,
    reps = 2,
    methods = c("cr2", "rs", "ri"),
    seed = 907,
    keep_replicates = TRUE
  ))

  comparison_columns <- setdiff(
    names(first$replicates),
    c("runtime_sec", "cluster_diagnostics")
  )

  first_results <- first$replicates[
    order(first$replicates$replicate, first$replicates$method),
    comparison_columns,
    drop = FALSE
  ]
  second_results <- second$replicates[
    order(second$replicates$replicate, second$replicates$method),
    comparison_columns,
    drop = FALSE
  ]

  rownames(first_results) <- NULL
  rownames(second_results) <- NULL

  expect_equal(first_results, second_results)
})


test_that("pwr_func_study2 can run all nine methods", {
  skip_on_cran()
  skip_if_not_installed("pbkrtest")
  skip_if_not_installed("robustlmm")

  result <- suppressWarnings(pwr_func_study2(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.20,
    random_slope_sd = 0.10,
    contamination = "vertical",
    reps = 1,
    seed = 908,
    keep_replicates = TRUE
  ))

  expect_setequal(result$summary$model, study2_method_names())
  expect_equal(nrow(result$replicates), 9)
})


test_that("pwr_func_study2 validates key inputs", {
  expect_error(
    pwr_func_study2(
      n_clusters = 2,
      reps = 1,
      methods = "cr2"
    ),
    "n_clusters"
  )
  expect_error(
    pwr_func_study2(
      random_slope_sd = 0,
      reps = 1,
      methods = "cr2"
    ),
    "random_slope_sd"
  )
  expect_error(
    pwr_func_study2(
      contamination = "bad_leverage",
      reps = 1,
      methods = "cr2"
    ),
    "arg"
  )
  expect_error(
    pwr_func_study2(
      methods = "not_a_method",
      reps = 1
    ),
    "methods"
  )
})


test_that("Study 2 replicate-level results can be omitted", {
  result <- pwr_func_study2(
    n_clusters = 5,
    cluster_size = 20,
    random_slope_sd = 0.05,
    reps = 1,
    methods = "cr2",
    seed = 909,
    keep_replicates = FALSE
  )

  expect_null(result$replicates)
})
