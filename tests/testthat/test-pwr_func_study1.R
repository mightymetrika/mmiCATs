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


test_that("pwr_func_study1 can run all seven methods", {
  skip_on_cran()
  skip_if_not_installed("pbkrtest")
  skip_if_not_installed("robustlmm")

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
  expect_equal(nrow(result$replicates), 7)
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




make_study1_validation_data <- function() {
  set.seed(801)

  n_clusters <- 8L
  cluster_size <- 15L
  cluster <- factor(
    rep(seq_len(n_clusters), each = cluster_size),
    levels = seq_len(n_clusters)
  )
  x <- stats::rnorm(n_clusters * cluster_size)
  random_intercept <- rep(
    stats::rnorm(n_clusters, mean = 0, sd = 0.8),
    each = cluster_size
  )
  residual <- stats::rnorm(
    n_clusters * cluster_size,
    mean = 0,
    sd = 0.7
  )

  data.frame(
    cluster = cluster,
    x = x,
    out = 0.4 + 0.3 * x + random_intercept + residual
  )
}


test_that("study1_fit_cr2 matches direct clubSandwich calculations", {
  dat <- make_study1_validation_data()
  alpha <- 0.05

  fit <- stats::lm(out ~ x, data = dat)
  direct_test <- clubSandwich::coef_test(
    fit,
    vcov = "CR2",
    cluster = dat$cluster,
    test = "Satterthwaite",
    coefs = "x"
  )
  direct_ci <- clubSandwich::conf_int(
    fit,
    vcov = "CR2",
    cluster = dat$cluster,
    level = 1 - alpha,
    test = "Satterthwaite",
    coefs = "x"
  )

  result <- study1_fit_cr2(dat = dat, alpha = alpha)

  expect_equal(result$estimate, unname(direct_test$beta[1L]),
               tolerance = 1e-10)
  expect_equal(result$std_error, unname(direct_test$SE[1L]),
               tolerance = 1e-10)
  expect_equal(result$df, unname(direct_test$df_Satt[1L]),
               tolerance = 1e-10)
  expect_equal(result$p_value, unname(direct_test$p_Satt[1L]),
               tolerance = 1e-10)
  expect_equal(result$conf_low, unname(direct_ci$CI_L[1L]),
               tolerance = 1e-10)
  expect_equal(result$conf_high, unname(direct_ci$CI_U[1L]),
               tolerance = 1e-10)
  expect_equal(result$retained_clusters, nlevels(dat$cluster))
})


test_that("study1_fit_ri matches direct Kenward-Roger calculations", {
  skip_on_cran()
  skip_if_not_installed("pbkrtest")

  dat <- make_study1_validation_data()
  alpha <- 0.05

  fit <- lmerTest::lmer(
    out ~ x + (1 | cluster),
    data = dat,
    REML = TRUE
  )
  fit_summary <- summary(fit, ddf = "Kenward-Roger")
  coefficient_row <- stats::coef(fit_summary)["x", , drop = FALSE]
  estimate <- unname(coefficient_row[1L, "Estimate"])
  std_error <- unname(coefficient_row[1L, "Std. Error"])
  df <- unname(coefficient_row[1L, "df"])
  p_value <- unname(coefficient_row[1L, "Pr(>|t|)"])
  critical_value <- stats::qt(1 - alpha / 2, df = df)

  result <- study1_fit_ri(dat = dat, alpha = alpha)

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
  expect_identical(
    result$converged,
    is.null(fit@optinfo$conv$lme4$messages)
  )
  expect_identical(
    result$singular,
    lme4::isSingular(fit, tol = 1e-4)
  )
  expect_equal(result$retained_clusters, nlevels(dat$cluster))
})


test_that("study1_fit_cats matches direct clusterSEs calculations", {
  dat <- make_study1_validation_data()
  alpha <- 0.05

  for (truncate in c(FALSE, TRUE)) {
    fit <- stats::glm(
      out ~ x,
      data = dat,
      family = stats::gaussian()
    )
    direct <- clusterSEs::cluster.im.glm(
      mod = fit,
      dat = dat,
      cluster = ~ cluster,
      ci.level = 1 - alpha,
      report = FALSE,
      drop = TRUE,
      truncate = truncate,
      return.vcv = TRUE
    )

    result <- study1_fit_cats(
      dat = dat,
      alpha = alpha,
      truncate = truncate
    )

    expect_equal(result$estimate, unname(direct$beta.bar["x"]),
                 tolerance = 1e-10)
    expect_equal(result$p_value, unname(direct$p.values["x", 1L]),
                 tolerance = 1e-10)
    expect_equal(result$conf_low, unname(direct$ci["x", 1L]),
                 tolerance = 1e-10)
    expect_equal(result$conf_high, unname(direct$ci["x", 2L]),
                 tolerance = 1e-10)
    expect_equal(
      result$std_error,
      sqrt(unname(direct$vcv.hat["x", "x"]) /
             result$retained_clusters),
      tolerance = 1e-10
    )
    expect_equal(result$df, result$retained_clusters - 1L)
  }
})


test_that("study1_fit_robust_cats matches direct robust CATs calculations", {
  skip_on_cran()

  dat <- make_study1_validation_data()
  alpha <- 0.05
  formula <- out ~ x

  for (engine in c("robust", "robustbase")) {
    set.seed(802)
    robust_fit <- switch(
      engine,
      "robust" = robust::lmRob(formula = formula, data = dat),
      "robustbase" = robustbase::lmrob(formula = formula, data = dat)
    )
    direct <- cluster_im_lmRob(
      robmod = robust_fit,
      formula = formula,
      dat = dat,
      cluster = ~ cluster,
      ci.level = 1 - alpha,
      drop = TRUE,
      return.vcv = TRUE,
      engine = engine
    )

    set.seed(802)
    result <- study1_fit_robust_cats(
      dat = dat,
      alpha = alpha,
      engine = engine
    )

    expect_equal(result$estimate, unname(direct$beta.bar["x"]),
                 tolerance = 1e-8, info = engine)
    expect_equal(result$p_value, unname(direct$p.values["x", 1L]),
                 tolerance = 1e-8, info = engine)
    expect_equal(result$conf_low, unname(direct$ci["x", 1L]),
                 tolerance = 1e-8, info = engine)
    expect_equal(result$conf_high, unname(direct$ci["x", 2L]),
                 tolerance = 1e-8, info = engine)
    expect_equal(
      result$std_error,
      sqrt(unname(direct$vcv.hat["x", "x"]) /
             result$retained_clusters),
      tolerance = 1e-8,
      info = engine
    )
    expect_equal(result$df, result$retained_clusters - 1L,
                 info = engine)
  }
})


test_that("study1_infer_retained_clusters recovers known cluster counts", {
  alpha <- 0.05
  coefficient_variance <- 0.49

  for (retained_clusters in c(3L, 5L, 8L)) {
    half_width <- stats::qt(
      1 - alpha / 2,
      df = retained_clusters - 1L
    ) * sqrt(coefficient_variance / retained_clusters)

    result <- study1_infer_retained_clusters(
      coefficient_variance = coefficient_variance,
      conf_low = -half_width,
      conf_high = half_width,
      alpha = alpha,
      n_clusters = 10L
    )

    expect_equal(result, retained_clusters)
  }
})


test_that("method order does not change Study 1 numerical results", {
  result_one <- pwr_func_study1(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.15,
    contamination = "vertical",
    reps = 2,
    methods = c("cr2", "cats"),
    seed = 803,
    keep_replicates = TRUE
  )
  result_two <- pwr_func_study1(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.15,
    contamination = "vertical",
    reps = 2,
    methods = c("cats", "cr2"),
    seed = 803,
    keep_replicates = TRUE
  )

  replicate_columns <- setdiff(
    names(result_one$replicates),
    "runtime_sec"
  )
  replicates_one <- result_one$replicates[
    order(result_one$replicates$replicate, result_one$replicates$method),
    replicate_columns,
    drop = FALSE
  ]
  replicates_two <- result_two$replicates[
    order(result_two$replicates$replicate, result_two$replicates$method),
    replicate_columns,
    drop = FALSE
  ]
  rownames(replicates_one) <- NULL
  rownames(replicates_two) <- NULL

  summary_columns <- setdiff(
    names(result_one$summary),
    "mean_runtime_sec"
  )
  summary_one <- result_one$summary[
    order(result_one$summary$model),
    summary_columns,
    drop = FALSE
  ]
  summary_two <- result_two$summary[
    order(result_two$summary$model),
    summary_columns,
    drop = FALSE
  ]
  rownames(summary_one) <- NULL
  rownames(summary_two) <- NULL

  expect_equal(replicates_one, replicates_two)
  expect_equal(summary_one, summary_two)
})
