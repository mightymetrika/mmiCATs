test_that("Study 1 robust CATs matches the existing robust CATs calculation", {
  skip_on_cran()

  dat <- study1_simulate_data(
    n_clusters = 6,
    cluster_size = 30,
    beta = 0.10,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = "none",
    contamination_prop = 0.05,
    contamination_size = 6,
    leverage_size = 4
  )

  for (engine in c("robust", "robustbase")) {
    set.seed(301)

    full_fit <- switch(
      engine,
      "robust" = robust::lmRob(out ~ x, data = dat),
      "robustbase" = robustbase::lmrob(out ~ x, data = dat)
    )

    expected <- cluster_im_lmRob(
      robmod = full_fit,
      formula = out ~ x,
      dat = dat,
      cluster = ~ cluster,
      ci.level = 0.95,
      drop = TRUE,
      return.vcv = TRUE,
      engine = engine
    )

    set.seed(301)

    observed <- study1_fit_robust_cats(
      dat = dat,
      alpha = 0.05,
      engine = engine
    )

    expect_equal(
      observed$estimate,
      unname(expected$beta.bar["x"]),
      tolerance = 1e-10
    )
    expect_equal(
      observed$p_value,
      unname(expected$p.values["x", 1L]),
      tolerance = 1e-10
    )
    expect_equal(
      observed$conf_low,
      unname(expected$ci["x", 1L]),
      tolerance = 1e-10
    )
    expect_equal(
      observed$conf_high,
      unname(expected$ci["x", 2L]),
      tolerance = 1e-10
    )
    expect_equal(observed$retained_clusters, 6L)
    expect_equal(nrow(observed$cluster_diagnostics), 6L)
    expect_true(is.na(observed$template_error))
  }
})


test_that("template failure does not block viable cluster-specific robust CATs", {
  dat <- study1_simulate_data(
    n_clusters = 6,
    cluster_size = 20,
    beta = 0.10,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = "none",
    contamination_prop = 0.05,
    contamination_size = 6,
    leverage_size = 4
  )

  template_failure_fit <- function(formula, data, engine) {
    if (length(unique(as.character(data$cluster))) > 1L) {
      stop("Synthetic template-only failure.", call. = FALSE)
    }

    stats::lm(formula = formula, data = data)
  }

  for (engine in c("robust", "robustbase")) {
    observed <- study1_fit_robust_cats(
      dat = dat,
      alpha = 0.05,
      engine = engine,
      fit_function = template_failure_fit
    )

    expect_true(is.finite(observed$estimate), info = engine)
    expect_true(is.finite(observed$p_value), info = engine)
    expect_equal(observed$retained_clusters, 6L, info = engine)
    expect_match(
      observed$template_error,
      "Synthetic template-only failure",
      fixed = TRUE
    )
    expect_equal(observed$cluster_error_count, 0L, info = engine)
  }
})


test_that("cluster-specific robust warnings are retained without dropping fits", {
  dat <- study1_simulate_data(
    n_clusters = 3,
    cluster_size = 20,
    beta = 0,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = "none",
    contamination_prop = 0.05,
    contamination_size = 6,
    leverage_size = 4
  )

  warning_fit <- function(formula, data, engine) {
    warning("Synthetic cluster warning.")
    stats::lm(formula = formula, data = data)
  }

  result <- study1_fit_robust_cluster(
    cluster_id = "1",
    dat = dat,
    formula = out ~ x,
    engine = "robust",
    fit_function = warning_fit
  )

  expect_true(result$retained)
  expect_match(result$warning, "Synthetic cluster warning")
  expect_true(is.na(result$error))
  expect_true(is.finite(result$x))
})


test_that("cluster-specific robust errors are recorded and dropped", {
  dat <- study1_simulate_data(
    n_clusters = 3,
    cluster_size = 20,
    beta = 0,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = "none",
    contamination_prop = 0.05,
    contamination_size = 6,
    leverage_size = 4
  )

  error_fit <- function(formula, data, engine) {
    stop("Synthetic cluster error.", call. = FALSE)
  }

  result <- study1_fit_robust_cluster(
    cluster_id = "2",
    dat = dat,
    formula = out ~ x,
    engine = "robustbase",
    fit_function = error_fit
  )

  expect_false(result$retained)
  expect_match(result$error, "Synthetic cluster error")
  expect_true(is.na(result$x))
})


test_that("robust CATs replicate results retain cluster diagnostics", {
  skip_on_cran()

  result <- suppressWarnings(pwr_func_study1(
    n_clusters = 5,
    cluster_size = 20,
    beta = 0,
    contamination = "vertical",
    contamination_size = 6,
    reps = 1,
    methods = "cats_robustbase",
    seed = 302,
    keep_replicates = TRUE
  ))

  expected_columns <- c(
    "template_warning",
    "template_error",
    "cluster_warning_count",
    "cluster_error_count",
    "dropped_cluster_count",
    "cluster_warning_ids",
    "cluster_error_ids",
    "dropped_cluster_ids",
    "cluster_diagnostics"
  )

  expect_true(all(expected_columns %in% names(result$replicates)))
  expect_true(is.list(result$replicates$cluster_diagnostics))

  diagnostics <- result$replicates$cluster_diagnostics[[1L]]

  expect_equal(nrow(diagnostics), 5L)
  expect_named(
    diagnostics,
    c("cluster", "intercept", "x", "retained", "warning", "error")
  )
  expect_equal(
    sum(diagnostics$retained),
    result$replicates$retained_clusters
  )
})
