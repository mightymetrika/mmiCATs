test_that("Study 1 RI treats a finite boundary singularity as nonfatal", {
  skip_on_cran()
  skip_if_not_installed("pbkrtest")

  n_clusters <- 10L
  cluster_size <- 20L
  x_pattern <- seq(-2, 2, length.out = cluster_size)
  residual_pattern <- 0.25 * sin(seq_len(cluster_size))

  dat <- data.frame(
    cluster = factor(
      rep(seq_len(n_clusters), each = cluster_size)
    ),
    x = rep(x_pattern, times = n_clusters)
  )

  dat$out <- 0.10 +
    0.25 * dat$x +
    rep(residual_pattern, times = n_clusters)

  direct_fit <- suppressWarnings(
    lmerTest::lmer(
      out ~ x + (1 | cluster),
      data = dat,
      REML = TRUE
    )
  )

  expect_true(
    lme4::isSingular(
      direct_fit,
      tol = 1e-4
    )
  )

  direct_summary <- summary(
    direct_fit,
    ddf = "Kenward-Roger"
  )
  direct_row <- stats::coef(
    direct_summary
  )["x", , drop = FALSE]

  expected_estimate <- unname(
    direct_row[1L, "Estimate"]
  )
  expected_se <- unname(
    direct_row[1L, "Std. Error"]
  )
  expected_df <- unname(
    direct_row[1L, "df"]
  )
  expected_p <- unname(
    direct_row[1L, "Pr(>|t|)"]
  )
  critical_value <- stats::qt(
    0.975,
    df = expected_df
  )

  result <- suppressWarnings(
    study1_fit_ri(
      dat = dat,
      alpha = 0.05
    )
  )

  expect_true(result$singular)
  expect_true(result$converged)

  expect_equal(
    result$estimate,
    expected_estimate,
    tolerance = 1e-10
  )
  expect_equal(
    result$std_error,
    expected_se,
    tolerance = 1e-10
  )
  expect_equal(
    result$df,
    expected_df,
    tolerance = 1e-10
  )
  expect_equal(
    result$p_value,
    expected_p,
    tolerance = 1e-10
  )
  expect_equal(
    result$conf_low,
    expected_estimate -
      critical_value * expected_se,
    tolerance = 1e-10
  )
  expect_equal(
    result$conf_high,
    expected_estimate +
      critical_value * expected_se,
    tolerance = 1e-10
  )

  replicate_result <- suppressWarnings(
    study1_fit_method(
      dat = dat,
      method = "ri",
      beta = 0.25,
      alpha = 0.05,
      replicate_id = 1L,
      method_seed = 20268101L
    )
  )

  expect_true(replicate_result$fit_success)
  expect_true(replicate_result$converged)
  expect_true(replicate_result$singular)
  expect_true(is.finite(replicate_result$estimate))
  expect_false(is.na(replicate_result$reject))
  expect_false(is.na(replicate_result$cover))
})


test_that("Study 1 RI uses the Study 2 convergence classifier", {
  body_text <- paste(
    deparse(body(study1_fit_ri)),
    collapse = "\n"
  )

  expect_true(
    grepl(
      "study2_classify_convergence",
      body_text,
      fixed = TRUE
    )
  )

  singular_only <- study2_classify_convergence(
    messages = paste(
      "boundary (singular) fit:",
      "see help('isSingular')"
    ),
    optimizer_code = 0L
  )

  optimizer_failure <- study2_classify_convergence(
    messages = "Model failed to converge with max|grad| = 0.01",
    optimizer_code = 0L
  )

  nonzero_code <- study2_classify_convergence(
    messages = NULL,
    optimizer_code = 1L
  )

  expect_true(singular_only$converged)
  expect_false(optimizer_failure$converged)
  expect_false(nonzero_code$converged)
})
