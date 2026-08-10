test_that("info returns all formula variables for robust linear models", {
  skip_on_cran()

  dat <- data.frame(
    cluster = factor(rep(1:3, each = 8)),
    x = seq(-2, 2, length.out = 24)
  )
  dat$out <- 0.25 + 0.40 * dat$x + rep(c(-0.2, 0, 0.2), each = 8)

  fit <- robustbase::lmrob(
    out ~ x,
    data = dat
  )

  observed <- info(
    formula = out ~ x,
    cluster = ~ cluster,
    dat = dat,
    robmod = fit
  )

  expect_identical(observed$variables, c("out", "x"))
})


test_that("fail_drop returns a named missing vector for unusable cluster fits", {
  dat <- data.frame(
    out = c(1, 2, 3, 4, 5, 6),
    x = c(0, 1, 2, 3, 4, 5)
  )
  fit <- stats::lm(out ~ x, data = dat)
  required <- c("(Intercept)", "x")
  expected_missing <- stats::setNames(c(NA_real_, NA_real_), required)

  expect_identical(
    fail_drop(
      drop = TRUE,
      fail = TRUE,
      clust.mod = NULL,
      ind_variables = required
    ),
    expected_missing
  )

  missing_fit <- fit
  missing_fit$coefficients <- missing_fit$coefficients["(Intercept)"]

  expect_identical(
    fail_drop(
      drop = TRUE,
      fail = FALSE,
      clust.mod = missing_fit,
      ind_variables = required
    ),
    expected_missing
  )

  rank_deficient_fit <- stats::lm(
    out ~ x,
    data = data.frame(
      out = 1:6,
      x = rep(1, 6)
    )
  )

  expect_identical(
    fail_drop(
      drop = TRUE,
      fail = FALSE,
      clust.mod = rank_deficient_fit,
      ind_variables = required
    ),
    expected_missing
  )

  nonfinite_fit <- fit
  nonfinite_fit$coefficients["x"] <- Inf

  expect_identical(
    fail_drop(
      drop = TRUE,
      fail = FALSE,
      clust.mod = nonfinite_fit,
      ind_variables = required
    ),
    expected_missing
  )
})


test_that("fail_drop preserves explicit errors when drop is FALSE", {
  dat <- data.frame(
    out = c(1, 2, 3, 4, 5, 6),
    x = c(0, 1, 2, 3, 4, 5)
  )
  fit <- stats::lm(out ~ x, data = dat)
  required <- c("(Intercept)", "x")

  expect_error(
    fail_drop(
      drop = FALSE,
      fail = TRUE,
      clust.mod = NULL,
      ind_variables = required
    ),
    "returned error",
    fixed = TRUE
  )

  rank_deficient_fit <- stats::lm(
    out ~ x,
    data = data.frame(
      out = 1:6,
      x = rep(1, 6)
    )
  )

  expect_error(
    fail_drop(
      drop = FALSE,
      fail = FALSE,
      clust.mod = rank_deficient_fit,
      ind_variables = required
    ),
    "dropped variables",
    fixed = TRUE
  )

  nonfinite_fit <- fit
  nonfinite_fit$coefficients["x"] <- Inf

  expect_error(
    fail_drop(
      drop = FALSE,
      fail = FALSE,
      clust.mod = nonfinite_fit,
      ind_variables = required
    ),
    "non-finite",
    fixed = TRUE
  )
})


test_that("process_results drops missing rows and requires two retained clusters", {
  required <- c("(Intercept)", "x")
  valid_one <- c("(Intercept)" = -0.10, "x" = 0.10)
  valid_two <- c("(Intercept)" = 0.10, "x" = 0.50)
  missing <- c("(Intercept)" = NA_real_, "x" = NA_real_)

  observed <- process_results(
    results = list(valid_one, missing, valid_two),
    ind_variables = required,
    ci.level = 0.95,
    drop = TRUE,
    return.vcv = TRUE
  )

  expected <- process_results(
    results = list(valid_one, valid_two),
    ind_variables = required,
    ci.level = 0.95,
    drop = TRUE,
    return.vcv = TRUE
  )

  expect_equal(observed, expected, tolerance = 1e-12)

  expect_error(
    process_results(
      results = list(valid_one, missing),
      ind_variables = required,
      ci.level = 0.95,
      drop = TRUE,
      return.vcv = TRUE
    ),
    "Fewer than two",
    fixed = TRUE
  )

  expect_error(
    process_results(
      results = list(valid_one, missing, valid_two),
      ind_variables = required,
      ci.level = 0.95,
      drop = FALSE,
      return.vcv = TRUE
    ),
    "missing coefficients",
    fixed = TRUE
  )
})


test_that("process_results rejects nonfinite coefficients defensively", {
  required <- c("(Intercept)", "x")
  valid_one <- c("(Intercept)" = -0.10, "x" = 0.10)
  valid_two <- c("(Intercept)" = 0.10, "x" = 0.50)
  nonfinite <- c("(Intercept)" = 0.20, "x" = Inf)

  expect_error(
    process_results(
      results = list(valid_one, nonfinite, valid_two),
      ind_variables = required,
      ci.level = 0.95,
      drop = TRUE,
      return.vcv = TRUE
    ),
    "non-finite",
    fixed = TRUE
  )
})


test_that("zero coefficient variance does not imply a retained cluster count", {
  expect_error(
    study1_infer_retained_clusters(
      coefficient_variance = 0,
      conf_low = 0.20,
      conf_high = 0.20,
      alpha = 0.05,
      n_clusters = 10L
    ),
    "cannot be inferred",
    fixed = TRUE
  )
})
