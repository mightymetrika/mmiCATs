test_that("cluster_data_explore returns structured tables and plots", {
  set.seed(1001)

  dat <- data.frame(
    y = rnorm(60),
    x = rnorm(60),
    id = rep(
      letters[1:6],
      each = 10
    ),
    stringsAsFactors = FALSE
  )

  dat$y[3] <- NA_real_
  dat$x[15] <- NA_real_

  out <- cluster_data_explore(
    y ~ x,
    ~ id,
    dat
  )

  expect_true(
    all(
      c(
        "overall",
        "cluster_summary",
        "missingness",
        "cluster_missingness",
        "analysis_data",
        "plots"
      ) %in% names(out)
    )
  )

  expect_equal(
    out$overall$clusters,
    6
  )

  expect_equal(
    nrow(
      out$cluster_summary
    ),
    6
  )

  expect_equal(
    sum(
      out$missingness$missing_n
    ),
    2
  )

  expect_true(
    all(
      vapply(
        out$plots,
        inherits,
        logical(1),
        what = "ggplot"
      )
    )
  )
})


test_that("cluster_data_explore identifies a non-estimable within-cluster slope", {
  dat <- data.frame(
    y = seq_len(18),
    x = c(
      rep(1, 6),
      seq_len(6),
      seq_len(6)
    ),
    id = rep(
      c("a", "b", "c"),
      each = 6
    )
  )

  out <- cluster_data_explore(
    y ~ x,
    ~ id,
    dat
  )

  expect_false(
    out$cluster_summary$
      estimable_slope[
        out$cluster_summary$
          cluster == "a"
      ]
  )

  expect_equal(
    out$overall$
      clusters_without_estimable_slope,
    1
  )
})


test_that("cluster_model_diagnostics uses the verified fitting path", {
  set.seed(1002)

  id <- factor(
    rep(
      seq_len(6),
      each = 20
    )
  )
  x <- rnorm(120)
  u <- rnorm(6, sd = 0.5)

  dat <- data.frame(
    y = 0.25 * x +
      u[as.integer(id)] +
      rnorm(120),
    x = x,
    id = id
  )

  out <- cluster_model_diagnostics(
    y ~ x,
    ~ id,
    dat,
    methods = c(
      "cr2",
      "cats"
    ),
    seed = 1003L,
    leave_one_cluster_out = FALSE
  )

  expect_equal(
    out$comparison$method,
    c(
      "cr2",
      "cats"
    )
  )

  expect_true(
    all(
      out$comparison$
        fit_success
    )
  )

  expect_true(
    all(
      is.finite(
        out$comparison$estimate
      )
    )
  )

  expect_equal(
    sort(
      unique(
        out$cluster_fits$engine
      )
    ),
    sort(
      c(
        "ols",
        "robust",
        "robustbase"
      )
    )
  )

  expect_equal(
    nrow(
      out$observation_diagnostics
    ),
    nrow(dat)
  )

  expect_equal(
    nrow(out$influence),
    0
  )

  expect_false(
    "recommendation" %in%
      names(out)
  )
})


test_that("leave-one-cluster-out diagnostics retain method and cluster identity", {
  set.seed(1004)

  dat <- data.frame(
    y = rnorm(80),
    x = rnorm(80),
    id = rep(
      seq_len(8),
      each = 10
    )
  )

  out <- cluster_model_diagnostics(
    y ~ x,
    ~ id,
    dat,
    methods = "cr2",
    seed = 1005L,
    leave_one_cluster_out = TRUE
  )

  expect_equal(
    nrow(out$influence),
    8
  )

  expect_true(
    all(
      out$influence$method ==
        "cr2"
    )
  )

  expect_equal(
    length(
      unique(
        out$influence$
          omitted_cluster
      )
    ),
    8
  )
})


test_that("diagnostic specification rejects transformed or multiple-predictor formulas", {
  dat <- data.frame(
    y = rnorm(30),
    x = rnorm(30),
    z = rnorm(30),
    id = rep(1:3, each = 10)
  )

  expect_error(
    cluster_data_explore(
      y ~ log(x),
      ~ id,
      dat
    ),
    "untransformed"
  )

  expect_error(
    cluster_model_diagnostics(
      y ~ x + z,
      ~ id,
      dat,
      methods = "cr2"
    ),
    "untransformed"
  )
})

test_that("cluster_diag_extract_weights recognizes lmRob M.weights", {
  expected <- c(
    1.0,
    0.75,
    0.25,
    0.0
  )

  synthetic_fit <- list(
    M.weights = expected
  )

  observed <- cluster_diag_extract_weights(
    fit = synthetic_fit,
    n = length(expected)
  )

  expect_equal(
    observed,
    expected
  )
})


test_that("cluster_diag_extract_weights returns real lmRob MM weights", {
  set.seed(1006)

  dat <- data.frame(
    y = rnorm(80),
    x = rnorm(80)
  )

  fit <- robust::lmRob(
    y ~ x,
    data = dat
  )

  expected <- fit[[
    "M.weights"
  ]]

  expect_true(
    is.numeric(expected)
  )

  expect_equal(
    length(expected),
    nrow(dat)
  )

  observed <- cluster_diag_extract_weights(
    fit = fit,
    n = nrow(dat)
  )

  expect_equal(
    observed,
    as.numeric(expected)
  )

  expect_true(
    any(
      is.finite(observed)
    )
  )
})


test_that("method-comparison diagnostic plot builds without orientation translation messages", {
  set.seed(1007)

  dat <- data.frame(
    y = rnorm(80),
    x = rnorm(80),
    id = rep(
      seq_len(8),
      each = 10
    )
  )

  out <- cluster_model_diagnostics(
    y ~ x,
    ~ id,
    dat,
    methods = "cr2",
    seed = 1008L,
    leave_one_cluster_out = FALSE
  )

  expect_silent(
    ggplot2::ggplot_build(
      out$plots$method_comparison
    )
  )
})

