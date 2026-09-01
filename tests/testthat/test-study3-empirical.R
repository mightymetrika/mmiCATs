test_that("Study 3 package API exposes the reproducible empirical workflow", {
  expect_true(
    is.function(
      prepare_study3_empirical
    )
  )

  expect_true(
    is.function(
      run_study3_empirical
    )
  )

  runner_text <- paste(
    deparse(
      body(
        run_study3_empirical
      )
    ),
    collapse = "\n"
  )

  expect_false(
    grepl(
      "source(",
      runner_text,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      "sleepstudy_canonical.rds",
      runner_text,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      "sleepstudy_perturbed.rds",
      runner_text,
      fixed = TRUE
    )
  )

  expect_false(
    grepl(
      "20261105L",
      runner_text,
      fixed = TRUE
    )
  )

  expect_false(
    grepl(
      "sample(",
      runner_text,
      fixed = TRUE
    )
  )
})


test_that("Study 3 package LOO helper reproduces cluster_model_diagnostics", {
  set.seed(20261110L)

  cluster <- factor(
    rep(
      seq_len(6L),
      each = 12L
    )
  )

  x <- stats::rnorm(
    length(cluster)
  )

  u <- stats::rnorm(
    nlevels(cluster),
    sd = 0.4
  )

  synthetic <- data.frame(
    Reaction =
      0.20 * x +
      u[
        as.integer(
          cluster
        )
      ] +
      stats::rnorm(
        length(cluster)
      ),
    Days = x,
    Subject = cluster
  )

  methods <- c(
    "cr2",
    "cats"
  )

  seed <- study3c_analysis_seed()

  direct <- cluster_model_diagnostics(
    Reaction ~ Days,
    ~ Subject,
    synthetic,
    methods = methods,
    alpha = 0.05,
    seed = seed,
    leave_one_cluster_out = TRUE
  )

  full <- study3c_fit_full(
    data = synthetic,
    methods = methods,
    seed = seed
  )

  dat <- study3c_prepare_analysis_data(
    synthetic
  )

  custom <- do.call(
    rbind,
    lapply(
      seq_along(
        levels(
          dat$cluster
        )
      ),
      function(cluster_index) {
        study3c_fit_loo_subject(
          dat = dat,
          methods = methods,
          seed = seed,
          full_comparison =
            full$comparison,
          cluster_index =
            cluster_index
        )
      }
    )
  )

  rownames(custom) <- NULL

  expected <- direct$influence
  rownames(expected) <- NULL

  expect_equal(
    custom,
    expected,
    tolerance = 1e-12
  )
})


test_that("Study 3 freeze verification can preserve artifacts after documented source amendments", {
  body_text <- paste(
    deparse(
      body(
        study3c_verify_freeze
      )
    ),
    collapse = "\n"
  )

  expect_true(
    "verify_original_sources" %in%
      names(
        formals(
          study3c_verify_freeze
        )
      )
  )

  expect_true(
    grepl(
      "study3_frozen_artifact_checksums.csv",
      body_text,
      fixed = TRUE
    )
  )

  expect_true(
    grepl(
      "if (isTRUE(verify_original_sources))",
      body_text,
      fixed = TRUE
    )
  )
})
