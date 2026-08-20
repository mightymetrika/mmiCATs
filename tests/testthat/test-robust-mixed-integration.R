test_that("robust mixed methods append to canonical method schedules", {
  expect_identical(
    study1_method_names(),
    c(
      "ri",
      "cr2",
      "cats",
      "cats_trunc",
      "cats_robust",
      "cats_robustbase",
      "robust_ri"
    )
  )

  expect_identical(
    study2_method_names(),
    c(
      "rs",
      "ri",
      "cr2",
      "cats",
      "cats_trunc",
      "cats_robust",
      "cats_robustbase",
      "robust_ri",
      "robust_rs"
    )
  )

  replicate_seed <- 20269401L

  study1_existing <- c(
    "ri",
    "cr2",
    "cats",
    "cats_trunc",
    "cats_robust",
    "cats_robustbase"
  )
  study2_existing <- c(
    "rs",
    "ri",
    "cr2",
    "cats",
    "cats_trunc",
    "cats_robust",
    "cats_robustbase"
  )

  study1_observed <- vapply(
    study1_existing,
    function(method) {
      study1_method_seed(
        replicate_seed = replicate_seed,
        method_index = match(
          method,
          study1_method_names()
        )
      )
    },
    integer(1)
  )

  study1_expected <- vapply(
    seq_along(study1_existing),
    function(index) {
      study1_method_seed(
        replicate_seed = replicate_seed,
        method_index = index
      )
    },
    integer(1)
  )

  study2_observed <- vapply(
    study2_existing,
    function(method) {
      study2_method_seed(
        replicate_seed = replicate_seed,
        method_index = match(
          method,
          study2_method_names()
        )
      )
    },
    integer(1)
  )

  study2_expected <- vapply(
    seq_along(study2_existing),
    function(index) {
      study2_method_seed(
        replicate_seed = replicate_seed,
        method_index = index
      )
    },
    integer(1)
  )

  expect_identical(
    unname(study1_observed),
    unname(study1_expected)
  )
  expect_identical(
    unname(study2_observed),
    unname(study2_expected)
  )
})


test_that("robust mixed dispatch matches the production helper", {
  skip_on_cran()
  skip_if_not_installed("robustlmm")

  if (
    utils::packageVersion("robustlmm") <
      base::package_version("3.5.0-2")
  ) {
    skip("robustlmm >= 3.5.0-2 is required.")
  }

  set.seed(20269001L)
  study1_dat <- study1_simulate_data(
    n_clusters = 10L,
    cluster_size = 40L,
    beta = 0.10,
    intercept = 0,
    random_intercept_sd = 1,
    residual_sd = 1,
    x_sd = 1,
    contamination = "vertical",
    contamination_prop = 0.05,
    contamination_size = 6,
    leverage_size = 4
  )

  method_seed <- 20269411L

  set.seed(method_seed)
  expected_study1 <- study_fit_robust_mixed(
    dat = study1_dat,
    alpha = 0.05,
    model = "ri"
  )

  observed_study1 <- study1_fit_method(
    dat = study1_dat,
    method = "robust_ri",
    beta = 0.10,
    alpha = 0.05,
    replicate_id = 1L,
    method_seed = method_seed
  )

  expect_true(observed_study1$fit_success)
  expect_equal(
    observed_study1$estimate,
    expected_study1$estimate,
    tolerance = 1e-8
  )
  expect_equal(
    observed_study1$std_error,
    expected_study1$std_error,
    tolerance = 1e-8
  )
  expect_equal(
    observed_study1$df,
    expected_study1$df,
    tolerance = 1e-3
  )
  expect_equal(
    observed_study1$p_value,
    expected_study1$p_value,
    tolerance = 1e-8
  )
  expect_identical(
    observed_study1$singular,
    expected_study1$singular
  )
  expect_identical(
    observed_study1$optimizer_code,
    expected_study1$optimizer_code
  )

  set.seed(20269002L)
  study2_dat <- study2_simulate_data(
    n_clusters = 10L,
    cluster_size = 40L,
    beta = 0.10,
    intercept = 0,
    random_intercept_sd = 1,
    random_slope_sd = 0.05,
    residual_sd = 1,
    x_sd = 1,
    contamination = "vertical",
    contamination_prop = 0.05,
    contamination_size = 6
  )

  realized_mean_slope <- mean(
    study2_dat$true_cluster_slope[
      !duplicated(study2_dat$cluster)
    ]
  )
  realized_random_slope_sd <- stats::sd(
    study2_dat$random_slope[
      !duplicated(study2_dat$cluster)
    ]
  )

  for (method in c("robust_ri", "robust_rs")) {
    model <- if (
      identical(method, "robust_ri")
    ) {
      "ri"
    } else {
      "rs"
    }

    method_seed <- if (
      identical(method, "robust_ri")
    ) {
      20269412L
    } else {
      20269413L
    }

    set.seed(method_seed)
    expected <- study_fit_robust_mixed(
      dat = study2_dat,
      alpha = 0.05,
      model = model
    )

    observed <- study2_fit_method(
      dat = study2_dat,
      method = method,
      beta = 0.10,
      alpha = 0.05,
      replicate_id = 1L,
      method_seed = method_seed,
      realized_mean_slope = realized_mean_slope,
      realized_random_slope_sd =
        realized_random_slope_sd
    )

    expect_true(observed$fit_success)
    expect_equal(
      observed$estimate,
      expected$estimate,
      tolerance = 1e-8
    )
    expect_equal(
      observed$std_error,
      expected$std_error,
      tolerance = 1e-8
    )
    expect_equal(
      observed$df,
      expected$df,
      tolerance = 1e-3
    )
    expect_equal(
      observed$p_value,
      expected$p_value,
      tolerance = 1e-8
    )
    expect_identical(
      observed$singular,
      expected$singular
    )
    expect_identical(
      observed$optimizer_code,
      expected$optimizer_code
    )
  }
})


test_that("known robust random-slope boundary remains usable through dispatch", {
  skip_on_cran()
  skip_if_not_installed("robustlmm")

  if (
    utils::packageVersion("robustlmm") <
      base::package_version("3.5.0-2")
  ) {
    skip("robustlmm >= 3.5.0-2 is required.")
  }

  set.seed(20269002L)
  dat <- study2_simulate_data(
    n_clusters = 10L,
    cluster_size = 40L,
    beta = 0.10,
    intercept = 0,
    random_intercept_sd = 1,
    random_slope_sd = 0.05,
    residual_sd = 1,
    x_sd = 1,
    contamination = "vertical",
    contamination_prop = 0.05,
    contamination_size = 6
  )

  realized_mean_slope <- mean(
    dat$true_cluster_slope[
      !duplicated(dat$cluster)
    ]
  )
  realized_random_slope_sd <- stats::sd(
    dat$random_slope[
      !duplicated(dat$cluster)
    ]
  )

  result <- study2_fit_method(
    dat = dat,
    method = "robust_rs",
    beta = 0.10,
    alpha = 0.05,
    replicate_id = 1L,
    method_seed = 20269413L,
    realized_mean_slope =
      realized_mean_slope,
    realized_random_slope_sd =
      realized_random_slope_sd
  )

  expect_true(result$fit_success)
  expect_true(result$converged)
  expect_true(result$singular)
  expect_identical(
    result$optimizer_code,
    0
  )
  expect_true(
    is.finite(
      result$estimated_random_intercept_sd
    )
  )
  expect_true(
    is.finite(
      result$estimated_random_slope_sd
    )
  )
})
