test_that("robust mixed-model helper locks DAStau and RSEn", {
  body_text <- paste(
    deparse(
      body(
        study_fit_robust_mixed
      )
    ),
    collapse = "\n"
  )

  expect_true(
    grepl(
      'method = "DAStau"',
      body_text,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      'setting = "RSEn"',
      body_text,
      fixed = TRUE
    )
  )
  expect_false(
    grepl(
      "RSEa",
      body_text,
      fixed = TRUE
    )
  )
  expect_false(
    grepl(
      "DASvar",
      body_text,
      fixed = TRUE
    )
  )
})


test_that("robust mixed-model helper matches direct robustlmm inference", {
  skip_on_cran()
  skip_if_not_installed(
    "robustlmm"
  )

  if (
    utils::packageVersion(
      "robustlmm"
    ) <
      base::package_version(
        "3.5.0-2"
      )
  ) {
    skip(
      "robustlmm >= 3.5.0-2 is required."
    )
  }

  direct_extract <- function(dat,
                             model,
                             alpha = 0.05) {
    formula <- if (
      identical(model, "ri")
    ) {
      out ~ x + (1 | cluster)
    } else {
      out ~ x + (1 + x || cluster)
    }

    fit <- robustlmm::rlmer(
      formula = formula,
      data = dat,
      method = "DAStau",
      setting = "RSEn"
    )

    fit_summary <- summary(
      fit,
      df = "satterthwaite"
    )
    coefficient_table <- stats::coef(
      fit_summary
    )
    row <- coefficient_table[
      "x",
      ,
      drop = FALSE
    ]

    p_column <- grep(
      "^Pr[(]",
      colnames(row),
      value = TRUE
    )

    estimate <- as.numeric(
      row[1L, "Estimate"]
    )
    std_error <- as.numeric(
      row[1L, "Std. Error"]
    )
    df <- as.numeric(
      row[1L, "df"]
    )
    p_value <- as.numeric(
      row[1L, p_column[1L]]
    )
    critical_value <- stats::qt(
      1 - alpha / 2,
      df = df
    )

    processed <- robustlmm::processFit(
      fit,
      all = FALSE,
      coefs = FALSE,
      stdErrors = FALSE,
      tValues = FALSE,
      sigma = FALSE,
      thetas = FALSE,
      b = FALSE,
      meanB = FALSE,
      meanAbsB = FALSE,
      residuals = FALSE,
      converged = TRUE,
      numWarnings = TRUE,
      procTime = FALSE
    )

    list(
      estimate = estimate,
      std_error = std_error,
      df = df,
      p_value = p_value,
      conf_low = estimate -
        critical_value * std_error,
      conf_high = estimate +
        critical_value * std_error,
      optimizer_code = as.numeric(
        processed$converged[1L]
      ),
      singular =
        study_robust_mixed_boundary(
          fit,
          tol = 1e-4
        )
    )
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

  cases <- list(
    list(
      dat = study1_dat,
      model = "ri"
    ),
    list(
      dat = study2_dat,
      model = "ri"
    ),
    list(
      dat = study2_dat,
      model = "rs"
    )
  )

  for (case in cases) {
    observed <- study_fit_robust_mixed(
      dat = case$dat,
      alpha = 0.05,
      model = case$model
    )

    expected <- direct_extract(
      dat = case$dat,
      model = case$model,
      alpha = 0.05
    )

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
    expect_equal(
      observed$conf_low,
      expected$conf_low,
      tolerance = 1e-8
    )
    expect_equal(
      observed$conf_high,
      expected$conf_high,
      tolerance = 1e-8
    )
    expect_identical(
      observed$optimizer_code,
      expected$optimizer_code
    )
    expect_identical(
      observed$singular,
      expected$singular
    )
  }
})


test_that("robust random-slope boundary remains usable", {
  skip_on_cran()
  skip_if_not_installed(
    "robustlmm"
  )

  if (
    utils::packageVersion(
      "robustlmm"
    ) <
      base::package_version(
        "3.5.0-2"
      )
  ) {
    skip(
      "robustlmm >= 3.5.0-2 is required."
    )
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

  result <- study_fit_robust_mixed(
    dat = dat,
    alpha = 0.05,
    model = "rs"
  )

  expect_true(
    result$singular
  )
  expect_true(
    result$converged
  )
  expect_identical(
    result$optimizer_code,
    0
  )
  expect_true(
    all(
      is.finite(
        c(
          result$estimate,
          result$std_error,
          result$df,
          result$p_value,
          result$conf_low,
          result$conf_high,
          result$retained_clusters
        )
      )
    )
  )
})


test_that("robust mixed-model helper is deterministic for a fixed dataset", {
  skip_on_cran()
  skip_if_not_installed(
    "robustlmm"
  )

  if (
    utils::packageVersion(
      "robustlmm"
    ) <
      base::package_version(
        "3.5.0-2"
      )
  ) {
    skip(
      "robustlmm >= 3.5.0-2 is required."
    )
  }

  set.seed(20269201L)
  dat <- study1_simulate_data(
    n_clusters = 8L,
    cluster_size = 30L,
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

  set.seed(20269202L)
  first <- study_fit_robust_mixed(
    dat = dat,
    alpha = 0.05,
    model = "ri"
  )

  set.seed(20269202L)
  second <- study_fit_robust_mixed(
    dat = dat,
    alpha = 0.05,
    model = "ri"
  )

  fields <- c(
    "estimate",
    "std_error",
    "df",
    "p_value",
    "conf_low",
    "conf_high",
    "converged",
    "singular",
    "retained_clusters",
    "optimizer_code",
    "estimated_random_intercept_sd",
    "estimated_random_slope_sd"
  )

  expect_equal(
    first[fields],
    second[fields],
    tolerance = 0
  )
})
