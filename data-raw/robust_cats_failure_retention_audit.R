# Robust CATs audit: Phase 2C adversarial failure and retention behavior
#
# Purpose:
#   Verify how the production and public CATs paths handle warnings, fitting
#   errors, omitted coefficients, nonfinite coefficients, dropped clusters,
#   exactly two retained clusters, fewer than two retained clusters, and
#   retained-cluster-count recovery.
#
# Requirements:
#   library(devtools)
#   load_all()
#   source("data-raw/robust_cats_audit_helpers.R")
#
# This script does not modify production source files. It temporarily replaces
# one in-memory binding for controlled tests and restores it immediately.

project_root <- rca_find_project_root()
rca_require_packages()

pkgload::load_all(
  project_root,
  quiet = TRUE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase2c-failure-retention"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

alpha <- 0.05
check_rows <- list()
check_index <- 0L
behavior_rows <- list()
behavior_index <- 0L

add_check <- function(
    category,
    check,
    passed,
    readiness_required,
    issue_id = NA_character_,
    details = NA_character_) {
  check_index <<- check_index + 1L

  check_rows[[check_index]] <<-
    data.frame(
      category = category,
      check = check,
      passed = isTRUE(passed),
      readiness_required =
        isTRUE(readiness_required),
      issue_id = as.character(issue_id),
      details = as.character(details),
      stringsAsFactors = FALSE
    )
}

add_behavior <- function(
    category,
    scenario,
    implementation,
    returned_value,
    returned_error,
    finite_inference,
    retained_clusters = NA_real_,
    notes = NA_character_) {
  behavior_index <<-
    behavior_index + 1L

  behavior_rows[[behavior_index]] <<-
    data.frame(
      category = category,
      scenario = scenario,
      implementation = implementation,
      returned_value = isTRUE(returned_value),
      returned_error = isTRUE(returned_error),
      finite_inference =
        if (is.na(finite_inference)) {
          NA
        } else {
          isTRUE(finite_inference)
        },
      retained_clusters =
        as.numeric(retained_clusters),
      notes = as.character(notes),
      stringsAsFactors = FALSE
    )
}

has_error <- function(captured) {
  rca_has_text(captured$error)
}

all_finite_cats_output <- function(value) {
  if (is.null(value)) {
    return(FALSE)
  }

  numeric_values <- c(
    value$p.values,
    value$ci,
    value$vcv.hat,
    value$beta.bar
  )

  length(numeric_values) > 0L &&
    all(is.finite(numeric_values))
}

make_validation_data <- function(
    n_clusters = 5L,
    cluster_size = 20L,
    seed = 20261201L) {
  set.seed(seed)

  study1_simulate_data(
    n_clusters = n_clusters,
    cluster_size = cluster_size,
    beta = 0.20,
    intercept = 0.30,
    random_intercept_sd = 0.80,
    residual_sd = 0.70,
    x_sd = 1,
    contamination = "none",
    contamination_prop = 0.05,
    contamination_size = 6,
    leverage_size = 4
  )
}

make_synthetic_fit_function <- function(
    scenario = c(
      "ok",
      "warning",
      "error",
      "missing_x",
      "nonfinite_x"
    )) {
  scenario <- match.arg(scenario)

  function(formula, data, engine) {
    if (identical(scenario, "error")) {
      stop(
        "Synthetic cluster fitting error.",
        call. = FALSE
      )
    }

    if (identical(scenario, "warning")) {
      warning(
        "Synthetic cluster fitting warning.",
        call. = FALSE
      )
    }

    fit <- stats::lm(
      formula = formula,
      data = data
    )

    if (identical(scenario, "missing_x")) {
      fit$coefficients <-
        fit$coefficients[
          names(fit$coefficients) != "x"
        ]
    }

    if (identical(scenario, "nonfinite_x")) {
      fit$coefficients["x"] <- Inf
    }

    fit
  }
}

with_temporary_binding <- function(
    name,
    replacement,
    target_function,
    code) {
  target_environment <-
    environment(target_function)

  if (!exists(
    name,
    envir = target_environment,
    inherits = FALSE
  )) {
    stop(
      paste(
        "Binding was not found:",
        name
      ),
      call. = FALSE
    )
  }

  original <- get(
    name,
    envir = target_environment,
    inherits = FALSE
  )
  originally_locked <- bindingIsLocked(
    name,
    target_environment
  )

  if (originally_locked) {
    unlockBinding(
      name,
      target_environment
    )
  }

  assign(
    name,
    replacement,
    envir = target_environment
  )

  if (originally_locked) {
    lockBinding(
      name,
      target_environment
    )
  }

  on.exit({
    if (bindingIsLocked(
      name,
      target_environment
    )) {
      unlockBinding(
        name,
        target_environment
      )
    }

    assign(
      name,
      original,
      envir = target_environment
    )

    if (originally_locked) {
      lockBinding(
        name,
        target_environment
      )
    }
  }, add = TRUE)

  force(code)
}

make_mock_cluster_helper <- function(
    scenarios,
    slopes = c(
      "1" = 0.10,
      "2" = 0.20,
      "3" = 0.30,
      "4" = 0.40,
      "5" = 0.50
    ),
    intercepts = c(
      "1" = -0.20,
      "2" = -0.10,
      "3" = 0.00,
      "4" = 0.10,
      "5" = 0.20
    )) {
  function(
      cluster_id,
      dat,
      formula,
      engine,
      fit_function =
        study1_fit_robust_model) {
    cluster_label <-
      as.character(cluster_id)
    scenario <- unname(
      scenarios[cluster_label]
    )

    if (length(scenario) == 0L ||
        is.na(scenario)) {
      stop(
        paste(
          "No mock scenario was provided for",
          cluster_label
        ),
        call. = FALSE
      )
    }

    retained <- identical(
      scenario,
      "ok"
    ) ||
      identical(
        scenario,
        "warning"
      )

    warning_text <- if (
      identical(scenario, "warning")
    ) {
      "Synthetic aggregate warning."
    } else {
      NA_character_
    }

    error_text <- switch(
      scenario,
      "error" =
        "Synthetic aggregate fitting error.",
      "missing_x" =
        "Cluster-specific fit did not return all required coefficients.",
      "nonfinite_x" =
        "Cluster-specific fit returned non-finite coefficients.",
      NA_character_
    )

    data.frame(
      cluster = cluster_label,
      intercept = if (retained) {
        unname(intercepts[cluster_label])
      } else {
        NA_real_
      },
      x = if (retained) {
        unname(slopes[cluster_label])
      } else {
        NA_real_
      },
      retained = retained,
      warning = warning_text,
      error = error_text,
      stringsAsFactors = FALSE
    )
  }
}

make_rank_deficient_cluster_data <- function() {
  set.seed(20261202L)

  cluster_size <- 16L
  cluster <- factor(
    rep(1:4, each = cluster_size),
    levels = 1:4
  )

  x <- unlist(
    lapply(
      1:4,
      function(cluster_id) {
        if (cluster_id == 1L) {
          rep(0, cluster_size)
        } else {
          seq(
            -1.5,
            1.5,
            length.out = cluster_size
          )
        }
      }
    ),
    use.names = FALSE
  )

  cluster_intercept <- rep(
    c(-0.30, -0.10, 0.10, 0.30),
    each = cluster_size
  )

  data.frame(
    cluster = cluster,
    x = x,
    out = 0.25 +
      cluster_intercept +
      0.40 * x +
      stats::rnorm(
        4L * cluster_size,
        sd = 0.15
      )
  )
}

make_truncation_count_data <- function(
    zero_retained_slope_variance = FALSE) {
  slopes <- if (
    isTRUE(zero_retained_slope_variance)
  ) {
    c(
      10.0,
      0.2,
      0.2,
      0.2,
      0.2
    )
  } else {
    c(
      10.0,
      0.0,
      0.1,
      0.2,
      0.3
    )
  }

  intercepts <- c(
    -0.20,
    -0.10,
    0.00,
    0.10,
    0.20
  )
  x_pattern <- rep(
    c(-2, -1, 0, 1, 2),
    times = 3L
  )

  rows <- lapply(
    seq_along(slopes),
    function(index) {
      data.frame(
        cluster = factor(
          rep(
            index,
            length(x_pattern)
          ),
          levels = seq_along(slopes)
        ),
        x = x_pattern,
        out = intercepts[index] +
          slopes[index] * x_pattern
      )
    }
  )

  dat <- do.call(rbind, rows)
  dat$cluster <- factor(
    as.character(dat$cluster),
    levels = as.character(
      seq_along(slopes)
    )
  )
  rownames(dat) <- NULL
  dat
}

# -------------------------------------------------------------------------
# 1. Cluster-specific robust helper
# -------------------------------------------------------------------------

validation_data <- make_validation_data()

cluster_scenarios <- c(
  "ok",
  "warning",
  "error",
  "missing_x",
  "nonfinite_x"
)

cluster_helper_results <- lapply(
  cluster_scenarios,
  function(scenario) {
    study1_fit_robust_cluster(
      cluster_id = "1",
      dat = validation_data,
      formula = out ~ x,
      engine = "robust",
      fit_function =
        make_synthetic_fit_function(
          scenario
        )
    )
  }
)

names(cluster_helper_results) <-
  cluster_scenarios

cluster_helper_table <- do.call(
  rbind,
  lapply(
    names(cluster_helper_results),
    function(scenario) {
      result <-
        cluster_helper_results[[
          scenario
        ]]

      data.frame(
        scenario = scenario,
        retained = result$retained,
        intercept = result$intercept,
        x = result$x,
        warning = result$warning,
        error = result$error,
        stringsAsFactors = FALSE
      )
    }
  )
)
rownames(cluster_helper_table) <- NULL

add_check(
  category = "Robust cluster helper",
  check =
    "warning_with_finite_coefficients_is_retained",
  passed = (
    cluster_helper_results$warning$
      retained %in% TRUE &&
      rca_has_text(
        cluster_helper_results$warning$
          warning
      ) &&
      is.finite(
        cluster_helper_results$warning$x
      )
  ),
  readiness_required = TRUE,
  details = cluster_helper_results$
    warning$warning
)

add_check(
  category = "Robust cluster helper",
  check = "fitting_error_is_dropped",
  passed = (
    !(cluster_helper_results$error$
        retained %in% TRUE) &&
      rca_has_text(
        cluster_helper_results$error$
          error
      ) &&
      is.na(
        cluster_helper_results$error$x
      )
  ),
  readiness_required = TRUE,
  details = cluster_helper_results$
    error$error
)

add_check(
  category = "Robust cluster helper",
  check = "missing_coefficient_is_dropped",
  passed = (
    !(cluster_helper_results$missing_x$
        retained %in% TRUE) &&
      grepl(
        "required coefficients",
        cluster_helper_results$missing_x$
          error,
        fixed = TRUE
      )
  ),
  readiness_required = TRUE,
  details = cluster_helper_results$
    missing_x$error
)

add_check(
  category = "Robust cluster helper",
  check = "nonfinite_coefficient_is_dropped",
  passed = (
    !(cluster_helper_results$nonfinite_x$
        retained %in% TRUE) &&
      grepl(
        "non-finite",
        cluster_helper_results$nonfinite_x$
          error,
        fixed = TRUE
      )
  ),
  readiness_required = TRUE,
  details = cluster_helper_results$
    nonfinite_x$error
)

# -------------------------------------------------------------------------
# 2. Production robust CATs aggregation with controlled cluster outcomes
# -------------------------------------------------------------------------

run_controlled_production <- function(
    scenarios) {
  mock_helper <-
    make_mock_cluster_helper(
      scenarios = scenarios
    )

  rca_capture(
    with_temporary_binding(
      name =
        "study1_fit_robust_cluster",
      replacement = mock_helper,
      target_function =
        study1_fit_robust_cats,
      code =
        study1_fit_robust_cats(
          dat = validation_data,
          alpha = alpha,
          engine = "robustbase"
        )
    )
  )
}

controlled_scenarios <- list(
  warning_retained = c(
    "1" = "ok",
    "2" = "warning",
    "3" = "ok",
    "4" = "ok",
    "5" = "ok"
  ),
  one_error_dropped = c(
    "1" = "ok",
    "2" = "ok",
    "3" = "error",
    "4" = "ok",
    "5" = "ok"
  ),
  exactly_two_retained = c(
    "1" = "ok",
    "2" = "error",
    "3" = "missing_x",
    "4" = "nonfinite_x",
    "5" = "ok"
  ),
  one_retained = c(
    "1" = "ok",
    "2" = "error",
    "3" = "missing_x",
    "4" = "nonfinite_x",
    "5" = "error"
  ),
  zero_retained = c(
    "1" = "error",
    "2" = "error",
    "3" = "missing_x",
    "4" = "nonfinite_x",
    "5" = "error"
  )
)

controlled_results <- lapply(
  controlled_scenarios,
  run_controlled_production
)

controlled_table <- do.call(
  rbind,
  lapply(
    names(controlled_results),
    function(scenario_name) {
      captured <-
        controlled_results[[
          scenario_name
        ]]
      value <- captured$value

      data.frame(
        scenario = scenario_name,
        returned_value =
          !is.null(value),
        returned_error =
          has_error(captured),
        estimate = if (
          is.null(value)
        ) {
          NA_real_
        } else {
          value$estimate
        },
        std_error = if (
          is.null(value)
        ) {
          NA_real_
        } else {
          value$std_error
        },
        df = if (
          is.null(value)
        ) {
          NA_real_
        } else {
          value$df
        },
        retained_clusters = if (
          is.null(value)
        ) {
          NA_real_
        } else {
          value$retained_clusters
        },
        warning_count = if (
          is.null(value)
        ) {
          NA_real_
        } else {
          value$cluster_warning_count
        },
        error_count = if (
          is.null(value)
        ) {
          NA_real_
        } else {
          value$cluster_error_count
        },
        dropped_count = if (
          is.null(value)
        ) {
          NA_real_
        } else {
          value$dropped_cluster_count
        },
        error = captured$error,
        stringsAsFactors = FALSE
      )
    }
  )
)
rownames(controlled_table) <- NULL

warning_result <-
  controlled_results$
    warning_retained$value

add_check(
  category =
    "Production robust aggregation",
  check =
    "finite_warning_cluster_is_counted",
  passed = (
    !is.null(warning_result) &&
      warning_result$
        retained_clusters == 5L &&
      warning_result$
        cluster_warning_count == 1L &&
      identical(
        warning_result$
          cluster_warning_ids,
        "2"
      ) &&
      warning_result$
        dropped_cluster_count == 0L
  ),
  readiness_required = TRUE,
  details = if (
    is.null(warning_result)
  ) {
    controlled_results$
      warning_retained$error
  } else {
    warning_result$warning
  }
)

one_error_result <-
  controlled_results$
    one_error_dropped$value

add_check(
  category =
    "Production robust aggregation",
  check =
    "one_failed_cluster_is_dropped_and_reported",
  passed = (
    !is.null(one_error_result) &&
      one_error_result$
        retained_clusters == 4L &&
      one_error_result$
        cluster_error_count == 1L &&
      one_error_result$
        dropped_cluster_count == 1L &&
      identical(
        one_error_result$
          dropped_cluster_ids,
        "3"
      )
  ),
  readiness_required = TRUE,
  details = if (
    is.null(one_error_result)
  ) {
    controlled_results$
      one_error_dropped$error
  } else {
    paste(
      "Dropped:",
      one_error_result$
        dropped_cluster_ids
    )
  }
)

two_result <-
  controlled_results$
    exactly_two_retained$value

two_expected_estimate <- 0.30
two_expected_se <- 0.20
two_expected_df <- 1
two_expected_t <-
  two_expected_estimate /
  two_expected_se
two_expected_p <- 2 * stats::pt(
  abs(two_expected_t),
  df = two_expected_df,
  lower.tail = FALSE
)
two_critical <- stats::qt(
  1 - alpha / 2,
  df = two_expected_df
)

add_check(
  category =
    "Production robust aggregation",
  check =
    "exactly_two_retained_clusters_produce_valid_inference",
  passed = (
    !is.null(two_result) &&
      two_result$
        retained_clusters == 2L &&
      two_result$df == 1L &&
      isTRUE(all.equal(
        two_result$estimate,
        two_expected_estimate,
        tolerance = 1e-12
      )) &&
      isTRUE(all.equal(
        two_result$std_error,
        two_expected_se,
        tolerance = 1e-12
      )) &&
      isTRUE(all.equal(
        two_result$p_value,
        two_expected_p,
        tolerance = 1e-12
      )) &&
      isTRUE(all.equal(
        two_result$conf_low,
        two_expected_estimate -
          two_critical *
          two_expected_se,
        tolerance = 1e-12
      )) &&
      isTRUE(all.equal(
        two_result$conf_high,
        two_expected_estimate +
          two_critical *
          two_expected_se,
        tolerance = 1e-12
      ))
  ),
  readiness_required = TRUE,
  details = if (
    is.null(two_result)
  ) {
    controlled_results$
      exactly_two_retained$error
  } else {
    paste(
      "Estimate:",
      format(two_result$estimate),
      "SE:",
      format(two_result$std_error),
      "df:",
      two_result$df
    )
  }
)

add_check(
  category =
    "Production robust aggregation",
  check =
    "one_retained_cluster_is_rejected",
  passed = has_error(
    controlled_results$one_retained
  ) &&
    grepl(
      "Fewer than two",
      controlled_results$
        one_retained$error,
      fixed = TRUE
    ),
  readiness_required = TRUE,
  details = controlled_results$
    one_retained$error
)

add_check(
  category =
    "Production robust aggregation",
  check =
    "zero_retained_clusters_are_rejected",
  passed = has_error(
    controlled_results$zero_retained
  ) &&
    grepl(
      "Fewer than two",
      controlled_results$
        zero_retained$error,
      fixed = TRUE
    ),
  readiness_required = TRUE,
  details = controlled_results$
    zero_retained$error
)

# -------------------------------------------------------------------------
# 3. Public helper fail_drop() behavior
# -------------------------------------------------------------------------

base_fit <- stats::lm(
  out ~ x,
  data = validation_data[
    validation_data$cluster == "1",
    ,
    drop = FALSE
  ]
)

missing_fit <- base_fit
missing_fit$coefficients <-
  missing_fit$coefficients[
    names(missing_fit$coefficients) != "x"
  ]

nonfinite_fit <- base_fit
nonfinite_fit$coefficients["x"] <- Inf

fail_drop_failed_drop <- rca_capture(
  fail_drop(
    drop = TRUE,
    fail = TRUE,
    clust.mod = NULL,
    ind_variables =
      c("(Intercept)", "x")
  )
)

fail_drop_failed_stop <- rca_capture(
  fail_drop(
    drop = FALSE,
    fail = TRUE,
    clust.mod = NULL,
    ind_variables =
      c("(Intercept)", "x")
  )
)

fail_drop_missing <- rca_capture(
  fail_drop(
    drop = TRUE,
    fail = FALSE,
    clust.mod = missing_fit,
    ind_variables =
      c("(Intercept)", "x")
  )
)

fail_drop_nonfinite <- rca_capture(
  fail_drop(
    drop = TRUE,
    fail = FALSE,
    clust.mod = nonfinite_fit,
    ind_variables =
      c("(Intercept)", "x")
  )
)

add_check(
  category = "Public robust helper",
  check =
    "fit_failure_with_drop_true_returns_missing",
  passed = (
    is.na(
      fail_drop_failed_drop$error
    ) &&
      length(
        fail_drop_failed_drop$value
      ) == 1L &&
      is.na(
        fail_drop_failed_drop$value
      )
  ),
  readiness_required = TRUE,
  details = fail_drop_failed_drop$error
)

add_check(
  category = "Public robust helper",
  check =
    "fit_failure_with_drop_false_stops",
  passed = has_error(
    fail_drop_failed_stop
  ),
  readiness_required = TRUE,
  details = fail_drop_failed_stop$error
)

add_check(
  category = "Public robust helper",
  check =
    "omitted_coefficient_with_drop_true_is_dropped",
  passed = (
    is.na(fail_drop_missing$error) &&
      all(is.na(
        fail_drop_missing$value
      ))
  ),
  readiness_required = TRUE,
  issue_id = "A-02",
  details = if (
    has_error(fail_drop_missing)
  ) {
    paste(
      "Current behavior:",
      fail_drop_missing$error
    )
  } else {
    "The omitted-coefficient fit was returned as missing."
  }
)

nonfinite_drop_success <- (
  is.na(fail_drop_nonfinite$error) &&
    all(
      is.na(
        fail_drop_nonfinite$value
      )
    )
)

add_check(
  category = "Public robust helper",
  check =
    "nonfinite_coefficient_with_drop_true_is_dropped",
  passed = nonfinite_drop_success,
  readiness_required = TRUE,
  issue_id = "A-11",
  details = if (
    has_error(fail_drop_nonfinite)
  ) {
    fail_drop_nonfinite$error
  } else {
    paste(
      "Returned coefficients:",
      paste(
        fail_drop_nonfinite$value,
        collapse = ", "
      )
    )
  }
)

# -------------------------------------------------------------------------
# 4. Public process_results() retained-cluster safety
# -------------------------------------------------------------------------

two_cluster_rows <- list(
  c(
    "(Intercept)" = -0.10,
    "x" = 0.10
  ),
  c(
    "(Intercept)" = 0.10,
    "x" = 0.50
  )
)

one_cluster_rows <- list(
  c(
    "(Intercept)" = -0.10,
    "x" = 0.10
  ),
  c(
    "(Intercept)" = NA_real_,
    "x" = NA_real_
  )
)

all_dropped_rows <- list(
  c(
    "(Intercept)" = NA_real_,
    "x" = NA_real_
  ),
  c(
    "(Intercept)" = NA_real_,
    "x" = NA_real_
  )
)

nonfinite_rows <- list(
  c(
    "(Intercept)" = -0.10,
    "x" = 0.10
  ),
  c(
    "(Intercept)" = 0.10,
    "x" = Inf
  ),
  c(
    "(Intercept)" = 0.20,
    "x" = 0.50
  )
)

two_public <- rca_capture(
  process_results(
    results = two_cluster_rows,
    ind_variables =
      c("(Intercept)", "x"),
    ci.level = 0.95,
    drop = TRUE,
    return.vcv = TRUE
  )
)

one_public <- rca_capture(
  process_results(
    results = one_cluster_rows,
    ind_variables =
      c("(Intercept)", "x"),
    ci.level = 0.95,
    drop = TRUE,
    return.vcv = TRUE
  )
)

all_dropped_public <- rca_capture(
  process_results(
    results = all_dropped_rows,
    ind_variables =
      c("(Intercept)", "x"),
    ci.level = 0.95,
    drop = TRUE,
    return.vcv = TRUE
  )
)

nonfinite_public <- rca_capture(
  process_results(
    results = nonfinite_rows,
    ind_variables =
      c("(Intercept)", "x"),
    ci.level = 0.95,
    drop = TRUE,
    return.vcv = TRUE
  )
)

two_diagnostics <- data.frame(
  cluster = c("1", "2"),
  intercept = c(-0.10, 0.10),
  x = c(0.10, 0.50),
  retained_before_truncation =
    c(TRUE, TRUE),
  stringsAsFactors = FALSE
)

two_oracle <- rca_aggregate_coefficients(
  diagnostics = two_diagnostics,
  alpha = alpha,
  focal = "x",
  truncation_rule = "none"
)

two_public_matches <- (
  !is.null(two_public$value) &&
    isTRUE(all.equal(
      unname(
        two_public$value$
          beta.bar["x"]
      ),
      two_oracle$estimate,
      tolerance = 1e-12
    )) &&
    isTRUE(all.equal(
      unname(
        two_public$value$
          p.values["x", 1L]
      ),
      two_oracle$p_value,
      tolerance = 1e-12
    )) &&
    isTRUE(all.equal(
      unname(
        two_public$value$
          ci["x", 1L]
      ),
      two_oracle$conf_low,
      tolerance = 1e-12
    )) &&
    isTRUE(all.equal(
      unname(
        two_public$value$
          ci["x", 2L]
      ),
      two_oracle$conf_high,
      tolerance = 1e-12
    ))
)

add_check(
  category = "Public result processing",
  check =
    "exactly_two_retained_clusters_match_oracle",
  passed = two_public_matches,
  readiness_required = TRUE,
  details = two_public$error
)

add_check(
  category = "Public result processing",
  check =
    "one_retained_cluster_is_rejected",
  passed = has_error(one_public),
  readiness_required = TRUE,
  issue_id = "A-03",
  details = if (
    has_error(one_public)
  ) {
    one_public$error
  } else {
    paste(
      "Function returned output; finite:",
      all_finite_cats_output(
        one_public$value
      )
    )
  }
)

add_check(
  category = "Public result processing",
  check = "all_dropped_clusters_are_rejected",
  passed = has_error(
    all_dropped_public
  ),
  readiness_required = TRUE,
  details = all_dropped_public$error
)

add_check(
  category = "Public result processing",
  check =
    "nonfinite_coefficients_are_rejected",
  passed = has_error(
    nonfinite_public
  ),
  readiness_required = TRUE,
  issue_id = "A-11",
  details = if (
    has_error(nonfinite_public)
  ) {
    nonfinite_public$error
  } else {
    paste(
      "Function returned output; finite:",
      all_finite_cats_output(
        nonfinite_public$value
      )
    )
  }
)

add_behavior(
  category = "Public result processing",
  scenario = "exactly_two_retained",
  implementation = "mmiCATs::process_results",
  returned_value = !is.null(
    two_public$value
  ),
  returned_error = has_error(
    two_public
  ),
  finite_inference =
    all_finite_cats_output(
      two_public$value
    ),
  retained_clusters = 2L,
  notes = two_public$warning
)

add_behavior(
  category = "Public result processing",
  scenario = "one_retained",
  implementation = "mmiCATs::process_results",
  returned_value = !is.null(
    one_public$value
  ),
  returned_error = has_error(
    one_public
  ),
  finite_inference =
    all_finite_cats_output(
      one_public$value
    ),
  retained_clusters = 1L,
  notes = one_public$warning
)

add_behavior(
  category = "Public result processing",
  scenario = "all_dropped",
  implementation = "mmiCATs::process_results",
  returned_value = !is.null(
    all_dropped_public$value
  ),
  returned_error = has_error(
    all_dropped_public
  ),
  finite_inference = NA,
  retained_clusters = 0L,
  notes = all_dropped_public$error
)

add_behavior(
  category = "Public result processing",
  scenario = "nonfinite_coefficient",
  implementation = "mmiCATs::process_results",
  returned_value = !is.null(
    nonfinite_public$value
  ),
  returned_error = has_error(
    nonfinite_public
  ),
  finite_inference =
    all_finite_cats_output(
      nonfinite_public$value
    ),
  retained_clusters = 3L,
  notes = nonfinite_public$warning
)

# -------------------------------------------------------------------------
# 5. Actual omitted-coefficient behavior across implementations
# -------------------------------------------------------------------------

rank_deficient_data <-
  make_rank_deficient_cluster_data()

ordinary_rank_deficient <- rca_capture({
  full_fit <- stats::glm(
    out ~ x,
    data = rank_deficient_data,
    family = stats::gaussian()
  )

  clusterSEs::cluster.im.glm(
    mod = full_fit,
    dat = rank_deficient_data,
    cluster = ~ cluster,
    ci.level = 0.95,
    report = FALSE,
    drop = TRUE,
    truncate = FALSE,
    return.vcv = TRUE
  )
})

add_check(
  category = "Omitted coefficient behavior",
  check =
    "clusterSEs_drop_true_drops_omitted_coefficient_cluster",
  passed = !has_error(
    ordinary_rank_deficient
  ),
  readiness_required = TRUE,
  issue_id = "A-02",
  details = if (
    has_error(
      ordinary_rank_deficient
    )
  ) {
    ordinary_rank_deficient$error
  } else {
    "clusterSEs returned a result."
  }
)

add_behavior(
  category = "Omitted coefficient behavior",
  scenario =
    "one_cluster_has_constant_x",
  implementation =
    "clusterSEs::cluster.im.glm",
  returned_value = !is.null(
    ordinary_rank_deficient$value
  ),
  returned_error = has_error(
    ordinary_rank_deficient
  ),
  finite_inference =
    if (
      is.null(
        ordinary_rank_deficient$value
      )
    ) {
      NA
    } else {
      all_finite_cats_output(
        ordinary_rank_deficient$value
      )
    },
  retained_clusters = NA_real_,
  notes = ordinary_rank_deficient$error
)

actual_robust_behavior <- list()

for (engine in c(
  "robust",
  "robustbase"
)) {
  set.seed(
    if (
      identical(engine, "robust")
    ) {
      20261203L
    } else {
      20261204L
    }
  )

  public_result <- rca_capture({
    full_fit <- switch(
      engine,
      "robust" = robust::lmRob(
        out ~ x,
        data = rank_deficient_data
      ),
      "robustbase" =
        robustbase::lmrob(
          out ~ x,
          data = rank_deficient_data
        )
    )

    cluster_im_lmRob(
      robmod = full_fit,
      formula = out ~ x,
      dat = rank_deficient_data,
      cluster = ~ cluster,
      ci.level = 0.95,
      drop = TRUE,
      return.vcv = TRUE,
      engine = engine
    )
  })

  set.seed(
    if (
      identical(engine, "robust")
    ) {
      20261203L
    } else {
      20261204L
    }
  )

  simulation_result <- rca_capture(
    study1_fit_robust_cats(
      dat = rank_deficient_data,
      alpha = alpha,
      engine = engine
    )
  )

  actual_robust_behavior[[
    engine
  ]] <- list(
    public = public_result,
    simulation = simulation_result
  )

  add_behavior(
    category =
      "Omitted coefficient behavior",
    scenario =
      "one_cluster_has_constant_x",
    implementation = paste0(
      "cluster_im_lmRob:",
      engine
    ),
    returned_value = !is.null(
      public_result$value
    ),
    returned_error = has_error(
      public_result
    ),
    finite_inference =
      if (
        is.null(public_result$value)
      ) {
        NA
      } else {
        all_finite_cats_output(
          public_result$value
        )
      },
    retained_clusters = NA_real_,
    notes = public_result$error
  )

  simulation_value <-
    simulation_result$value

  add_behavior(
    category =
      "Omitted coefficient behavior",
    scenario =
      "one_cluster_has_constant_x",
    implementation = paste0(
      "study1_fit_robust_cats:",
      engine
    ),
    returned_value = !is.null(
      simulation_value
    ),
    returned_error = has_error(
      simulation_result
    ),
    finite_inference =
      if (
        is.null(simulation_value)
      ) {
        NA
      } else {
        all(is.finite(c(
          simulation_value$estimate,
          simulation_value$std_error,
          simulation_value$df,
          simulation_value$p_value,
          simulation_value$conf_low,
          simulation_value$conf_high
        )))
      },
    retained_clusters =
      if (
        is.null(simulation_value)
      ) {
        NA_real_
      } else {
        simulation_value$
          retained_clusters
      },
    notes = simulation_result$error
  )

  add_check(
    category =
      "Omitted coefficient behavior",
    check = paste0(
      engine,
      "_simulation_path_drops_constant_x_cluster"
    ),
    passed = (
      !is.null(simulation_value) &&
        simulation_value$
          retained_clusters == 3L &&
        simulation_value$
          dropped_cluster_count == 1L &&
        identical(
          simulation_value$
            dropped_cluster_ids,
          "1"
        )
    ),
    readiness_required = TRUE,
    details = if (
      is.null(simulation_value)
    ) {
      simulation_result$error
    } else {
      paste(
        "Retained:",
        simulation_value$
          retained_clusters,
        "Dropped:",
        simulation_value$
          dropped_cluster_ids
      )
    }
  )
}

# -------------------------------------------------------------------------
# 6. Retained-cluster inference for ordinary and truncated CATs
# -------------------------------------------------------------------------

nonzero_variance_data <-
  make_truncation_count_data(
    zero_retained_slope_variance =
      FALSE
  )

zero_variance_data <-
  make_truncation_count_data(
    zero_retained_slope_variance =
      TRUE
  )

evaluate_retained_count <- function(
    dat,
    scenario) {
  oracle <- rca_oracle(
    dat = dat,
    engine = "glm",
    alpha = alpha,
    truncation_rule =
      "clusterSEs",
    consume_template = FALSE
  )

  production <- rca_capture(
    study1_fit_cats(
      dat = dat,
      alpha = alpha,
      truncate = TRUE
    )
  )

  value <- production$value

  data.frame(
    scenario = scenario,
    actual_retained_clusters =
      oracle$aggregate$
        retained_clusters,
    inferred_retained_clusters =
      if (
        is.null(value)
      ) {
        NA_real_
      } else {
        value$retained_clusters
      },
    coefficient_variance =
      unname(
        oracle$aggregate$
          vcv_hat["x", "x"]
      ),
    production_returned =
      !is.null(value),
    production_error =
      production$error,
    counts_match =
      !is.null(value) &&
      value$retained_clusters ==
        oracle$aggregate$
          retained_clusters,
    stringsAsFactors = FALSE
  )
}

retained_count_table <- rbind(
  evaluate_retained_count(
    nonzero_variance_data,
    "truncation_nonzero_retained_slope_variance"
  ),
  evaluate_retained_count(
    zero_variance_data,
    "truncation_zero_retained_slope_variance"
  )
)

nonzero_count_row <-
  retained_count_table[
    retained_count_table$scenario ==
      "truncation_nonzero_retained_slope_variance",
    ,
    drop = FALSE
  ]

zero_count_row <-
  retained_count_table[
    retained_count_table$scenario ==
      "truncation_zero_retained_slope_variance",
    ,
    drop = FALSE
  ]

add_check(
  category = "Retained-cluster inference",
  check =
    "nonzero_variance_retained_count_is_recovered",
  passed = (
    nonzero_count_row$counts_match %in%
      TRUE
  ),
  readiness_required = TRUE,
  details = paste(
    "Actual:",
    nonzero_count_row$
      actual_retained_clusters,
    "Inferred:",
    nonzero_count_row$
      inferred_retained_clusters
  )
)

add_check(
  category = "Retained-cluster inference",
  check =
    "zero_variance_retained_count_is_recovered",
  passed = (
    zero_count_row$counts_match %in%
      TRUE
  ),
  readiness_required = TRUE,
  issue_id = "A-04",
  details = paste(
    "Actual:",
    zero_count_row$
      actual_retained_clusters,
    "Inferred:",
    zero_count_row$
      inferred_retained_clusters,
    "Variance:",
    zero_count_row$
      coefficient_variance
  )
)

# Broader exact-formula recovery check for positive coefficient variance.
recovery_rows <- list()
recovery_index <- 0L

for (n_clusters in c(
  5L,
  10L,
  20L,
  40L
)) {
  for (
    retained_clusters in
    seq.int(2L, n_clusters)
  ) {
    for (
      coefficient_variance in
      c(1e-8, 0.01, 0.49, 10)
    ) {
      half_width <- stats::qt(
        1 - alpha / 2,
        df = retained_clusters - 1L
      ) * sqrt(
        coefficient_variance /
          retained_clusters
      )

      inferred <-
        study1_infer_retained_clusters(
          coefficient_variance =
            coefficient_variance,
          conf_low = -half_width,
          conf_high = half_width,
          alpha = alpha,
          n_clusters = n_clusters
        )

      recovery_index <-
        recovery_index + 1L
      recovery_rows[[
        recovery_index
      ]] <- data.frame(
        n_clusters = n_clusters,
        retained_clusters =
          retained_clusters,
        coefficient_variance =
          coefficient_variance,
        inferred_clusters = inferred,
        passed =
          identical(
            inferred,
            as.integer(
              retained_clusters
            )
          ),
        stringsAsFactors = FALSE
      )
    }
  }
}

recovery_table <- do.call(
  rbind,
  recovery_rows
)

add_check(
  category = "Retained-cluster inference",
  check =
    "positive_variance_formula_recovery",
  passed = all(
    recovery_table$passed
  ),
  readiness_required = TRUE,
  details = paste(
    sum(recovery_table$passed),
    "of",
    nrow(recovery_table),
    "cases recovered."
  )
)

# -------------------------------------------------------------------------
# 7. info() vector extraction behavior
# -------------------------------------------------------------------------

set.seed(20261205L)
info_fit <- robustbase::lmrob(
  out ~ x,
  data = validation_data
)

info_result <- info(
  formula = out ~ x,
  cluster = ~ cluster,
  dat = validation_data,
  robmod = info_fit
)

add_check(
  category = "Public model information",
  check =
    "info_returns_all_formula_variables",
  passed = identical(
    info_result$variables,
    c("out", "x")
  ),
  readiness_required = FALSE,
  issue_id = "A-07",
  details = paste(
    "Returned:",
    paste(
      info_result$variables,
      collapse = ", "
    )
  )
)

# -------------------------------------------------------------------------
# 8. Save evidence and issue summary
# -------------------------------------------------------------------------

checks <- rca_bind_rows(
  check_rows
)
behaviors <- rca_bind_rows(
  behavior_rows
)

issue_summary <- data.frame(
  issue_id = c(
    "A-02",
    "A-03",
    "A-04",
    "A-07",
    "A-11"
  ),
  issue = c(
    paste(
      "drop = TRUE does not consistently drop",
      "clusters with omitted coefficients."
    ),
    paste(
      "Public result processing does not explicitly",
      "reject fewer than two retained clusters."
    ),
    paste(
      "Retained-cluster inference is ambiguous when",
      "the cross-cluster focal variance is zero."
    ),
    paste(
      "info() uses ifelse() for a vector-valued",
      "formula-variable result."
    ),
    paste(
      "Public robust helpers do not explicitly reject",
      "nonfinite cluster coefficients."
    )
  ),
  status = vapply(
    c(
      "A-02",
      "A-03",
      "A-04",
      "A-07",
      "A-11"
    ),
    function(issue_id) {
      issue_checks <- checks[
        !is.na(checks$issue_id) &
          checks$issue_id == issue_id,
        ,
        drop = FALSE
      ]

      if (nrow(issue_checks) == 0L) {
        return("not evaluated")
      }

      if (all(issue_checks$passed %in% TRUE)) {
        "not reproduced"
      } else {
        "reproduced or unresolved"
      }
    },
    character(1)
  ),
  stringsAsFactors = FALSE
)

source_files <- c(
  robust_cats_audit_helpers = file.path(
    project_root,
    "data-raw",
    "robust_cats_audit_helpers.R"
  ),
  robust_cats_failure_retention =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_failure_retention_audit.R"
    ),
  pwr_func_study1_helpers = file.path(
    project_root,
    "R",
    "pwr_func_study1_helpers.R"
  ),
  cluster_im_lmRob = file.path(
    project_root,
    "R",
    "cluster_im_lmRob.R"
  ),
  helpers_cimrob = file.path(
    project_root,
    "R",
    "helpers_cimrob.R"
  )
)

source_checksums <- rca_source_checksums(
  source_files
)

results <- list(
  checks = checks,
  behaviors = behaviors,
  cluster_helper_table =
    cluster_helper_table,
  controlled_production_table =
    controlled_table,
  retained_count_table =
    retained_count_table,
  retained_count_recovery =
    recovery_table,
  issue_summary = issue_summary,
  source_checksums =
    source_checksums,
  session_info =
    utils::sessionInfo()
)

rca_write_csv_atomic(
  checks,
  file.path(
    output_dir,
    "robust_cats_failure_retention_checks.csv"
  )
)

rca_write_csv_atomic(
  behaviors,
  file.path(
    output_dir,
    "robust_cats_failure_retention_behaviors.csv"
  )
)

rca_write_csv_atomic(
  cluster_helper_table,
  file.path(
    output_dir,
    "robust_cats_cluster_helper_scenarios.csv"
  )
)

rca_write_csv_atomic(
  controlled_table,
  file.path(
    output_dir,
    "robust_cats_controlled_production_scenarios.csv"
  )
)

rca_write_csv_atomic(
  retained_count_table,
  file.path(
    output_dir,
    "ordinary_cats_retained_count_cases.csv"
  )
)

rca_write_csv_atomic(
  recovery_table,
  file.path(
    output_dir,
    "ordinary_cats_retained_count_recovery.csv"
  )
)

rca_write_csv_atomic(
  issue_summary,
  file.path(
    output_dir,
    "robust_cats_phase2c_issue_summary.csv"
  )
)

rca_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "robust_cats_phase2c_source_checksums.csv"
  )
)

rca_save_rds_atomic(
  results,
  file.path(
    output_dir,
    "robust_cats_phase2c_results.rds"
  )
)

writeLines(
  capture.output(
    utils::sessionInfo()
  ),
  con = file.path(
    output_dir,
    "session_info.txt"
  ),
  useBytes = TRUE
)

readiness_failures <- checks[
  checks$readiness_required %in%
    TRUE &
    !(checks$passed %in% TRUE),
  ,
  drop = FALSE
]

message("")
message(
  "Robust CATs Phase 2C checks:"
)
print(
  checks,
  row.names = FALSE
)

message("")
message(
  "Observed implementation behaviors:"
)
print(
  behaviors,
  row.names = FALSE
)

message("")
message(
  "Issue summary:"
)
print(
  issue_summary,
  row.names = FALSE
)

message("")
message(paste(
  "Readiness checks passed:",
  sum(
    checks$readiness_required %in%
      TRUE &
      checks$passed %in% TRUE
  ),
  "of",
  sum(
    checks$readiness_required %in%
      TRUE
  )
))

message(paste(
  "Results saved to:",
  output_dir
))

if (nrow(readiness_failures) > 0L) {
  stop(
    paste(
      nrow(readiness_failures),
      "production-readiness check(s) failed.",
      "The evidence was saved; review the issue",
      "summary before changing production code."
    ),
    call. = FALSE
  )
}

message("")
message(
  "All Phase 2C production-readiness checks passed."
)
