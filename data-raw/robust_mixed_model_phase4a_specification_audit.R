# Robust mixed-model Phase 4A specification-lock audit
#
# Purpose:
#   Lock and verify the robustlmm comparator specification before any
#   production integration into pwr_func_study1() or pwr_func_study2().
#
# This audit changes no production files under R/. It uses the experimental
# data-raw helper, which must explicitly request:
#   method = "DAStau"
#   setting = "RSEn"
#
# Frozen comparator specification:
#   Study 1 robust RI: out ~ x + (1 | cluster)
#   Study 2 robust RI: out ~ x + (1 | cluster)
#   Study 2 robust RS: out ~ x + (1 + x || cluster)
#   inference: robustlmm Satterthwaite df, default covariance
#   CI: Wald t interval using the same Satterthwaite df
#   convergence: robustlmm::processFit() code 0
#   boundary: diagonal theta < 1e-4, retained as a diagnostic and nonfatal
#   rescue/fallback: none
#
# Run from the mmiCATs project root.

phase4a_find_project_root <- function(path = getwd()) {
  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  repeat {
    if (file.exists(
      file.path(path, "DESCRIPTION")
    )) {
      return(path)
    }

    parent <- dirname(path)

    if (identical(parent, path)) {
      stop(
        "Could not locate the mmiCATs project root.",
        call. = FALSE
      )
    }

    path <- parent
  }
}

project_root <- phase4a_find_project_root()

helper_path <- file.path(
  project_root,
  "data-raw",
  "robust_mixed_model_pilot_helpers.R"
)

if (!file.exists(helper_path)) {
  stop(
    paste(
      "Could not find:",
      helper_path
    ),
    call. = FALSE
  )
}

source(helper_path)

rmm_require_packages()

pkgload::load_all(
  project_root,
  quiet = TRUE,
  export_all = TRUE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-mixed-model-results",
  "phase4a-specification-lock"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

checks <- list()

add_check <- function(category,
                      check,
                      passed,
                      required = TRUE,
                      details = NA_character_) {
  checks[[length(checks) + 1L]] <<-
    data.frame(
      category = category,
      check = check,
      passed = as.logical(passed),
      required = as.logical(required),
      details = details,
      stringsAsFactors = FALSE
    )
}

collapse_deparse <- function(x) {
  paste(
    deparse(x),
    collapse = " "
  )
}

body_text <- paste(
  deparse(
    body(
      rmm_fit_and_extract
    )
  ),
  collapse = "\n"
)

rlmer_call_count <- lengths(
  regmatches(
    body_text,
    gregexpr(
      "robustlmm::rlmer",
      body_text,
      fixed = TRUE
    )
  )
)

if (identical(
  rlmer_call_count,
  1L
)) {
  rlmer_call_count <- 1L
} else {
  rlmer_call_count <- as.integer(
    rlmer_call_count
  )
}

add_check(
  "Specification",
  "robustlmm_minimum_version_available",
  utils::packageVersion("robustlmm") >=
    base::package_version("3.5.0-2"),
  details = paste(
    "Installed robustlmm:",
    as.character(
      utils::packageVersion(
        "robustlmm"
      )
    )
  )
)

add_check(
  "Specification",
  "prototype_has_one_rlmer_call",
  identical(
    as.integer(rlmer_call_count),
    1L
  ),
  details = paste(
    "rlmer calls:",
    rlmer_call_count
  )
)

add_check(
  "Specification",
  "dastau_is_explicit",
  grepl(
    'method = "DAStau"',
    body_text,
    fixed = TRUE
  )
)

add_check(
  "Specification",
  "rsen_is_explicit",
  grepl(
    'setting = "RSEn"',
    body_text,
    fixed = TRUE
  )
)

add_check(
  "Specification",
  "no_rsea_or_dasvar_rescue_in_candidate_helper",
  !grepl(
    "RSEa",
    body_text,
    fixed = TRUE
  ) &&
    !grepl(
      "DASvar",
      body_text,
      fixed = TRUE
    )
)

expected_formulas <- c(
  study1_robust_ri =
    "out ~ x + (1 | cluster)",
  study2_robust_ri =
    "out ~ x + (1 | cluster)",
  study2_robust_rs =
    "out ~ x + (1 + x || cluster)"
)

observed_formulas <- vapply(
  names(expected_formulas),
  function(model) {
    collapse_deparse(
      rmm_model_formula(model)
    )
  },
  character(1)
)

formula_match <- observed_formulas ==
  expected_formulas

add_check(
  "Specification",
  "model_formulas_are_frozen",
  all(formula_match),
  details = paste(
    names(formula_match),
    formula_match,
    sep = "=",
    collapse = "; "
  )
)

# -------------------------------------------------------------------------
# Direct numerical comparisons against robustlmm itself.
# -------------------------------------------------------------------------

alpha <- 0.05

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
  study1_robust_ri = list(
    dat = study1_dat,
    beta = 0.10
  ),
  study2_robust_ri = list(
    dat = study2_dat,
    beta = 0.10
  ),
  study2_robust_rs = list(
    dat = study2_dat,
    beta = 0.10
  )
)

direct_rows <- list()
direct_index <- 0L
status_rows <- list()

for (model in names(cases)) {
  dat <- cases[[model]]$dat
  beta <- cases[[model]]$beta
  formula <- rmm_model_formula(model)

  helper <- rmm_fit_and_extract(
    dat = dat,
    model = model,
    beta = beta,
    alpha = alpha,
    return_fit = FALSE
  )

  direct_fit_capture <- rmm_capture(
    robustlmm::rlmer(
      formula = formula,
      data = dat,
      method = "DAStau",
      setting = "RSEn"
    )
  )

  if (!is.na(direct_fit_capture$error)) {
    stop(
      paste(
        "Direct robustlmm fit failed for",
        model,
        ":",
        direct_fit_capture$error
      ),
      call. = FALSE
    )
  }

  direct_fit <- direct_fit_capture$value

  direct_summary_capture <- rmm_capture(
    summary(
      direct_fit,
      df = "satterthwaite"
    )
  )

  if (!is.na(direct_summary_capture$error)) {
    stop(
      paste(
        "Direct robustlmm summary failed for",
        model,
        ":",
        direct_summary_capture$error
      ),
      call. = FALSE
    )
  }

  coefficient_table <- stats::coef(
    direct_summary_capture$value
  )

  required_columns <- c(
    "Estimate",
    "Std. Error",
    "df",
    "t value"
  )

  missing_columns <- setdiff(
    required_columns,
    colnames(coefficient_table)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste(
        "Direct robustlmm summary is missing:",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  p_columns <- grep(
    "^Pr[(]",
    colnames(coefficient_table),
    value = TRUE
  )

  if (length(p_columns) == 0L) {
    stop(
      paste(
        "Direct robustlmm Satterthwaite summary",
        "did not return a p-value column for",
        model
      ),
      call. = FALSE
    )
  }

  if (!("x" %in%
        rownames(coefficient_table))) {
    stop(
      paste(
        "Coefficient x is absent for",
        model
      ),
      call. = FALSE
    )
  }

  coefficient_row <- coefficient_table[
    "x",
    ,
    drop = FALSE
  ]

  direct_values <- c(
    estimate = as.numeric(
      coefficient_row[
        1L,
        "Estimate"
      ]
    ),
    std_error = as.numeric(
      coefficient_row[
        1L,
        "Std. Error"
      ]
    ),
    df = as.numeric(
      coefficient_row[
        1L,
        "df"
      ]
    ),
    statistic = as.numeric(
      coefficient_row[
        1L,
        "t value"
      ]
    ),
    p_value = as.numeric(
      coefficient_row[
        1L,
        p_columns[1L]
      ]
    )
  )

  ci_capture <- rmm_capture(
    stats::confint(
      direct_fit,
      parm = "x",
      level = 1 - alpha,
      method = "Wald",
      vcov_type = "default",
      df = "satterthwaite"
    )
  )

  if (!is.na(ci_capture$error)) {
    stop(
      paste(
        "Direct robustlmm Satterthwaite CI failed for",
        model,
        ":",
        ci_capture$error
      ),
      call. = FALSE
    )
  }

  direct_ci <- as.numeric(
    ci_capture$value[
      "x",
      ,
      drop = TRUE
    ]
  )

  process <- robustlmm::processFit(
    direct_fit,
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

  direct_convergence_code <- as.numeric(
    process$converged[1L]
  )

  theta <- as.numeric(
    robustlmm::getME(
      direct_fit,
      "theta"
    )
  )

  lower <- as.numeric(
    robustlmm::getME(
      direct_fit,
      "lower"
    )
  )

  diagonal <- lower == 0

  direct_boundary <- if (
    length(theta) == 0L ||
    length(lower) != length(theta)
  ) {
    NA
  } else if (!any(diagonal)) {
    FALSE
  } else {
    any(
      theta[diagonal] < 1e-4
    )
  }

  helper_values <- c(
    estimate = helper$estimate,
    std_error = helper$std_error,
    df = helper$df,
    statistic = helper$statistic,
    p_value = helper$p_value
  )

  tolerances <- c(
    estimate = 1e-8,
    std_error = 1e-8,
    df = 1e-3,
    statistic = 1e-8,
    p_value = 1e-8
  )

  for (quantity in names(helper_values)) {
    direct_index <- direct_index + 1L

    difference <- abs(
      helper_values[quantity] -
        direct_values[quantity]
    )

    direct_rows[[direct_index]] <-
      data.frame(
        model = model,
        quantity = quantity,
        helper_value =
          unname(
            helper_values[quantity]
          ),
        direct_value =
          unname(
            direct_values[quantity]
          ),
        absolute_difference =
          unname(difference),
        tolerance =
          unname(
            tolerances[quantity]
          ),
        passed =
          is.finite(difference) &&
          difference <=
            tolerances[quantity],
        stringsAsFactors = FALSE
      )
  }

  for (ci_index in 1:2) {
    quantity <- c(
      "conf_low",
      "conf_high"
    )[ci_index]

    helper_value <- c(
      helper$conf_low,
      helper$conf_high
    )[ci_index]

    direct_value <-
      direct_ci[ci_index]

    difference <- abs(
      helper_value -
        direct_value
    )

    direct_index <- direct_index + 1L

    direct_rows[[direct_index]] <-
      data.frame(
        model = model,
        quantity = quantity,
        helper_value = helper_value,
        direct_value = direct_value,
        absolute_difference =
          difference,
        tolerance = 1e-8,
        passed =
          is.finite(difference) &&
          difference <= 1e-8,
        stringsAsFactors = FALSE
      )
  }

  direct_index <- direct_index + 1L

  direct_rows[[direct_index]] <-
    data.frame(
      model = model,
      quantity = "convergence_code",
      helper_value =
        helper$convergence_code,
      direct_value =
        direct_convergence_code,
      absolute_difference = abs(
        helper$convergence_code -
          direct_convergence_code
      ),
      tolerance = 0,
      passed = identical(
        as.numeric(
          helper$convergence_code
        ),
        as.numeric(
          direct_convergence_code
        )
      ),
      stringsAsFactors = FALSE
    )

  direct_index <- direct_index + 1L

  direct_rows[[direct_index]] <-
    data.frame(
      model = model,
      quantity = "boundary_fit",
      helper_value =
        as.numeric(
          helper$boundary_fit
        ),
      direct_value =
        as.numeric(
          direct_boundary
        ),
      absolute_difference =
        if (
          is.na(helper$boundary_fit) ||
          is.na(direct_boundary)
        ) {
          NA_real_
        } else {
          abs(
            as.numeric(
              helper$boundary_fit
            ) -
              as.numeric(
                direct_boundary
              )
          )
        },
      tolerance = 0,
      passed = identical(
        helper$boundary_fit,
        direct_boundary
      ),
      stringsAsFactors = FALSE
    )

  status_rows[[model]] <- data.frame(
    model = model,
    helper_fit_available =
      helper$fit_available,
    helper_inference_complete =
      helper$inference_complete,
    helper_convergence_code =
      helper$convergence_code,
    helper_convergence_zero =
      helper$convergence_code_zero,
    helper_boundary_fit =
      helper$boundary_fit,
    direct_convergence_code =
      direct_convergence_code,
    direct_boundary_fit =
      direct_boundary,
    helper_fit_warning =
      helper$fit_warning,
    helper_fit_message =
      helper$fit_message,
    helper_inference_warning =
      helper$inference_warning,
    helper_inference_message =
      helper$inference_message,
    ci_warning =
      ci_capture$warning,
    ci_message =
      ci_capture$message,
    stringsAsFactors = FALSE
  )
}

direct_comparison <- do.call(
  rbind,
  direct_rows
)
rownames(direct_comparison) <- NULL

direct_status <- do.call(
  rbind,
  status_rows
)
rownames(direct_status) <- NULL

add_check(
  "Numerical validation",
  "all_helper_direct_quantities_match",
  all(
    direct_comparison$passed
  ),
  details = paste(
    sum(
      direct_comparison$passed
    ),
    "of",
    nrow(direct_comparison),
    "quantities matched."
  )
)

add_check(
  "Numerical validation",
  "all_three_models_return_complete_inference",
  all(
    direct_status$
      helper_fit_available
  ) &&
    all(
      direct_status$
        helper_inference_complete
    ),
  details = paste(
    direct_status$model,
    direct_status$
      helper_inference_complete,
    sep = "=",
    collapse = "; "
  )
)

add_check(
  "Convergence",
  "processfit_codes_are_available_and_zero",
  all(
    is.finite(
      direct_status$
        direct_convergence_code
    )
  ) &&
    all(
      direct_status$
        direct_convergence_code ==
        0
    ),
  details = paste(
    direct_status$model,
    direct_status$
      direct_convergence_code,
    sep = "=",
    collapse = "; "
  )
)

# -------------------------------------------------------------------------
# Boundary behavior: deterministic bookkeeping check plus an optional
# behavioral search for a real robustlmm boundary fit.
# -------------------------------------------------------------------------

synthetic_boundary_result <- list(
  model = "study2_robust_rs",
  model_label =
    "Study 2 robust random slope",
  estimate = 0.10,
  std_error = 0.04,
  df = 8,
  statistic = 2.5,
  p_value = 0.036,
  conf_low = 0.01,
  conf_high = 0.19,
  reject = TRUE,
  cover = TRUE,
  fit_available = TRUE,
  inference_complete = TRUE,
  convergence_code = 0,
  convergence_code_zero = TRUE,
  boundary_fit = TRUE,
  estimated_random_intercept_sd =
    0.80,
  estimated_random_slope_sd = 0,
  estimated_residual_sd = 1,
  residual_weight_minimum = 0.8,
  residual_weight_mean = 0.95,
  residual_weight_prop_below_0_5 = 0,
  residual_weight_prop_below_0_8 = 0,
  residual_weight_count = 100,
  random_effect_weight_minimum = 0.8,
  random_effect_weight_mean = 0.95,
  random_effect_weight_prop_below_0_5 = 0,
  random_effect_weight_prop_below_0_8 = 0,
  random_effect_weight_count = 20,
  fit_warning = NA_character_,
  fit_message = NA_character_,
  fit_error = NA_character_,
  inference_warning = NA_character_,
  inference_message = NA_character_,
  inference_error = NA_character_,
  process_warning = NA_character_,
  process_message = NA_character_,
  process_error = NA_character_,
  variance_component_error = NA_character_,
  summary_column_names = NA_character_,
  fit_elapsed_sec = 1,
  inference_elapsed_sec = 1,
  process_elapsed_sec = 0,
  total_elapsed_sec = 2,
  fit = NULL
)

synthetic_boundary_row <-
  rmm_result_to_row(
    result =
      synthetic_boundary_result,
    replicate = 1L,
    replicate_seed = 20269010L
  )

add_check(
  "Boundary behavior",
  "boundary_is_nonfatal_when_fit_and_inference_are_usable",
  isTRUE(
    synthetic_boundary_row$
      usable
  ),
  details = paste(
    "boundary:",
    synthetic_boundary_row$
      boundary_fit,
    "; usable:",
    synthetic_boundary_row$usable
  )
)

# Optional actual boundary search. A failure to find a boundary in this
# deliberately small search is not a failed gate; if a boundary is found,
# it must remain usable whenever inference is complete and convergence is 0.

boundary_rows <- list()
boundary_found <- FALSE

for (candidate_seed in seq_len(12L)) {
  set.seed(
    20269100L +
      candidate_seed
  )

  cluster <- factor(
    rep(
      seq_len(10L),
      each = 20L
    )
  )

  x <- stats::rnorm(
    length(cluster)
  )

  out <- 0.10 +
    0.25 * x +
    stats::rnorm(
      length(cluster),
      sd = 1
    )

  candidate <- data.frame(
    cluster = cluster,
    x = x,
    out = out
  )

  candidate_result <-
    rmm_fit_and_extract(
      dat = candidate,
      model =
        "study1_robust_ri",
      beta = 0.25,
      alpha = alpha,
      return_fit = FALSE
    )

  candidate_row <-
    rmm_result_to_row(
      result =
        candidate_result,
      replicate =
        candidate_seed,
      replicate_seed =
        20269100L +
        candidate_seed
    )

  boundary_rows[[
    candidate_seed
  ]] <- data.frame(
    candidate_seed =
      20269100L +
      candidate_seed,
    fit_available =
      candidate_result$
        fit_available,
    inference_complete =
      candidate_result$
        inference_complete,
    convergence_code =
      candidate_result$
        convergence_code,
    boundary_fit =
      candidate_result$
        boundary_fit,
    usable =
      candidate_row$usable,
    stringsAsFactors = FALSE
  )

  if (isTRUE(
    candidate_result$
      boundary_fit
  )) {
    boundary_found <- TRUE
    break
  }
}

boundary_search <- do.call(
  rbind,
  boundary_rows
)
rownames(boundary_search) <- NULL

actual_boundary_ok <- if (
  boundary_found
) {
  boundary_case <- boundary_search[
    boundary_search$
      boundary_fit %in% TRUE,
    ,
    drop = FALSE
  ]

  all(
    !(
      boundary_case$
        inference_complete %in% TRUE &
        boundary_case$
          convergence_code == 0
    ) |
      boundary_case$usable
  )
} else {
  TRUE
}

add_check(
  "Boundary behavior",
  "actual_boundary_if_found_obeys_nonfatal_rule",
  actual_boundary_ok,
  required = FALSE,
  details = if (boundary_found) {
    paste(
      "A boundary case was found;",
      "usable =",
      boundary_search$usable[
        which(
          boundary_search$
            boundary_fit %in% TRUE
        )[1L]
      ]
    )
  } else {
    paste(
      "No boundary fit found in",
      nrow(boundary_search),
      "small characterization attempts."
    )
  }
)

# -------------------------------------------------------------------------
# Specification record and checksums.
# -------------------------------------------------------------------------

specification <- data.frame(
  item = c(
    "robustlmm_minimum_version",
    "fit_method",
    "fit_setting",
    "study1_robust_ri_formula",
    "study2_robust_ri_formula",
    "study2_robust_rs_formula",
    "fixed_effect_inference",
    "fixed_effect_covariance",
    "confidence_interval",
    "convergence_rule",
    "boundary_rule",
    "boundary_disposition",
    "rescue_rule"
  ),
  value = c(
    "3.5.0-2",
    "DAStau",
    "RSEn (explicit)",
    expected_formulas[
      "study1_robust_ri"
    ],
    expected_formulas[
      "study2_robust_ri"
    ],
    expected_formulas[
      "study2_robust_rs"
    ],
    paste(
      "summary(fit,",
      "df = 'satterthwaite')"
    ),
    "robustlmm default covariance",
    paste(
      "Wald t interval with",
      "Satterthwaite df"
    ),
    paste(
      "robustlmm::processFit();",
      "code 0 = converged"
    ),
    paste(
      "any diagonal theta < 1e-4,",
      "using getME(theta/lower)"
    ),
    paste(
      "record boundary diagnostically;",
      "do not fail an otherwise usable fit"
    ),
    "none; no RSEa or DASvar fallback"
  ),
  stringsAsFactors = FALSE
)

source_files <- c(
  robust_mixed_model_pilot_helpers =
    helper_path,
  phase4a_audit = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_phase4a_specification_audit.R"
  ),
  pwr_func_study1 =
    file.path(
      project_root,
      "R",
      "pwr_func_study1.R"
    ),
  pwr_func_study1_helpers =
    file.path(
      project_root,
      "R",
      "pwr_func_study1_helpers.R"
    ),
  pwr_func_study2 =
    file.path(
      project_root,
      "R",
      "pwr_func_study2.R"
    ),
  pwr_func_study2_helpers =
    file.path(
      project_root,
      "R",
      "pwr_func_study2_helpers.R"
    )
)

source_checksums <- rmm_source_checksums(
  project_root = project_root,
  files = source_files[
    names(source_files) %in% c(
      "robust_mixed_model_pilot_helpers",
      "phase4a_audit"
    )
  ]
)

# Add all production paths explicitly, without relying on duplicate names
# from rmm_source_checksums().
additional_checksums <- data.frame(
  source = c(
    "phase4a_pwr_func_study1",
    "phase4a_pwr_func_study1_helpers",
    "phase4a_pwr_func_study2",
    "phase4a_pwr_func_study2_helpers"
  ),
  path = normalizePath(
    source_files[
      c(
        "pwr_func_study1",
        "pwr_func_study1_helpers",
        "pwr_func_study2",
        "pwr_func_study2_helpers"
      )
    ],
    winslash = "/",
    mustWork = TRUE
  ),
  md5 = unname(
    tools::md5sum(
      source_files[
        c(
          "pwr_func_study1",
          "pwr_func_study1_helpers",
          "pwr_func_study2",
          "pwr_func_study2_helpers"
        )
      ]
    )
  ),
  stringsAsFactors = FALSE
)

source_checksums <- rbind(
  source_checksums,
  additional_checksums
)

checks_df <- do.call(
  rbind,
  checks
)
rownames(checks_df) <- NULL

required_failures <- checks_df[
  checks_df$required %in% TRUE &
    !(checks_df$passed %in% TRUE),
  ,
  drop = FALSE
]

package_versions <- data.frame(
  package = c(
    "mmiCATs",
    "robustlmm",
    "lme4",
    "lmerTest",
    "pbkrtest"
  ),
  version = vapply(
    c(
      "mmiCATs",
      "robustlmm",
      "lme4",
      "lmerTest",
      "pbkrtest"
    ),
    function(package_name) {
      if (requireNamespace(
        package_name,
        quietly = TRUE
      )) {
        as.character(
          utils::packageVersion(
            package_name
          )
        )
      } else {
        NA_character_
      }
    },
    character(1)
  ),
  stringsAsFactors = FALSE
)

rmm_write_csv_atomic(
  checks_df,
  file.path(
    output_dir,
    "phase4a_checks.csv"
  )
)

rmm_write_csv_atomic(
  specification,
  file.path(
    output_dir,
    "phase4a_locked_specification.csv"
  )
)

rmm_write_csv_atomic(
  direct_comparison,
  file.path(
    output_dir,
    "phase4a_direct_comparison.csv"
  )
)

rmm_write_csv_atomic(
  direct_status,
  file.path(
    output_dir,
    "phase4a_direct_status.csv"
  )
)

rmm_write_csv_atomic(
  boundary_search,
  file.path(
    output_dir,
    "phase4a_boundary_search.csv"
  )
)

rmm_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "phase4a_source_checksums.csv"
  )
)

rmm_write_csv_atomic(
  package_versions,
  file.path(
    output_dir,
    "phase4a_package_versions.csv"
  )
)

results <- list(
  checks = checks_df,
  specification = specification,
  direct_comparison =
    direct_comparison,
  direct_status = direct_status,
  boundary_search =
    boundary_search,
  source_checksums =
    source_checksums,
  package_versions =
    package_versions,
  session_info =
    utils::sessionInfo()
)

rmm_save_rds_atomic(
  results,
  file.path(
    output_dir,
    "phase4a_results.rds"
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

summary_lines <- c(
  "Robust mixed-model Phase 4A specification-lock audit",
  "",
  paste(
    "Required checks passed:",
    sum(
      checks_df$required %in% TRUE &
        checks_df$passed %in% TRUE
    ),
    "of",
    sum(
      checks_df$required %in% TRUE
    )
  ),
  paste(
    "Unresolved required checks:",
    nrow(required_failures)
  ),
  paste(
    "Direct quantities matched:",
    sum(direct_comparison$passed),
    "of",
    nrow(direct_comparison)
  ),
  paste(
    "Actual boundary found in small search:",
    boundary_found
  ),
  paste(
    "robustlmm version:",
    as.character(
      utils::packageVersion(
        "robustlmm"
      )
    )
  )
)

writeLines(
  summary_lines,
  con = file.path(
    output_dir,
    "phase4a_summary.txt"
  ),
  useBytes = TRUE
)

message("")
message("Phase 4A specification-lock checks:")
print(
  checks_df,
  row.names = FALSE
)

message("")
message("Locked robust mixed-model specification:")
print(
  specification,
  row.names = FALSE
)

message("")
message("Direct helper-versus-robustlmm comparisons:")
print(
  direct_comparison,
  row.names = FALSE
)

message("")
message("Direct fit status:")
print(
  direct_status,
  row.names = FALSE
)

message("")
message(paste(
  "Required checks passed:",
  sum(
    checks_df$required %in% TRUE &
      checks_df$passed %in% TRUE
  ),
  "of",
  sum(
    checks_df$required %in% TRUE
  )
))

message(paste(
  "Results saved to:",
  output_dir
))

if (nrow(required_failures) > 0L) {
  stop(
    paste(
      nrow(required_failures),
      "required Phase 4A check(s) failed.",
      "Do not integrate robust mixed models into production yet."
    ),
    call. = FALSE
  )
}

message("")
message(
  "All Phase 4A robust mixed-model specification gates passed."
)
