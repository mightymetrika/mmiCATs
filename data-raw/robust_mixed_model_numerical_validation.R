# Robust mixed-model numerical validation
#
# Run this script before the runtime pilot. It verifies that the pilot helper
# extracts robustlmm's fixed slope, standard error, robust Satterthwaite
# degrees of freedom, p value, and Wald confidence interval correctly for:
#
#   1. the Study 1 robust random-intercept model;
#   2. the Study 2 robust but misspecified random-intercept model; and
#   3. the Study 2 correctly specified independent random-slope model.
#
# No package functions under R/ are changed.

project_root <- rmm_find_project_root()
rmm_require_packages()

pkgload::load_all(
  project_root,
  quiet = TRUE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-mixed-model-results",
  "numerical-validation"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

validation_seed <- 20260918L
set.seed(validation_seed)

study1_data <- rmm_simulate_study1(
  n_clusters = 20,
  contamination = "vertical"
)

set.seed(validation_seed + 1L)

study2_data <- rmm_simulate_study2(
  n_clusters = 20,
  random_slope_sd = 0.10,
  contamination = "vertical"
)

validation_cases <- list(
  study1_robust_ri = list(
    data = study1_data,
    beta = 0
  ),
  study2_robust_ri = list(
    data = study2_data,
    beta = 0
  ),
  study2_robust_rs = list(
    data = study2_data,
    beta = 0
  )
)

comparison_rows <- list()
validation_status_rows <- list()
structure_text <- character()

for (case_name in names(validation_cases)) {
  case <- validation_cases[[case_name]]

  message(
    paste(
      "Validating",
      rmm_model_label(case_name),
      "..."
    )
  )

  helper_result <- rmm_fit_and_extract(
    dat = case$data,
    model = case_name,
    beta = case$beta,
    alpha = 0.05,
    return_fit = TRUE
  )

  if (!helper_result$fit_available) {
    stop(
      paste(
        "The validation fit failed for",
        case_name,
        ":",
        helper_result$fit_error
      ),
      call. = FALSE
    )
  }

  fit <- helper_result$fit

  direct_summary_capture <- rmm_capture(
    summary(
      fit,
      df = "satterthwaite"
    )
  )

  if (!is.na(direct_summary_capture$error)) {
    stop(
      paste(
        "Direct summary failed for",
        case_name,
        ":",
        direct_summary_capture$error
      ),
      call. = FALSE
    )
  }

  direct_table <- stats::coef(
    direct_summary_capture$value
  )

  if (!("x" %in% rownames(direct_table))) {
    stop(
      paste(
        "The direct summary table did not contain x for",
        case_name
      ),
      call. = FALSE
    )
  }

  direct_extraction <- rmm_extract_coefficient_table(
    direct_summary_capture$value,
    coefficient = "x"
  )

  direct_confint_capture <- rmm_capture(
    stats::confint(
      fit,
      parm = "x",
      level = 0.95,
      method = "Wald",
      vcov_type = "default",
      df = "satterthwaite"
    )
  )

  if (!is.na(direct_confint_capture$error)) {
    stop(
      paste(
        "Direct confint failed for",
        case_name,
        ":",
        direct_confint_capture$error
      ),
      call. = FALSE
    )
  }

  direct_ci <- direct_confint_capture$value[
    "x",
    ,
    drop = TRUE
  ]

  direct_process <- rmm_extract_process_fit(fit)
  direct_boundary_fit <- tryCatch(
    {
      direct_theta <- as.numeric(
        robustlmm::getME(fit, "theta")
      )
      direct_lower <- as.numeric(
        robustlmm::getME(fit, "lower")
      )
      direct_diagonal <- direct_lower == 0

      if (!any(direct_diagonal)) {
        FALSE
      } else {
        any(
          direct_theta[direct_diagonal] <
            1e-4
        )
      }
    },
    error = function(e) NA
  )

  values <- data.frame(
    model = case_name,
    quantity = c(
      "estimate",
      "std_error",
      "df",
      "statistic",
      "p_value",
      "conf_low",
      "conf_high",
      "convergence_code",
      "boundary_fit"
    ),
    helper_value = c(
      helper_result$estimate,
      helper_result$std_error,
      helper_result$df,
      helper_result$statistic,
      helper_result$p_value,
      helper_result$conf_low,
      helper_result$conf_high,
      helper_result$convergence_code,
      as.numeric(helper_result$boundary_fit)
    ),
    direct_value = c(
      direct_extraction$estimate,
      direct_extraction$std_error,
      direct_extraction$df,
      direct_extraction$statistic,
      direct_extraction$p_value,
      as.numeric(direct_ci[1L]),
      as.numeric(direct_ci[2L]),
      direct_process$convergence_code,
      as.numeric(direct_boundary_fit)
    ),
    tolerance = c(
      1e-8,
      1e-8,
      1e-3,
      1e-8,
      1e-8,
      1e-8,
      1e-8,
      0,
      0
    ),
    stringsAsFactors = FALSE
  )

  values$difference <- abs(
    values$helper_value -
      values$direct_value
  )
  values$passed <- ifelse(
    is.na(values$helper_value) &
      is.na(values$direct_value),
    TRUE,
    values$difference <= values$tolerance
  )

  comparison_rows[[case_name]] <- values
  validation_status_rows[[case_name]] <- data.frame(
    model = case_name,
    inference_complete =
      helper_result$inference_complete,
    convergence_code =
      helper_result$convergence_code,
    convergence_code_zero =
      helper_result$convergence_code_zero,
    boundary_fit = helper_result$boundary_fit,
    stringsAsFactors = FALSE
  )

  structure_text <- c(
    structure_text,
    paste0("===== ", case_name, " ====="),
    paste(
      "robustlmm version:",
      as.character(
        utils::packageVersion("robustlmm")
      )
    ),
    paste(
      "Class:",
      paste(class(fit), collapse = ", ")
    ),
    paste(
      "Slots:",
      paste(methods::slotNames(fit), collapse = ", ")
    ),
    paste(
      "Summary columns:",
      paste(
        colnames(direct_table),
        collapse = " | "
      )
    ),
    paste(
      "processFit convergence code:",
      direct_process$convergence_code
    ),
    paste(
      "Boundary fit under lme4 theta/lower criterion:",
      direct_boundary_fit
    ),
    paste(
      "Fit warnings:",
      helper_result$fit_warning
    ),
    paste(
      "Fit messages:",
      helper_result$fit_message
    ),
    paste(
      "Inference warnings:",
      helper_result$inference_warning
    ),
    paste(
      "Inference messages:",
      helper_result$inference_message
    ),
    "",
    "VarCorr:",
    capture.output(
      print(lme4::VarCorr(fit))
    ),
    "",
    "processFit structure:",
    capture.output(
      str(direct_process$raw)
    ),
    "",
    "Summary coefficient table:",
    capture.output(
      print(direct_table)
    ),
    ""
  )

  helper_result$fit <- NULL
  gc(verbose = FALSE)
}

comparison <- do.call(
  rbind,
  comparison_rows
)
rownames(comparison) <- NULL

validation_status <- do.call(
  rbind,
  validation_status_rows
)
rownames(validation_status) <- NULL

validation_checks <- data.frame(
  check = c(
    "all_helper_direct_comparisons_pass",
    "all_inference_complete",
    "all_convergence_codes_available",
    "all_convergence_codes_zero"
  ),
  passed = c(
    all(comparison$passed),
    all(validation_status$inference_complete),
    all(is.finite(
      validation_status$convergence_code
    )),
    all(
      validation_status$convergence_code == 0
    )
  ),
  stringsAsFactors = FALSE
)

source_files <- c(
  robust_mixed_model_pilot_helpers = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_pilot_helpers.R"
  ),
  robust_mixed_model_numerical_validation = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_numerical_validation.R"
  )
)

source_checksums <- rmm_source_checksums(
  project_root = project_root,
  files = source_files
)

metadata <- list(
  purpose = paste(
    "Validate robustlmm extraction before the separate",
    "robust mixed-model runtime pilot."
  ),
  validation_seed = validation_seed,
  robustlmm_version = as.character(
    utils::packageVersion("robustlmm")
  ),
  robustlmm_method = "DAStau",
  robustlmm_setting = paste(
    "Package default. The setting argument is intentionally omitted."
  ),
  inference = paste(
    "summary(fit, df = 'satterthwaite') with the default covariance;",
    "Wald intervals use the corresponding t critical value."
  ),
  source_checksums = source_checksums,
  session_info = utils::sessionInfo()
)

rmm_save_rds_atomic(
  list(
    comparison = comparison,
    validation_status = validation_status,
    checks = validation_checks,
    metadata = metadata
  ),
  file.path(
    output_dir,
    "robust_mixed_model_numerical_validation.rds"
  )
)

rmm_write_csv_atomic(
  comparison,
  file.path(
    output_dir,
    "robust_mixed_model_direct_comparison.csv"
  )
)

rmm_write_csv_atomic(
  validation_checks,
  file.path(
    output_dir,
    "robust_mixed_model_validation_checks.csv"
  )
)

rmm_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "robust_mixed_model_source_checksums.csv"
  )
)

writeLines(
  structure_text,
  con = file.path(
    output_dir,
    "robust_mixed_model_structure_probe.txt"
  ),
  useBytes = TRUE
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(
    output_dir,
    "session_info.txt"
  ),
  useBytes = TRUE
)

message("")
message("Robust mixed-model numerical validation checks:")
print(validation_checks, row.names = FALSE)

if (!all(validation_checks$passed)) {
  stop(
    paste(
      "One or more robust mixed-model numerical validation",
      "checks failed. Do not run the runtime pilot yet."
    ),
    call. = FALSE
  )
}

message("")
message(
  "All robust mixed-model numerical validation checks passed."
)
message(paste("Results saved to:", output_dir))
