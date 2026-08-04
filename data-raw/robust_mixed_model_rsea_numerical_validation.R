# Numerical validation for the RSEa random-slope comparison pilot
#
# Source robust_mixed_model_pilot_helpers.R and
# robust_mixed_model_rsea_pilot_helpers.R before this script.

project_root <- rmm_find_project_root()
rmm_require_packages()

pkgload::load_all(
  project_root,
  quiet = TRUE
)

pilot_dir <- file.path(
  project_root,
  "data-raw",
  "robust-mixed-model-results",
  "runtime-pilot"
)

existing_path <- file.path(
  pilot_dir,
  "robust_mixed_model_pilot_replicates.rds"
)

if (!file.exists(existing_path)) {
  stop(
    paste(
      "The completed first runtime pilot was not found at:",
      existing_path
    ),
    call. = FALSE
  )
}

existing <- readRDS(existing_path)

candidate <- existing[
  existing$study == "Study 2" &
    existing$model == "study2_robust_rs",
  ,
  drop = FALSE
]

if (nrow(candidate) == 0L) {
  stop(
    "No Study 2 RSEn random-slope pilot rows were found.",
    call. = FALSE
  )
}

candidate <- candidate[
  order(
    candidate$condition_id,
    candidate$replicate
  ),
  ,
  drop = FALSE
]

validation_row <- candidate[1L, , drop = FALSE]

set.seed(validation_row$replicate_seed)

dat <- rmm_simulate_study2(
  n_clusters = validation_row$n_clusters,
  random_slope_sd =
    validation_row$random_slope_sd,
  contamination =
    validation_row$contamination
)

rsea <- rmm_fit_and_extract_rlmer_setting(
  dat = dat,
  beta = validation_row$beta,
  setting = "RSEa",
  return_fit = TRUE
)

if (!rsea$fit_available) {
  stop(
    paste(
      "The RSEa validation fit failed:",
      rsea$fit_error
    ),
    call. = FALSE
  )
}

direct_summary_capture <- rmm_capture(
  summary(
    rsea$fit,
    df = "satterthwaite"
  )
)

if (!is.na(direct_summary_capture$error)) {
  stop(
    paste(
      "The direct RSEa summary failed:",
      direct_summary_capture$error
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
    rsea$fit,
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
      "The direct RSEa confidence interval failed:",
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

direct_process <- rmm_extract_process_fit(
  rsea$fit
)
direct_boundary <- rmm_is_boundary_fit(
  rsea$fit,
  tol = 1e-4
)

rsea_comparison <- data.frame(
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
    rsea$estimate,
    rsea$std_error,
    rsea$df,
    rsea$statistic,
    rsea$p_value,
    rsea$conf_low,
    rsea$conf_high,
    rsea$convergence_code,
    as.numeric(rsea$boundary_fit)
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
    as.numeric(direct_boundary)
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

rsea_comparison$difference <- abs(
  rsea_comparison$helper_value -
    rsea_comparison$direct_value
)
rsea_comparison$passed <- ifelse(
  is.na(rsea_comparison$helper_value) &
    is.na(rsea_comparison$direct_value),
  TRUE,
  rsea_comparison$difference <=
    rsea_comparison$tolerance
)

lmer_result <- rmm_fit_and_extract_lmer_rs(
  dat = dat,
  beta = validation_row$beta,
  return_fit = TRUE
)

if (!lmer_result$fit_available) {
  stop(
    paste(
      "The conventional validation fit failed:",
      lmer_result$fit_error
    ),
    call. = FALSE
  )
}

direct_lmer_summary_capture <- rmm_capture(
  summary(
    lmer_result$fit,
    ddf = "Kenward-Roger"
  )
)

if (!is.na(direct_lmer_summary_capture$error)) {
  stop(
    paste(
      "The direct conventional summary failed:",
      direct_lmer_summary_capture$error
    ),
    call. = FALSE
  )
}

direct_lmer_table <- stats::coef(
  direct_lmer_summary_capture$value
)
direct_lmer_row <- direct_lmer_table[
  "x",
  ,
  drop = FALSE
]
direct_lmer_boundary <- lme4::isSingular(
  lmer_result$fit,
  tol = 1e-4
)

lmer_comparison <- data.frame(
  quantity = c(
    "estimate",
    "std_error",
    "df",
    "statistic",
    "p_value",
    "boundary_fit"
  ),
  helper_value = c(
    lmer_result$estimate,
    lmer_result$std_error,
    lmer_result$df,
    lmer_result$statistic,
    lmer_result$p_value,
    as.numeric(lmer_result$boundary_fit)
  ),
  direct_value = c(
    as.numeric(
      direct_lmer_row[1L, "Estimate"]
    ),
    as.numeric(
      direct_lmer_row[1L, "Std. Error"]
    ),
    as.numeric(
      direct_lmer_row[1L, "df"]
    ),
    as.numeric(
      direct_lmer_row[1L, "t value"]
    ),
    as.numeric(
      direct_lmer_row[
        1L,
        grep(
          "^Pr[(]",
          colnames(direct_lmer_row),
          value = TRUE
        )[1L]
      ]
    ),
    as.numeric(direct_lmer_boundary)
  ),
  tolerance = c(
    1e-8,
    1e-8,
    1e-6,
    1e-8,
    1e-8,
    0
  ),
  stringsAsFactors = FALSE
)

lmer_comparison$difference <- abs(
  lmer_comparison$helper_value -
    lmer_comparison$direct_value
)
lmer_comparison$passed <- ifelse(
  is.na(lmer_comparison$helper_value) &
    is.na(lmer_comparison$direct_value),
  TRUE,
  lmer_comparison$difference <=
    lmer_comparison$tolerance
)

validation_checks <- data.frame(
  check = c(
    "rsea_direct_comparison_passed",
    "rsea_inference_complete",
    "rsea_convergence_code_available",
    "conventional_direct_comparison_passed",
    "conventional_inference_complete"
  ),
  passed = c(
    all(rsea_comparison$passed),
    rsea$inference_complete,
    is.finite(rsea$convergence_code),
    all(lmer_comparison$passed),
    lmer_result$inference_complete
  ),
  stringsAsFactors = FALSE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-mixed-model-results",
  "random-slope-settings-pilot",
  "numerical-validation"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

source_files <- c(
  robust_mixed_model_pilot_helpers = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_pilot_helpers.R"
  ),
  robust_mixed_model_rsea_pilot_helpers = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_rsea_pilot_helpers.R"
  ),
  robust_mixed_model_rsea_numerical_validation = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_rsea_numerical_validation.R"
  )
)

source_checksums <- rmm_source_checksums(
  project_root = project_root,
  files = source_files
)

rmm_write_csv_atomic(
  rsea_comparison,
  file.path(
    output_dir,
    "rsea_direct_comparison.csv"
  )
)

rmm_write_csv_atomic(
  lmer_comparison,
  file.path(
    output_dir,
    "conventional_rs_direct_comparison.csv"
  )
)

rmm_write_csv_atomic(
  validation_checks,
  file.path(
    output_dir,
    "rsea_pilot_validation_checks.csv"
  )
)

rmm_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "rsea_pilot_source_checksums.csv"
  )
)

rmm_save_rds_atomic(
  list(
    validation_row = validation_row,
    rsea_comparison = rsea_comparison,
    conventional_comparison = lmer_comparison,
    checks = validation_checks,
    source_checksums = source_checksums,
    session_info = utils::sessionInfo()
  ),
  file.path(
    output_dir,
    "rsea_pilot_numerical_validation.rds"
  )
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
message("RSEa settings-pilot numerical validation:")
print(validation_checks, row.names = FALSE)

if (!all(validation_checks$passed)) {
  stop(
    paste(
      "One or more RSEa settings-pilot numerical",
      "validation checks failed. Do not run the",
      "comparison pilot yet."
    ),
    call. = FALSE
  )
}

message("")
message(
  "All RSEa settings-pilot numerical validation checks passed."
)
message(paste("Results saved to:", output_dir))
