# Robust mixed-model Phase 4B production-helper validation
#
# Run after installing the Phase 4B files and running devtools::document().
# This validates the new standalone production helper before Study 1 or Study 2
# method vectors and dispatch are modified.

library(devtools)

load_all()

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-mixed-model-results",
  "phase4b-production-helper"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(
  file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_pilot_helpers.R"
  )
)) {
  stop(
    paste(
      "Phase 4B validation requires the locked Phase 4A",
      "prototype helper in data-raw."
    ),
    call. = FALSE
  )
}

source(
  file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_pilot_helpers.R"
  )
)

rmm_require_packages()

checks <- list()

add_check <- function(check,
                      passed,
                      details = NA_character_) {
  checks[[length(checks) + 1L]] <<-
    data.frame(
      check = check,
      passed = as.logical(passed),
      details = details,
      stringsAsFactors = FALSE
    )
}

message("1. Running focused production-helper tests...")

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-robust-mixed-models.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

message("2. Comparing production helper with locked Phase 4A prototype...")

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
    production_model = "ri",
    prototype_model =
      "study1_robust_ri"
  ),
  study2_robust_ri = list(
    dat = study2_dat,
    production_model = "ri",
    prototype_model =
      "study2_robust_ri"
  ),
  study2_robust_rs = list(
    dat = study2_dat,
    production_model = "rs",
    prototype_model =
      "study2_robust_rs"
  )
)

comparison_rows <- list()
status_rows <- list()
index <- 0L

for (case_name in names(cases)) {
  case <- cases[[case_name]]

  set.seed(20269301L)
  production <- study_fit_robust_mixed(
    dat = case$dat,
    alpha = 0.05,
    model = case$production_model
  )

  set.seed(20269301L)
  prototype <- rmm_fit_and_extract(
    dat = case$dat,
    model = case$prototype_model,
    beta = 0.10,
    alpha = 0.05,
    return_fit = FALSE
  )

  fields <- c(
    "estimate",
    "std_error",
    "df",
    "p_value",
    "conf_low",
    "conf_high",
    "optimizer_code",
    "estimated_random_intercept_sd",
    "estimated_random_slope_sd"
  )

  prototype_fields <- c(
    estimate = "estimate",
    std_error = "std_error",
    df = "df",
    p_value = "p_value",
    conf_low = "conf_low",
    conf_high = "conf_high",
    optimizer_code = "convergence_code",
    estimated_random_intercept_sd =
      "estimated_random_intercept_sd",
    estimated_random_slope_sd =
      "estimated_random_slope_sd"
  )

  tolerances <- c(
    estimate = 1e-8,
    std_error = 1e-8,
    df = 1e-3,
    p_value = 1e-8,
    conf_low = 1e-8,
    conf_high = 1e-8,
    optimizer_code = 0,
    estimated_random_intercept_sd = 1e-8,
    estimated_random_slope_sd = 1e-8
  )

  for (field in fields) {
    index <- index + 1L

    production_value <- as.numeric(
      production[[field]]
    )
    prototype_value <- as.numeric(
      prototype[[
        prototype_fields[field]
      ]]
    )

    both_missing <-
      is.na(production_value) &&
      is.na(prototype_value)

    difference <- if (
      both_missing
    ) {
      0
    } else {
      abs(
        production_value -
          prototype_value
      )
    }

    comparison_rows[[index]] <-
      data.frame(
        model = case_name,
        quantity = field,
        production_value =
          production_value,
        prototype_value =
          prototype_value,
        absolute_difference =
          difference,
        tolerance =
          tolerances[field],
        passed =
          both_missing ||
          (
            is.finite(difference) &&
            difference <=
              tolerances[field]
          ),
        stringsAsFactors = FALSE
      )
  }

  status_rows[[case_name]] <-
    data.frame(
      model = case_name,
      production_converged =
        production$converged,
      production_boundary =
        production$singular,
      production_optimizer_code =
        production$optimizer_code,
      prototype_converged =
        prototype$convergence_code_zero,
      prototype_boundary =
        prototype$boundary_fit,
      prototype_optimizer_code =
        prototype$convergence_code,
      production_warning =
        production$warning,
      prototype_fit_warning =
        prototype$fit_warning,
      prototype_fit_message =
        prototype$fit_message,
      stringsAsFactors = FALSE
    )
}

comparison <- do.call(
  rbind,
  comparison_rows
)
rownames(comparison) <- NULL

status <- do.call(
  rbind,
  status_rows
)
rownames(status) <- NULL

add_check(
  "production_matches_locked_phase4a_prototype",
  all(comparison$passed),
  paste(
    sum(comparison$passed),
    "of",
    nrow(comparison),
    "quantities matched."
  )
)

add_check(
  "all_three_production_models_converged",
  all(status$production_converged),
  paste(
    status$model,
    status$production_converged,
    sep = "=",
    collapse = "; "
  )
)

add_check(
  "known_random_slope_boundary_is_retained_and_nonfatal",
  isTRUE(
    status$production_boundary[
      status$model ==
        "study2_robust_rs"
    ]
  ) &&
    isTRUE(
      status$production_converged[
        status$model ==
          "study2_robust_rs"
      ]
    ),
  paste(
    "boundary:",
    status$production_boundary[
      status$model ==
        "study2_robust_rs"
    ],
    "; converged:",
    status$production_converged[
      status$model ==
        "study2_robust_rs"
    ]
  )
)

message("3. Verifying existing Study 1/2 method vectors are still untouched...")

expected_study1 <- c(
  "ri",
  "cr2",
  "cats",
  "cats_trunc",
  "cats_robust",
  "cats_robustbase"
)

expected_study2 <- c(
  "rs",
  "ri",
  "cr2",
  "cats",
  "cats_trunc",
  "cats_robust",
  "cats_robustbase"
)

add_check(
  "study1_dispatch_not_integrated_yet",
  identical(
    study1_method_names(),
    expected_study1
  ),
  paste(
    study1_method_names(),
    collapse = ","
  )
)

add_check(
  "study2_dispatch_not_integrated_yet",
  identical(
    study2_method_names(),
    expected_study2
  ),
  paste(
    study2_method_names(),
    collapse = ","
  )
)

message("4. Verifying optional dependency declaration...")

description <- read.dcf(
  file.path(
    project_root,
    "DESCRIPTION"
  )
)

suggests <- description[
  1L,
  "Suggests"
]

add_check(
  "robustlmm_declared_as_suggested_dependency",
  grepl(
    "robustlmm[[:space:]]*[(]>= 3[.]5[.]0-2[)]",
    suggests
  ),
  suggests
)

message("5. Saving Phase 4B evidence...")

checks_df <- do.call(
  rbind,
  checks
)
rownames(checks_df) <- NULL

source_files <- c(
  production_helper = file.path(
    project_root,
    "R",
    "robust_mixed_models.R"
  ),
  description = file.path(
    project_root,
    "DESCRIPTION"
  ),
  focused_test = file.path(
    project_root,
    "tests",
    "testthat",
    "test-robust-mixed-models.R"
  ),
  phase4b_validator = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_phase4b_production_validation.R"
  ),
  phase4a_prototype = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_pilot_helpers.R"
  ),
  pwr_func_study1 = file.path(
    project_root,
    "R",
    "pwr_func_study1.R"
  ),
  pwr_func_study1_helpers = file.path(
    project_root,
    "R",
    "pwr_func_study1_helpers.R"
  ),
  pwr_func_study2 = file.path(
    project_root,
    "R",
    "pwr_func_study2.R"
  ),
  pwr_func_study2_helpers = file.path(
    project_root,
    "R",
    "pwr_func_study2_helpers.R"
  )
)

source_checksums <- data.frame(
  source = names(source_files),
  path = normalizePath(
    source_files,
    winslash = "/",
    mustWork = TRUE
  ),
  md5 = unname(
    tools::md5sum(
      source_files
    )
  ),
  stringsAsFactors = FALSE
)

package_names <- c(
  "mmiCATs",
  "robustlmm",
  "lme4",
  "lmerTest",
  "pbkrtest",
  "testthat"
)

package_versions <- data.frame(
  package = package_names,
  version = vapply(
    package_names,
    function(package_name) {
      if (
        requireNamespace(
          package_name,
          quietly = TRUE
        )
      ) {
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

utils::write.csv(
  checks_df,
  file.path(
    output_dir,
    "phase4b_checks.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  comparison,
  file.path(
    output_dir,
    "phase4b_prototype_comparison.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  status,
  file.path(
    output_dir,
    "phase4b_status.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  source_checksums,
  file.path(
    output_dir,
    "phase4b_source_checksums.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  package_versions,
  file.path(
    output_dir,
    "phase4b_package_versions.csv"
  ),
  row.names = FALSE
)

saveRDS(
  list(
    checks = checks_df,
    comparison = comparison,
    status = status,
    source_checksums =
      source_checksums,
    package_versions =
      package_versions,
    session_info =
      utils::sessionInfo()
  ),
  file.path(
    output_dir,
    "phase4b_results.rds"
  ),
  version = 3
)

writeLines(
  capture.output(
    utils::sessionInfo()
  ),
  file.path(
    output_dir,
    "session_info.txt"
  ),
  useBytes = TRUE
)

summary_lines <- c(
  "Robust mixed-model Phase 4B production-helper validation",
  "",
  paste(
    "Checks passed:",
    sum(checks_df$passed),
    "of",
    nrow(checks_df)
  ),
  paste(
    "Prototype quantities matched:",
    sum(comparison$passed),
    "of",
    nrow(comparison)
  ),
  paste(
    "Known random-slope boundary:",
    status$production_boundary[
      status$model ==
        "study2_robust_rs"
    ]
  ),
  paste(
    "Known random-slope converged:",
    status$production_converged[
      status$model ==
        "study2_robust_rs"
    ]
  ),
  paste(
    "Study 1 method vector unchanged:",
    identical(
      study1_method_names(),
      expected_study1
    )
  ),
  paste(
    "Study 2 method vector unchanged:",
    identical(
      study2_method_names(),
      expected_study2
    )
  )
)

writeLines(
  summary_lines,
  file.path(
    output_dir,
    "phase4b_summary.txt"
  ),
  useBytes = TRUE
)

message("")
message("Phase 4B checks:")
print(
  checks_df,
  row.names = FALSE
)

message("")
message("Production versus locked Phase 4A prototype:")
print(
  comparison,
  row.names = FALSE
)

message("")
message("Phase 4B production status:")
print(
  status,
  row.names = FALSE
)

failed <- checks_df[
  !(checks_df$passed %in% TRUE),
  ,
  drop = FALSE
]

if (nrow(failed) > 0L) {
  stop(
    paste(
      nrow(failed),
      "Phase 4B validation check(s) failed.",
      "Do not integrate the new methods into Study 1 or Study 2 dispatch."
    ),
    call. = FALSE
  )
}

message("")
message(
  "All Phase 4B production-helper validations passed."
)
