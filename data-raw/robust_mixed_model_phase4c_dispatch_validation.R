# Robust mixed-model Phase 4C dispatch-integration validation
#
# Validates method schedules, seed preservation, dispatch, bookkeeping, and
# existing-method invariance after robust mixed models are added to Study 1/2.

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
  "phase4c-dispatch-integration"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

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

drop_runtime <- function(x) {
  x[
    ,
    setdiff(
      names(x),
      "runtime_sec"
    ),
    drop = FALSE
  ]
}

sort_replicates <- function(x) {
  x <- x[
    order(
      x$replicate,
      x$method
    ),
    ,
    drop = FALSE
  ]
  rownames(x) <- NULL
  x
}


bind_rows_fill <- function(...) {
  data_list <- list(...)

  all_names <- unique(
    unlist(
      lapply(
        data_list,
        names
      ),
      use.names = FALSE
    )
  )

  aligned <- lapply(
    data_list,
    function(data) {
      missing_names <- setdiff(
        all_names,
        names(data)
      )

      for (name in missing_names) {
        data[[name]] <- NA
      }

      data[
        ,
        all_names,
        drop = FALSE
      ]
    }
  )

  out <- do.call(
    rbind,
    aligned
  )
  rownames(out) <- NULL
  out
}

message("1. Running focused Phase 4C integration tests...")

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-robust-mixed-integration.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

message("2. Verifying canonical schedules and preservation of existing seeds...")

old_study1 <- c(
  "ri",
  "cr2",
  "cats",
  "cats_trunc",
  "cats_robust",
  "cats_robustbase"
)

old_study2 <- c(
  "rs",
  "ri",
  "cr2",
  "cats",
  "cats_trunc",
  "cats_robust",
  "cats_robustbase"
)

expected_study1 <- c(
  old_study1,
  "robust_ri"
)

expected_study2 <- c(
  old_study2,
  "robust_ri",
  "robust_rs"
)

add_check(
  "study1_method_schedule_is_append_only",
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
  "study2_method_schedule_is_append_only",
  identical(
    study2_method_names(),
    expected_study2
  ),
  paste(
    study2_method_names(),
    collapse = ","
  )
)

replicate_seed <- 20269501L

study1_old_seed_match <- all(
  vapply(
    seq_along(old_study1),
    function(index) {
      method <- old_study1[index]

      identical(
        study1_method_seed(
          replicate_seed =
            replicate_seed,
          method_index = match(
            method,
            study1_method_names()
          )
        ),
        study1_method_seed(
          replicate_seed =
            replicate_seed,
          method_index = index
        )
      )
    },
    logical(1)
  )
)

study2_old_seed_match <- all(
  vapply(
    seq_along(old_study2),
    function(index) {
      method <- old_study2[index]

      identical(
        study2_method_seed(
          replicate_seed =
            replicate_seed,
          method_index = match(
            method,
            study2_method_names()
          )
        ),
        study2_method_seed(
          replicate_seed =
            replicate_seed,
          method_index = index
        )
      )
    },
    logical(1)
  )
)

add_check(
  "existing_method_seed_indices_are_preserved",
  study1_old_seed_match &&
    study2_old_seed_match,
  paste(
    "Study 1:",
    study1_old_seed_match,
    "; Study 2:",
    study2_old_seed_match
  )
)

message("3. Checking existing-method order invariance after schedule extension...")

study1_existing_forward <- suppressWarnings(
  pwr_func_study1(
    n_clusters = 8L,
    cluster_size = 30L,
    beta = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 1L,
    methods = old_study1,
    seed = 20269502L,
    keep_replicates = TRUE
  )
)

study1_existing_reverse <- suppressWarnings(
  pwr_func_study1(
    n_clusters = 8L,
    cluster_size = 30L,
    beta = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 1L,
    methods = rev(old_study1),
    seed = 20269502L,
    keep_replicates = TRUE
  )
)

study1_existing_match <- isTRUE(
  all.equal(
    drop_runtime(
      sort_replicates(
        study1_existing_forward$replicates
      )
    ),
    drop_runtime(
      sort_replicates(
        study1_existing_reverse$replicates
      )
    ),
    tolerance = 1e-12,
    check.attributes = TRUE
  )
)

study2_existing_forward <- suppressWarnings(
  pwr_func_study2(
    n_clusters = 8L,
    cluster_size = 30L,
    beta = 0.10,
    random_slope_sd = 0.05,
    contamination = "vertical",
    contamination_size = 6,
    reps = 1L,
    methods = old_study2,
    seed = 20269503L,
    keep_replicates = TRUE
  )
)

study2_existing_reverse <- suppressWarnings(
  pwr_func_study2(
    n_clusters = 8L,
    cluster_size = 30L,
    beta = 0.10,
    random_slope_sd = 0.05,
    contamination = "vertical",
    contamination_size = 6,
    reps = 1L,
    methods = rev(old_study2),
    seed = 20269503L,
    keep_replicates = TRUE
  )
)

study2_existing_match <- isTRUE(
  all.equal(
    drop_runtime(
      sort_replicates(
        study2_existing_forward$replicates
      )
    ),
    drop_runtime(
      sort_replicates(
        study2_existing_reverse$replicates
      )
    ),
    tolerance = 1e-12,
    check.attributes = TRUE
  )
)

add_check(
  "existing_methods_remain_order_invariant",
  study1_existing_match &&
    study2_existing_match,
  paste(
    "Study 1:",
    study1_existing_match,
    "; Study 2:",
    study2_existing_match
  )
)

message("4. Running the new methods through the top-level simulations...")

study1_new <- suppressWarnings(
  pwr_func_study1(
    n_clusters = 8L,
    cluster_size = 30L,
    beta = 0.10,
    contamination = "vertical",
    contamination_size = 6,
    reps = 1L,
    methods = "robust_ri",
    seed = 20269504L,
    keep_replicates = TRUE
  )
)

study2_new <- suppressWarnings(
  pwr_func_study2(
    n_clusters = 8L,
    cluster_size = 30L,
    beta = 0.10,
    random_slope_sd = 0.05,
    contamination = "vertical",
    contamination_size = 6,
    reps = 1L,
    methods = c(
      "robust_ri",
      "robust_rs"
    ),
    seed = 20269505L,
    keep_replicates = TRUE
  )
)

new_method_rows <- bind_rows_fill(
  data.frame(
    study = "Study 1",
    study1_new$replicates,
    stringsAsFactors = FALSE
  ),
  data.frame(
    study = "Study 2",
    study2_new$replicates,
    stringsAsFactors = FALSE
  )
)

new_methods_complete <-
  nrow(study1_new$replicates) == 1L &&
  nrow(study2_new$replicates) == 2L &&
  all(
    new_method_rows$
      fit_success %in% TRUE
  ) &&
  all(
    is.finite(
      new_method_rows$estimate
    )
  ) &&
  all(
    is.finite(
      new_method_rows$std_error
    )
  ) &&
  all(
    is.finite(
      new_method_rows$df
    )
  ) &&
  all(
    is.finite(
      new_method_rows$p_value
    )
  )

add_check(
  "new_methods_run_through_top_level_simulations",
  new_methods_complete,
  paste(
    "rows:",
    nrow(new_method_rows),
    "; successful:",
    sum(
      new_method_rows$
        fit_success %in% TRUE
    )
  )
)

summary_has_new_methods <-
  identical(
    as.character(
      study1_new$summary$model
    ),
    "robust_ri"
  ) &&
  setequal(
    as.character(
      study2_new$summary$model
    ),
    c(
      "robust_ri",
      "robust_rs"
    )
  )

add_check(
  "new_methods_flow_into_simulation_summaries",
  summary_has_new_methods,
  paste(
    "Study 1:",
    paste(
      study1_new$summary$model,
      collapse = ","
    ),
    "; Study 2:",
    paste(
      study2_new$summary$model,
      collapse = ","
    )
  )
)

message("5. Rechecking the known robust random-slope boundary through dispatch...")

set.seed(20269002L)

boundary_dat <- study2_simulate_data(
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

boundary_realized_mean <- mean(
  boundary_dat$true_cluster_slope[
    !duplicated(boundary_dat$cluster)
  ]
)

boundary_realized_sd <- stats::sd(
  boundary_dat$random_slope[
    !duplicated(boundary_dat$cluster)
  ]
)

boundary_result <- study2_fit_method(
  dat = boundary_dat,
  method = "robust_rs",
  beta = 0.10,
  alpha = 0.05,
  replicate_id = 1L,
  method_seed = 20269413L,
  realized_mean_slope =
    boundary_realized_mean,
  realized_random_slope_sd =
    boundary_realized_sd
)

add_check(
  "robust_rs_boundary_remains_nonfatal_after_dispatch",
  isTRUE(
    boundary_result$fit_success
  ) &&
    isTRUE(
      boundary_result$converged
    ) &&
    isTRUE(
      boundary_result$singular
    ) &&
    identical(
      boundary_result$optimizer_code,
      0
    ),
  paste(
    "success:",
    boundary_result$fit_success,
    "; converged:",
    boundary_result$converged,
    "; boundary:",
    boundary_result$singular,
    "; code:",
    boundary_result$optimizer_code
  )
)

message("6. Checking the Phase 4B production-helper baseline...")

helper_path <- file.path(
  project_root,
  "R",
  "robust_mixed_models.R"
)

helper_md5 <- unname(
  tools::md5sum(
    helper_path
  )
)

expected_helper_md5 <-
  "a3f55f48736df665fa8ce45706dd9c49"

add_check(
  "phase4b_production_helper_is_unchanged",
  identical(
    helper_md5,
    expected_helper_md5
  ),
  paste(
    "MD5:",
    helper_md5
  )
)

message("7. Saving Phase 4C evidence...")

checks_df <- do.call(
  rbind,
  checks
)
rownames(checks_df) <- NULL

source_files <- c(
  robust_mixed_models = file.path(
    project_root,
    "R",
    "robust_mixed_models.R"
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
  ),
  test_study1 = file.path(
    project_root,
    "tests",
    "testthat",
    "test-pwr_func_study1.R"
  ),
  test_study2 = file.path(
    project_root,
    "tests",
    "testthat",
    "test-pwr_func_study2.R"
  ),
  integration_test = file.path(
    project_root,
    "tests",
    "testthat",
    "test-robust-mixed-integration.R"
  ),
  phase4c_validator = file.path(
    project_root,
    "data-raw",
    "robust_mixed_model_phase4c_dispatch_validation.R"
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
  "clubSandwich",
  "robust",
  "robustbase",
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
    "phase4c_checks.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  new_method_rows,
  file.path(
    output_dir,
    "phase4c_new_method_rows.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  boundary_result,
  file.path(
    output_dir,
    "phase4c_boundary_dispatch.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  source_checksums,
  file.path(
    output_dir,
    "phase4c_source_checksums.csv"
  ),
  row.names = FALSE
)

utils::write.csv(
  package_versions,
  file.path(
    output_dir,
    "phase4c_package_versions.csv"
  ),
  row.names = FALSE
)

saveRDS(
  list(
    checks = checks_df,
    new_method_rows =
      new_method_rows,
    boundary_dispatch =
      boundary_result,
    source_checksums =
      source_checksums,
    package_versions =
      package_versions,
    session_info =
      utils::sessionInfo()
  ),
  file.path(
    output_dir,
    "phase4c_results.rds"
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
  "Robust mixed-model Phase 4C dispatch-integration validation",
  "",
  paste(
    "Checks passed:",
    sum(
      checks_df$passed
    ),
    "of",
    nrow(checks_df)
  ),
  paste(
    "Study 1 canonical methods:",
    paste(
      study1_method_names(),
      collapse = ", "
    )
  ),
  paste(
    "Study 2 canonical methods:",
    paste(
      study2_method_names(),
      collapse = ", "
    )
  ),
  paste(
    "Existing Study 1 order invariance:",
    study1_existing_match
  ),
  paste(
    "Existing Study 2 order invariance:",
    study2_existing_match
  ),
  paste(
    "New top-level method fits successful:",
    sum(
      new_method_rows$
        fit_success %in% TRUE
    ),
    "of",
    nrow(new_method_rows)
  ),
  paste(
    "Known robust RS boundary usable:",
    boundary_result$fit_success
  )
)

writeLines(
  summary_lines,
  file.path(
    output_dir,
    "phase4c_summary.txt"
  ),
  useBytes = TRUE
)

message("")
message("Phase 4C checks:")
print(
  checks_df,
  row.names = FALSE
)

message("")
message("New robust mixed-model top-level rows:")
print(
  new_method_rows[
    ,
    c(
      "study",
      "method",
      "fit_success",
      "converged",
      "singular",
      "optimizer_code",
      "estimate",
      "std_error",
      "df",
      "p_value"
    )
  ],
  row.names = FALSE
)

message("")
message("Known robust RS boundary dispatch:")
print(
  boundary_result[
    ,
    c(
      "method",
      "fit_success",
      "converged",
      "singular",
      "optimizer_code",
      "estimated_random_intercept_sd",
      "estimated_random_slope_sd"
    )
  ],
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
      "Phase 4C validation check(s) failed.",
      "Do not proceed to the end-to-end robust mixed-model pilot."
    ),
    call. = FALSE
  )
}

message("")
message(
  "All Phase 4C robust mixed-model dispatch validations passed."
)
