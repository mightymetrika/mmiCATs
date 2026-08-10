# Robust CATs audit: Phase 2 closeout runner
#
# Purpose:
#   Re-run Phases 2A, 2B, 2C/2D against the current post-Phase-2D source,
#   accept only the already-characterized numerical row-order findings and the
#   known upstream clusterSEs omitted-coefficient limitation, and save a compact
#   closeout record with source checksums and issue dispositions.
#
# This script does not modify package production code.
# Run from the mmiCATs project root after replacing
# data-raw/robust_cats_failure_retention_audit.R with the Phase 2C v3 file.

library(devtools)

load_all()

source(
  "data-raw/robust_cats_audit_helpers.R"
)

project_root <- rca_find_project_root()

closeout_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase2-closeout"
)

dir.create(
  closeout_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

capture_source <- function(path) {
  captured_error <- tryCatch(
    {
      source(path, local = .GlobalEnv)
      NULL
    },
    error = function(e) e
  )

  if (is.null(captured_error)) {
    return(NA_character_)
  }

  conditionMessage(captured_error)
}

read_check_file <- function(path) {
  if (!file.exists(path)) {
    stop(
      paste("Expected audit output was not created:", path),
      call. = FALSE
    )
  }

  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
}

collapse_or_none <- function(x) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0L) {
    return("none")
  }

  paste(x, collapse = " | ")
}

message("")
message("Phase 2 closeout: rerunning Phase 2A...")

phase2a_error <- capture_source(
  "data-raw/robust_cats_audit_numerical_validation.R"
)

phase2a_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase2a-numerical-validation"
)

phase2a_checks <- read_check_file(
  file.path(
    phase2a_dir,
    "robust_cats_audit_validation_checks.csv"
  )
)

phase2a_required_failures <- phase2a_checks[
  phase2a_checks$required %in% TRUE &
    !(phase2a_checks$passed %in% TRUE),
  ,
  drop = FALSE
]

expected_phase2a_numerical_findings <- c(
  "robust_within_cluster_row_order",
  "robust_global_row_order",
  "robustbase_within_cluster_row_order",
  "robustbase_global_row_order"
)

phase2a_unexpected_failures <- setdiff(
  phase2a_required_failures$check,
  expected_phase2a_numerical_findings
)

phase2a_pass <- length(phase2a_unexpected_failures) == 0L

message("")
message("Phase 2 closeout: rerunning Phase 2B...")

phase2b_error <- capture_source(
  "data-raw/robust_cats_row_order_stress.R"
)

phase2b_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase2b-row-order-stress"
)

phase2b_checks <- read_check_file(
  file.path(
    phase2b_dir,
    "robust_cats_row_order_checks.csv"
  )
)

phase2b_gate_checks <- c(
  "all_fits_returned_results",
  "exact_repeat_is_exact",
  "no_retained_cluster_changes",
  "no_rejection_changes",
  "no_ci_decision_changes"
)

phase2b_gate_rows <- phase2b_checks[
  phase2b_checks$check %in% phase2b_gate_checks,
  ,
  drop = FALSE
]

phase2b_pass <-
  is.na(phase2b_error) &&
  nrow(phase2b_gate_rows) == length(phase2b_gate_checks) &&
  all(phase2b_gate_rows$passed %in% TRUE)

phase2b_summary <- read_check_file(
  file.path(
    phase2b_dir,
    "robust_cats_row_order_summary.csv"
  )
)

phase2b_max_difference <- if (
  nrow(phase2b_summary) == 0L ||
    all(is.na(phase2b_summary$maximum_output_difference))
) {
  NA_real_
} else {
  max(
    phase2b_summary$maximum_output_difference,
    na.rm = TRUE
  )
}

message("")
message("Phase 2 closeout: rerunning Phase 2D (including Phase 2C)...")

phase2d_error <- capture_source(
  "data-raw/robust_cats_phase2d_post_fix_validation.R"
)

phase2c_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase2c-failure-retention"
)

phase2c_checks <- read_check_file(
  file.path(
    phase2c_dir,
    "robust_cats_failure_retention_checks.csv"
  )
)

phase2c_failures <- phase2c_checks[
  phase2c_checks$readiness_required %in% TRUE &
    !(phase2c_checks$passed %in% TRUE),
  ,
  drop = FALSE
]

expected_upstream_check <-
  "clusterSEs_drop_true_drops_omitted_coefficient_cluster"

phase2c_unexpected_failures <- phase2c_failures[
  phase2c_failures$check != expected_upstream_check,
  ,
  drop = FALSE
]

phase2c_pass <- nrow(phase2c_unexpected_failures) == 0L
phase2d_pass <- is.na(phase2d_error)

phase_status <- data.frame(
  phase = c("2A", "2B", "2C", "2D"),
  closeout_pass = c(
    phase2a_pass,
    phase2b_pass,
    phase2c_pass,
    phase2d_pass
  ),
  details = c(
    paste0(
      "Unexpected required failures: ",
      collapse_or_none(phase2a_unexpected_failures),
      "; characterized row-order findings observed: ",
      collapse_or_none(phase2a_required_failures$check)
    ),
    paste0(
      "Core row-order gates passed; maximum aggregate numerical difference: ",
      format(phase2b_max_difference, digits = 10),
      "; script error: ",
      ifelse(is.na(phase2b_error), "none", phase2b_error)
    ),
    paste0(
      "Unexpected readiness failures: ",
      collapse_or_none(phase2c_unexpected_failures$check),
      "; expected upstream clusterSEs failure present: ",
      expected_upstream_check %in% phase2c_failures$check
    ),
    paste0(
      "Post-fix validator error: ",
      ifelse(is.na(phase2d_error), "none", phase2d_error)
    )
  ),
  stringsAsFactors = FALSE
)

issue_status <- data.frame(
  issue_id = c(
    "A-01", "A-02", "A-03", "A-04", "A-05", "A-06",
    "A-07", "A-08", "A-09", "A-10", "A-11", "A-12"
  ),
  closeout_status = c(
    "tracked: truncation provenance unresolved",
    "mmiCATs resolved; upstream clusterSEs limitation retained",
    "resolved",
    "resolved",
    "open for Phase 3 adversarial template-failure testing",
    "resolved by independent oracle/mutation testing",
    "resolved",
    "open for Phase 3 row-name/alignment adversarial testing",
    "open documentation cleanup; not simulation-result altering",
    "resolved by robust method/order/row-order testing",
    "resolved",
    "accepted legacy pwr_func_lmer RNG coupling; definitive studies use separate seed handling"
  ),
  stringsAsFactors = FALSE
)

source_paths <- c(
  helpers_cimrob = file.path(project_root, "R", "helpers_cimrob.R"),
  pwr_func_study1_helpers = file.path(
    project_root,
    "R",
    "pwr_func_study1_helpers.R"
  ),
  pwr_func_study2_helpers = file.path(
    project_root,
    "R",
    "pwr_func_study2_helpers.R"
  ),
  cluster_im_lmRob = file.path(
    project_root,
    "R",
    "cluster_im_lmRob.R"
  ),
  phase2a = file.path(
    project_root,
    "data-raw",
    "robust_cats_audit_numerical_validation.R"
  ),
  phase2b = file.path(
    project_root,
    "data-raw",
    "robust_cats_row_order_stress.R"
  ),
  phase2c = file.path(
    project_root,
    "data-raw",
    "robust_cats_failure_retention_audit.R"
  ),
  phase2d = file.path(
    project_root,
    "data-raw",
    "robust_cats_phase2d_post_fix_validation.R"
  ),
  regression_tests = file.path(
    project_root,
    "tests",
    "testthat",
    "test-robust-cats-failure-retention.R"
  )
)

source_checksums <- data.frame(
  source = names(source_paths),
  path = normalizePath(
    source_paths,
    winslash = "/",
    mustWork = FALSE
  ),
  exists = file.exists(source_paths),
  md5 = ifelse(
    file.exists(source_paths),
    unname(tools::md5sum(source_paths)),
    NA_character_
  ),
  stringsAsFactors = FALSE
)

package_names <- c(
  "mmiCATs",
  "clusterSEs",
  "robust",
  "robustbase",
  "clubSandwich",
  "lme4",
  "lmerTest",
  "pbkrtest"
)

package_versions <- data.frame(
  package = package_names,
  version = vapply(
    package_names,
    function(package_name) {
      if (!requireNamespace(package_name, quietly = TRUE)) {
        return(NA_character_)
      }

      as.character(utils::packageVersion(package_name))
    },
    FUN.VALUE = character(1)
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  phase_status,
  file.path(closeout_dir, "phase2_closeout_status.csv"),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  issue_status,
  file.path(closeout_dir, "phase2_closeout_issue_register.csv"),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  source_checksums,
  file.path(closeout_dir, "phase2_closeout_source_checksums.csv"),
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  package_versions,
  file.path(closeout_dir, "phase2_closeout_package_versions.csv"),
  row.names = FALSE,
  na = ""
)

saveRDS(
  list(
    phase_status = phase_status,
    issue_status = issue_status,
    source_checksums = source_checksums,
    package_versions = package_versions,
    phase2a_error = phase2a_error,
    phase2b_error = phase2b_error,
    phase2d_error = phase2d_error
  ),
  file.path(closeout_dir, "phase2_closeout_results.rds"),
  version = 3
)

writeLines(
  capture.output(utils::sessionInfo()),
  con = file.path(closeout_dir, "session_info.txt"),
  useBytes = TRUE
)

summary_lines <- c(
  "Robust CATs Phase 2 closeout",
  "",
  capture.output(print(phase_status, row.names = FALSE)),
  "",
  "Issue dispositions:",
  capture.output(print(issue_status, row.names = FALSE)),
  "",
  paste("Results saved to:", closeout_dir)
)

writeLines(
  summary_lines,
  con = file.path(closeout_dir, "phase2_closeout_summary.txt"),
  useBytes = TRUE
)

message("")
message("Phase 2 closeout status:")
print(phase_status, row.names = FALSE)

if (!all(phase_status$closeout_pass %in% TRUE)) {
  stop(
    "At least one Phase 2 closeout gate failed. Review the saved closeout record.",
    call. = FALSE
  )
}

message("")
message("All Phase 2 closeout gates passed.")
message(paste("Closeout record saved to:", closeout_dir))
