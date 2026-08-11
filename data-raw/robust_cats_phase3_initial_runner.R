# Robust CATs audit: Phase 3 initial adversarial runner
#
# Runs Phase 3A (A-05) and Phase 3B (A-08), then writes a small combined
# status record. No production code is modified.

library(devtools)

load_all()

source(
  "data-raw/robust_cats_audit_helpers.R"
)

message("")
message("Phase 3 initial audit: running Phase 3A (A-05)...")

phase3a_error <- tryCatch(
  {
    source(
      "data-raw/robust_cats_phase3a_template_failure_audit.R"
    )
    NULL
  },
  error = function(e) e
)

if (!is.null(phase3a_error)) {
  stop(
    paste(
      "Phase 3A stopped:",
      conditionMessage(phase3a_error)
    ),
    call. = FALSE
  )
}

message("")
message("Phase 3 initial audit: running Phase 3B (A-08)...")

phase3b_error <- tryCatch(
  {
    source(
      "data-raw/robust_cats_phase3b_row_alignment_audit.R"
    )
    NULL
  },
  error = function(e) e
)

if (!is.null(phase3b_error)) {
  stop(
    paste(
      "Phase 3B stopped:",
      conditionMessage(phase3b_error)
    ),
    call. = FALSE
  )
}

project_root <- rca_find_project_root()

phase3a_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase3a-template-failure"
)

phase3b_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase3b-row-alignment"
)

phase3a_issue <- utils::read.csv(
  file.path(
    phase3a_dir,
    "phase3a_issue_summary.csv"
  ),
  stringsAsFactors = FALSE
)

phase3b_issue <- utils::read.csv(
  file.path(
    phase3b_dir,
    "phase3b_issue_summary.csv"
  ),
  stringsAsFactors = FALSE
)

status <- data.frame(
  phase = c(
    "3A",
    "3B"
  ),
  issue_id = c(
    "A-05",
    "A-08"
  ),
  reproduced = c(
    isTRUE(
      phase3a_issue$
        structurally_reproduced[1L]
    ),
    isTRUE(
      phase3b_issue$
        reproduced[1L]
    )
  ),
  production_code_changed =
    FALSE,
  stringsAsFactors = FALSE
)

output_dir <- file.path(
  project_root,
  "data-raw",
  "robust-cats-audit-results",
  "phase3-initial"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

rca_write_csv_atomic(
  status,
  file.path(
    output_dir,
    "phase3_initial_status.csv"
  )
)

source_files <- c(
  phase3a =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_phase3a_template_failure_audit.R"
    ),
  phase3b =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_phase3b_row_alignment_audit.R"
    ),
  runner =
    file.path(
      project_root,
      "data-raw",
      "robust_cats_phase3_initial_runner.R"
    ),
  study1_helpers =
    file.path(
      project_root,
      "R",
      "pwr_func_study1_helpers.R"
    ),
  helpers_cimrob =
    file.path(
      project_root,
      "R",
      "helpers_cimrob.R"
    )
)

source_checksums <- rca_source_checksums(
  source_files
)

rca_write_csv_atomic(
  source_checksums,
  file.path(
    output_dir,
    "phase3_initial_source_checksums.csv"
  )
)

results <- list(
  status = status,
  phase3a_issue = phase3a_issue,
  phase3b_issue = phase3b_issue,
  source_checksums = source_checksums
)

rca_save_rds_atomic(
  results,
  file.path(
    output_dir,
    "phase3_initial_results.rds"
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

message("")
message("Phase 3 initial adversarial status:")
print(
  status,
  row.names = FALSE
)

message("")
message(
  "Phase 3A and 3B completed without modifying production code."
)

message(paste(
  "Combined record saved to:",
  output_dir
))
