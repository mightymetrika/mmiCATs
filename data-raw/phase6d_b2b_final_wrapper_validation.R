# Phase 6D-B2B final thin-wrapper validation
#
# Static only: this file DOES NOT source any manuscript execution wrapper.

library(devtools)
load_all()

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

paths <- c(
  study1 = file.path(
    project_root,
    "data-raw",
    "study1_final_simulation.R"
  ),
  study2 = file.path(
    project_root,
    "data-raw",
    "study2_final_simulation.R"
  ),
  study3_prepare = file.path(
    project_root,
    "data-raw",
    "study3_prepare_pre_results_freeze.R"
  ),
  study3_run = file.path(
    project_root,
    "data-raw",
    "study3_definitive_analysis.R"
  )
)

if (!all(file.exists(paths))) {
  stop(
    "One or more manuscript wrapper files are missing.",
    call. = FALSE
  )
}

read_wrapper <- function(path) {
  paste(
    readLines(
      path,
      warn = FALSE
    ),
    collapse = "\n"
  )
}

text <- lapply(
  paths,
  read_wrapper
)

parsed <- lapply(
  paths,
  function(path) {
    parse(
      file = path,
      keep.source = FALSE
    )
  }
)

find_call <- function(expressions,
                      function_name) {
  hits <- Filter(
    function(x) {
      is.call(x) &&
        identical(
          as.character(x[[1L]]),
          function_name
        )
    },
    as.list(expressions)
  )

  if (length(hits) != 1L) {
    stop(
      paste(
        "Expected exactly one top-level",
        function_name,
        "call."
      ),
      call. = FALSE
    )
  }

  hits[[1L]]
}

s1_call <- find_call(
  parsed$study1,
  "run_study1_definitive"
)

s2_call <- find_call(
  parsed$study2,
  "run_study2_definitive"
)

s3_prepare_call <- find_call(
  parsed$study3_prepare,
  "prepare_study3_empirical"
)

s3_run_call <- find_call(
  parsed$study3_run,
  "run_study3_empirical"
)

s1_args <- names(
  as.list(s1_call)[-1L]
)
s2_args <- names(
  as.list(s2_call)[-1L]
)
s3_prepare_args <- names(
  as.list(s3_prepare_call)[-1L]
)
s3_run_args <- names(
  as.list(s3_run_call)[-1L]
)

checks <- data.frame(
  check = c(
    "study1_wrapper_calls_frozen_default_runner",
    "study2_wrapper_calls_frozen_default_runner",
    "study3_prepare_uses_valid_default_signature",
    "study3_prepare_does_not_use_output_dir_argument",
    "study3_runner_sets_project_root",
    "study3_runner_sets_output_dir",
    "study3_runner_disables_original_source_match",
    "study3_runner_uses_new_corrected_directory",
    "study3_runner_does_not_name_original_result_directory",
    "all_four_package_entry_points_exported"
  ),
  passed = c(
    length(s1_args) == 0L,
    length(s2_args) == 0L,
    length(s3_prepare_args) == 0L,
    !("output_dir" %in% s3_prepare_args),
    "project_root" %in% s3_run_args,
    "output_dir" %in% s3_run_args,
    "verify_original_sources" %in% s3_run_args,
    grepl(
      "definitive-study3-phase6d-corrected",
      text$study3_run,
      fixed = TRUE
    ),
    !grepl(
      '"definitive-study3"',
      text$study3_run,
      fixed = TRUE
    ),
    all(
      c(
        "run_study1_definitive",
        "run_study2_definitive",
        "prepare_study3_empirical",
        "run_study3_empirical"
      ) %in%
        getNamespaceExports(
          "mmiCATs"
        )
    )
  ),
  stringsAsFactors = FALSE
)

# Check the literal value FALSE, not merely presence of the argument.
verify_expr <- s3_run_call[["verify_original_sources"]]
checks$passed[
  checks$check ==
    "study3_runner_disables_original_source_match"
] <- identical(
  verify_expr,
  FALSE
)

message(
  "Phase 6D-B2B final wrapper checks:"
)

print(
  checks,
  row.names = FALSE
)

if (!all(checks$passed)) {
  stop(
    paste(
      sum(!checks$passed),
      "final B2B wrapper check(s) failed."
    ),
    call. = FALSE
  )
}

message("")
message(
  "Rerunning CRAN-safe manuscript architecture package test..."
)

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-manuscript-runner-architecture.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

message("")
message(
  "All final Phase 6D-B2B wrapper checks passed. No manuscript runner was executed."
)
