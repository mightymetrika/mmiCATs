# Phase 6D-B2B thin-wrapper architecture validation
# Static/test validation only. This script never sources the real runner wrappers.

library(devtools)
load_all()

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

message("Phase 6D-B2B: architecture tests...")
testthat::test_file(
  file.path(project_root, "tests", "testthat",
            "test-manuscript-runner-architecture.R"),
  reporter = "progress", stop_on_failure = TRUE, stop_on_warning = FALSE
)

message("Phase 6D-B2B: definitive Study 1/2 package tests...")
testthat::test_file(
  file.path(project_root, "tests", "testthat",
            "test-definitive-study-runners.R"),
  reporter = "progress", stop_on_failure = TRUE, stop_on_warning = FALSE
)

message("Phase 6D-B2B: deterministic sharding tests...")
testthat::test_file(
  file.path(project_root, "tests", "testthat",
            "test-definitive-sharding-runner.R"),
  reporter = "progress", stop_on_failure = TRUE, stop_on_warning = FALSE
)

message("Phase 6D-B2B: Study 3 package tests...")
testthat::test_file(
  file.path(project_root, "tests", "testthat",
            "test-study3-empirical.R"),
  reporter = "progress", stop_on_failure = TRUE, stop_on_warning = FALSE
)

paths <- c(
  study1 = file.path(project_root, "data-raw", "study1_final_simulation.R"),
  study2 = file.path(project_root, "data-raw", "study2_final_simulation.R"),
  study3_prepare = file.path(project_root, "data-raw", "study3_prepare_pre_results_freeze.R"),
  study3_run = file.path(project_root, "data-raw", "study3_definitive_analysis.R")
)
x <- lapply(paths, readLines, warn = FALSE)

checks <- data.frame(
  check = c(
    "study1_wrapper_short",
    "study2_wrapper_short",
    "study3_prepare_wrapper_short",
    "study3_runner_wrapper_short",
    "study1_calls_package_runner",
    "study2_calls_package_runner",
    "study3_prepare_calls_package_function",
    "study3_runner_calls_package_function",
    "study3_uses_new_corrected_directory",
    "all_four_entry_points_exported"
  ),
  passed = c(
    length(x$study1) <= 25L,
    length(x$study2) <= 25L,
    length(x$study3_prepare) <= 25L,
    length(x$study3_run) <= 35L,
    any(grepl("run_study1_definitive(", x$study1, fixed = TRUE)),
    any(grepl("run_study2_definitive(", x$study2, fixed = TRUE)),
    any(grepl("prepare_study3_empirical(", x$study3_prepare, fixed = TRUE)),
    any(grepl("run_study3_empirical(", x$study3_run, fixed = TRUE)),
    any(grepl("definitive-study3-phase6d-corrected", x$study3_run, fixed = TRUE)),
    all(c("run_study1_definitive", "run_study2_definitive",
          "prepare_study3_empirical", "run_study3_empirical") %in%
          getNamespaceExports("mmiCATs"))
  ),
  stringsAsFactors = FALSE
)

message("")
message("Phase 6D-B2B architecture checks:")
print(checks, row.names = FALSE)

if (!all(checks$passed)) {
  stop(paste(sum(!checks$passed), "Phase 6D-B2B check(s) failed."),
       call. = FALSE)
}

message("")
message("All Phase 6D-B2B architecture checks passed. No manuscript runner was executed.")
