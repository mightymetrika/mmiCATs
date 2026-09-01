# Phase 6D-B2A CRAN read.dcf correction validation
#
# B2A migration/equivalence has already passed. This validator checks only the
# namespace correction required by R CMD check.

library(devtools)
load_all()

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

files <- c(
  file.path(
    project_root,
    "R",
    "definitive_study1.R"
  ),
  file.path(
    project_root,
    "R",
    "definitive_study2.R"
  )
)

text <- paste(
  vapply(
    files,
    function(path) {
      paste(
        readLines(
          path,
          warn = FALSE
        ),
        collapse = "\n"
      )
    },
    character(1)
  ),
  collapse = "\n"
)

checks <- data.frame(
  check = c(
    "no_utils_read_dcf_remains",
    "base_read_dcf_present_twice",
    "definitive_runner_tests_pass",
    "deterministic_sharding_tests_pass"
  ),
  passed = FALSE,
  details = NA_character_,
  stringsAsFactors = FALSE
)

checks$passed[
  checks$check ==
    "no_utils_read_dcf_remains"
] <- !grepl(
  "utils::read.dcf(",
  text,
  fixed = TRUE
)

matches <- gregexpr(
  "base::read.dcf(",
  text,
  fixed = TRUE
)[[1L]]

base_count <- if (
  length(matches) == 1L &&
    matches[1L] == -1L
) {
  0L
} else {
  length(matches)
}

checks$passed[
  checks$check ==
    "base_read_dcf_present_twice"
] <- identical(
  base_count,
  2L
)

message(
  "Phase 6D-B2A CRAN fix: rerunning definitive-runner tests..."
)

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-definitive-study-runners.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

checks$passed[
  checks$check ==
    "definitive_runner_tests_pass"
] <- TRUE

message(
  "Phase 6D-B2A CRAN fix: rerunning deterministic sharding tests..."
)

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-definitive-sharding-runner.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

checks$passed[
  checks$check ==
    "deterministic_sharding_tests_pass"
] <- TRUE

message("")
message(
  "Phase 6D-B2A CRAN read.dcf checks:"
)

print(
  checks,
  row.names = FALSE
)

if (!all(checks$passed)) {
  stop(
    paste(
      sum(!checks$passed),
      "Phase 6D-B2A CRAN read.dcf check(s) failed."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 6D-B2A CRAN read.dcf checks passed.",
    "Run devtools::check() and require 0/0/0."
  )
)
