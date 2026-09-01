# Phase 6D-B1 CRAN-facing namespace/NSE hygiene validation
#
# This validator checks only the package-hygiene corrections made after the
# already-passed Phase 6D-B1 migration/equivalence gate. It does not alter or
# regenerate Study 3 results.

library(devtools)
load_all()

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

checks <- list()

add_check <- function(check,
                      passed,
                      details = NA_character_) {
  checks[[length(checks) + 1L]] <<-
    data.frame(
      check = check,
      passed = isTRUE(passed),
      details = as.character(details),
      stringsAsFactors = FALSE
    )
}

read_r <- function(name) {
  paste(
    readLines(
      file.path(
        project_root,
        "R",
        name
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

sharding <- read_r(
  "definitive_sharding_helpers.R"
)

study3_helpers <- read_r(
  "study3_empirical_helpers.R"
)

study3_runner <- read_r(
  "study3_empirical.R"
)

package_r <- paste(
  sharding,
  study3_helpers,
  study3_runner,
  sep = "\n"
)

add_check(
  "no_self_namespace_colon3_calls",
  !grepl(
    "mmiCATs:::",
    package_r,
    fixed = TRUE
  )
)

add_check(
  "no_self_namespace_colon2_calls",
  !grepl(
    "mmiCATs::",
    package_r,
    fixed = TRUE
  )
)

add_check(
  "tail_is_explicitly_utils_qualified",
  grepl(
    "utils::tail(",
    sharding,
    fixed = TRUE
  ) &&
    !grepl(
      "(^|[^:[:alnum:]_.])tail[(]",
      sharding,
      perl = TRUE
    )
)

add_check(
  "capture_output_is_explicitly_utils_qualified",
  grepl(
    "utils::capture.output(",
    study3_runner,
    fixed = TRUE
  ) &&
    !grepl(
      "(^|[^:[:alnum:]_.])capture[.]output[(]",
      study3_runner,
      perl = TRUE
    )
)

add_check(
  "setNames_is_explicitly_stats_qualified",
  grepl(
    "stats::setNames(",
    study3_helpers,
    fixed = TRUE
  ) &&
    !grepl(
      "(^|[^:[:alnum:]_.])setNames[(]",
      study3_helpers,
      perl = TRUE
    )
)

add_check(
  "study3_dataset_plot_column_declared",
  grepl(
    'utils::globalVariables("dataset")',
    study3_helpers,
    fixed = TRUE
  )
)

message(
  "Phase 6D-B1 CRAN hygiene: rerunning migrated sharding tests..."
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

message(
  "Phase 6D-B1 CRAN hygiene: rerunning Study 3 package tests..."
)

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-study3-empirical.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

checks_df <- do.call(
  rbind,
  checks
)

rownames(checks_df) <- NULL

message("")
message(
  "Phase 6D-B1 CRAN-hygiene checks:"
)

print(
  checks_df,
  row.names = FALSE
)

if (!all(checks_df$passed)) {
  stop(
    paste(
      sum(!checks_df$passed),
      "Phase 6D-B1 CRAN-hygiene check(s) failed."
    ),
    call. = FALSE
  )
}

message("")
message(
  paste(
    "All Phase 6D-B1 CRAN-hygiene checks passed.",
    "Run devtools::check() and require 0 errors / 0 warnings / 0 notes."
  )
)
