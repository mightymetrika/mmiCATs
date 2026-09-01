# Phase 6D post-audit Study 3 plot-hygiene validation
#
# This does not fit models and does not touch any Study 3 result directory.

library(devtools)
load_all()

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

helper_path <- file.path(
  project_root,
  "R",
  "study3_empirical_helpers.R"
)

text <- paste(
  readLines(
    helper_path,
    warn = FALSE
  ),
  collapse = "\n"
)

checks <- data.frame(
  check = c(
    "package_study3_helper_has_no_height_zero",
    "package_study3_helper_uses_width_zero",
    "package_study3_helper_keeps_orientation_y"
  ),
  passed = c(
    !grepl(
      "height = 0",
      text,
      fixed = TRUE
    ),
    grepl(
      "width = 0",
      text,
      fixed = TRUE
    ),
    grepl(
      'orientation = "y"',
      text,
      fixed = TRUE
    )
  ),
  stringsAsFactors = FALSE
)

message(
  "Phase 6D post-audit Study 3 plot-hygiene checks:"
)
print(
  checks,
  row.names = FALSE
)

if (!all(checks$passed)) {
  stop(
    paste(
      sum(!checks$passed),
      "plot-hygiene check(s) failed."
    ),
    call. = FALSE
  )
}

message("")
message(
  "Running focused Study 3 plot-hygiene test..."
)

testthat::test_file(
  file.path(
    project_root,
    "tests",
    "testthat",
    "test-study3-plot-hygiene.R"
  ),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

message("")
message(
  paste(
    "All post-audit Study 3 plot-hygiene checks passed.",
    "No model or result directory was modified."
  )
)
