# Study 3 empirical illustration: Phase 6D corrected comparative analysis
# Thin wrapper: substantive fitting/diagnostic logic lives under R/.
# Writes to a NEW directory and never overwrites the original Study 3 results.

library(devtools)
load_all()

project_root <- if (file.exists("DESCRIPTION")) {
  "."
} else if (file.exists(file.path("..", "DESCRIPTION"))) {
  ".."
} else {
  stop("Run this wrapper from the mmiCATs project root or data-raw.", call. = FALSE)
}

project_root <- normalizePath(
  project_root,
  winslash = "/",
  mustWork = TRUE
)

run_study3_empirical(
  project_root = project_root,
  output_dir = file.path(
    project_root,
    "data-raw",
    "study3-results",
    "definitive-study3-phase6d-corrected"
  ),
  verify_original_sources = FALSE
)
